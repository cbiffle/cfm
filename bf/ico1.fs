\ vim: syntax=forth:cc=64
\ Icoboard support code - basics
: ledtog  5 + #bit OUTTOG io! ;
: cycles ( u -- )   \ delays for at least u cycles
  >r
  TIMV io@
  begin   ( start )
    TIMV io@ over -   ( start delta )
    r@ u< 0= if
      rdrop drop exit
    then
  again ;
---
\ Text mode video display: fundamentals.
\ Overwrites a section of video memory with a given value.
: vfill  ( v-addr u x -- )
  VWA io@ >r  rot VWA io!
  swap 0 do
    VWD io!d
  loop drop
  r> VWA io!  ;
variable vcols    variable vrows
: vsize vcols @ vrows @ u* ;
variable vatt   \ attributes for text in top 8 bits
: vcolor!  ( back fore -- )
  4 lshift or 8 lshift  vatt ! ;
: vclr  ( v-addr u -- ) \ fills a section of VRAM with spaces
  bl vatt @ or vfill ;
: voff  VWA io@  VC0 io@ -  $7FF and ;
---
\ Clearing, scrolling, and placing characters.
: vpage
  0 vsize vclr    0 VC0 io!
  vsize vcols @ - VWA io!  ;
: vscroll
  VC0 io@  vcols @ +  VC0 io!
  VC0 io@  vsize vcols @ - + $7FF and  vcols @  vclr ;
: vputc ( c -- )
  vatt @ or VWD io!   \ store the character with attributes
  voff vsize = if   \ if we've run off the end
    vscroll  \ reveal another line
  then ;
---
\ "Typing" a character, simulating a terminal.
: VWA+! VWA io@ + $7FF and VWA io! ;
: vemit ( c -- )
  7 over = if drop exit then   \ ignore BEL
  8 over = if drop  \ backspace
    VC0 io@ VWA io@ xor if  -1 VWA+! then
    exit then
  10 over = if drop \ line feed
    vscroll   vcols @ VWA+!  exit then
  12 over = if drop \ form feed / page
    vpage exit then
  13 over = if drop \ carriage return
    voff  vcols @ u/  vcols @ u*   VC0 io@ + $7FF and VWA io!
    exit then
  vputc ;
---
\ Display initialization routine. Insets an 80x25 character
\ display in an 800x600 frame.
: vid
  103 VTH io!    207 VTH 4 + io!    639 VTH 6 + io!
  100 VTV io!    121 VTV 4 + io!    399 VTV 6 + io!
  80 vcols !  25 vrows !  0 15 vcolor!  vpage ;
---
\ SD card - low-level SPI protocol, delays
2 outpin >sdclk   3 outpin >sdmosi   4 outpin >sdcs_
variable sdcyc  50 sdcyc !
: sddelay sdcyc @  ?dup if cycles then ;
: sdx1  ( bits -- bits' )
  $8000 over and >sdmosi   1 lshift
  sddelay  1 >sdclk  sddelay
  IN io@ 2 and 1 rshift  or   0 >sdclk ;
: sdx  ( tx -- rx )
  8 lshift sdx1 sdx1 sdx1 sdx1 sdx1 sdx1 sdx1 sdx1 ;
: sdidle $FF sdx drop ;
---
\ SD card - SD protocol basics
: sdr1  ( -- resp )
  $FF sdx   dup $FF = if  drop sdr1 exit  then  sdidle ;
: sdcmd  ( arglo arghi cmd -- )
  $40 or sdx drop         \ start bit + cmd
  dup 8 rshift sdx drop sdx drop     \ arg[31:16]
  dup 8 rshift sdx drop sdx drop     \ arg[15:0]
  $95 sdx drop ;          \ checksum, hardcoded for CMD0
: sdacmd  ( arglo arghi cmd -- )
  0 0 55 sdcmd sdr1 drop sdcmd ;
: sdcmd0 0 0 0 sdcmd sdr1 ;
: sdacmd41 0 0 41 sdacmd sdr1 ;
---
\ SD card - initialization.
: sdinit
  50 sdcyc !  \ Use slow clock.
  1 >sdmosi   1 >sdcs_
  \ Send 9 bytes with MOSI high (=81 edges, > required 74)
  9 0 do $FF sdx drop loop
  0 >sdcs_   \ select card
  sdcmd0  1 <> 1 and throw
  begin sdacmd41 0= until
  0 sdcyc ! ; \ high speed
---
\ SD card - CRC16
: sdcrc16  ( c-addr u -- crc )
  0 rot rot   \ initial CRC seed
  [: \ Swap bytes
    swap dup 8 lshift swap 8 rshift or
    xor
    $FF over and 4 rshift xor
    dup 12 lshift xor
    $FF over and 5 lshift xor
  ;] s-each ;
---
\ SD card - sector read
: sdrd ( dest seclo sechi -- )
  17 sdcmd sdr1 throw
  begin $FF sdx $FF xor until
  dup >r    \ stash the buffer address
  512 bounds do
    $FF sdx i c!
  loop
  $FF sdx 8 lshift $FF sdx or   \ read the CRC16
  r> 512 sdcrc16 xor throw  sdidle ;
---
\ SD card - sector write
: sdwr ( src seclo sechi -- )
  24 sdcmd sdr1 throw   sdidle  $FE sdx drop
  dup 512 sdcrc16 >r
  512 bounds [: sdx drop ;] s-each
  r> dup 8 rshift sdx drop sdx drop
  begin $FF sdx dup $FF = while drop repeat
  $1F and   begin $FF sdx $FF = until
  sdidle   5 <> throw ;
---
\ Block support.
0 1024 - constant blkbuf
variable blkstat
  \ bit 0: allocated; bit 1: dirty
variable blkall
  \ block number if allocated
: blk>lba  ( u -- u )  1- 2* ;
: update  blkstat @  2 or  blkstat ! ;
: flush
  blkstat @ 3 and 3 = if \ assigned + dirty
    blkall @ blk>lba >r
    blkbuf        r@    9 lshift  r@    7 rshift  sdwr
    blkbuf 512 +  r@ 1+ 9 lshift  r> 1+ 7 rshift  sdwr
  then
  0 blkstat ! ; \ unassign
---
\ Block loading / access.
: block  ( u -- addr )
  \ If the requested block is already loaded, just return it.
  blkall @ over =  blkstat @ 1 and  and
      if  drop blkbuf exit  then
  \ This is written to stay exception-safe.
  flush   dup blk>lba >r
  blkbuf        r@    9 lshift  r@    7 rshift  sdrd
  blkbuf 512 +  r@ 1+ 9 lshift  r> 1+ 7 rshift  sdrd
  blkall !   1 blkstat !   blkbuf ;
---
\ Loading source code from blocks.
: load  ( i*x u -- j*x )
  \ Save source state.
  BLK @ >r   SOURCE >r >r   >IN @ >r
  \ Set up input to read from the block.
  dup BLK !   1024 swap block 'SOURCE 2!  0 >IN !
  ['] interpret catch
  \ Whether that succeeded or failed, restore the input spec.
  r> >IN !  r> r> swap 'SOURCE 2!  r> dup BLK ! ( ex old-blk )
  \ If we were loading from a block before LOAD, the address is
  \ likely wrong. Update it.
  ?dup if  block 'SOURCE !  then
  throw ;
---
\ Thru, List
: thru  ( i*x u1 u2 -- j*x )
  1+ swap do
    i load
  loop ;
: list  ( u -- )
  block 1024 bounds do
    cr  i 64 type
  64 +loop ;
---
\ PS/2 keyboard interface: init, receive ISR
variable kbdbuf
variable kbd#bit
: kbdinit  12 #bit OUTSET io! ;
: kbdisr
  3 #bit  IN io@ and  12 lshift   \ get data value in bit 15
  kbdbuf @  1 rshift or  kbdbuf ! \ shift it into buf
  kbd#bit @ 1- kbd#bit !d         \ decrement bit counter
  0= if   \ we're done
    irq#negedge irq-off           \ disable this IRQ
    12 #bit OUTSET io!            \ pull clock low
  else
    IN io!d                       \ acknowledge IRQ
  then ;
' kbdisr  vectors irq#negedge cells +  !
---
\ PS/2 keyboard interface: raw byte polling
: kbd@
  11 kbd#bit !                    \ expecting 11 bits
  12 #bit OUTCLR io!                 \ release clock line
  IN io!d                           \ clear pending negedge IRQ
  irq#negedge irq-on              \ enable IRQ
  begin kbd#bit @ 0= until        \ wait for all bits
  kbdbuf @ 6 rshift $FF and       \ extract bits
  ;
---
\ PS/2 keyboard interface: scan code polling.
: kbdscan   ( -- scancode )
  kbd@
  $E0 over = if   \ extended scan code set 0
    drop kbdscan  8 #bit or  exit
  then
  $E1 over = if   \ hello, pause
    drop
    kbdscan @ drop
    kbdscan @ drop
    $200 exit
  then
  $F0 over = if   \ break code
    drop kbdscan  15 #bit or  exit
  then ;
---
\ PS/2 keyboard interface: scan code to ASCII table
create kbdlt
(   x0             x1            x2            x3       )
000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, ( 00 )
000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, ( 04 )
000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, ( 08 )
000 c, 000 c,   9 c,   9 c, '`' c, '~' c, 000 c, 000 c, ( 0C )
000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, ( 10 )
000 c, 000 c, 'q' c, 'Q' c, '1' c, '!' c, 000 c, 000 c, ( 14 )
000 c, 000 c, 000 c, 000 c, 'z' c, 'Z' c, 's' c, 'S' c, ( 18 )
'a' c, 'A' c, 'w' c, 'W' c, '2' c, '@' c, 000 c, 000 c, ( 1C )
000 c, 000 c, 'c' c, 'C' c, 'x' c, 'X' c, 'd' c, 'D' c, ( 20 )
'e' c, 'E' c, '4' c, '$' c, '3' c, '#' c, 000 c, 000 c, ( 24 )
000 c, 000 c,  bl c,  bl c, 'v' c, 'V' c, 'f' c, 'F' c, ( 28 )
't' c, 'T' c, 'r' c, 'R' c, '5' c, '%' c, 000 c, 000 c, ( 2C )
---
\ PS/2 keyboard interface: scan code to ASCII table cont'd
(   x0             x1            x2            x3       )
000 c, 000 c, 'n' c, 'N' c, 'b' c, 'B' c, 'h' c, 'H' c, ( 30 )
'g' c, 'G' c, 'y' c, 'Y' c, '6' c, '^' c, 000 c, 000 c, ( 34 )
000 c, 000 c, 000 c, 000 c, 'm' c, 'M' c, 'j' c, 'J' c, ( 38 )
'u' c, 'U' c, '7' c, '&' c, '8' c, '*' c, 000 c, 000 c, ( 3C )
000 c, 000 c, ',' c, '<' c, 'k' c, 'K' c, 'i' c, 'I' c, ( 40 )
'o' c, 'O' c, '0' c, ')' c, '9' c, '(' c, 000 c, 000 c, ( 44 )
000 c, 000 c, '.' c, '>' c, '/' c, '?' c, 'l' c, 'L' c, ( 48 )
';' c, ':' c, 'p' c, 'P' c, '-' c, '_' c, 000 c, 000 c, ( 4C )
000 c, 000 c, 000 c, 000 c, ''' c, '"' c, 000 c, 000 c, ( 50 )
'[' c, '{' c, '=' c, '+' c, 000 c, 000 c, 000 c, 000 c, ( 54 )
000 c, 000 c, 000 c, 000 c,  13 c,  13 c, ']' c, '}' c, ( 58 )
000 c, 000 c, '\' c, '|' c, 000 c, 000 c, 000 c, 000 c, ( 5C )
000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 000 c, ( 60 )
000 c, 000 c, 000 c, 000 c,   8 c,   8 c,               ( 64 )
---
\ PS/2 keyboard interface: modifier handling, ASCII scanning
variable kbdmod \ bit 0 = shift, 1 = ctrl
: kbdkey
  kbdscan
  $12 over =  over $59 = or if  \ shift make
    drop  kbdmod @ 1 or kbdmod !  kbdkey exit then
  $8012 over =  over $8059 = or if  \ shift break
    drop kbdmod @ 1 invert and kbdmod !  kbdkey exit then
  $14 over =  over $114 = or if \ ctrl make
    drop  kbdmod @ 2 or kbdmod !  kbdkey exit then
  $8014 over =  over $8114 = or if  \ ctrl break
    drop kbdmod @ 2 invert and kbdmod !  kbdkey exit then
  dup  $66 u> if  drop kbdkey exit  then  \ table range
  cells kbdlt +  kbdmod @ 1 and +  c@  \ table lookup
  dup 0= if drop kbdkey exit then  \ ignore if zero
  kbdmod @ 2 and if  $20 invert and  '@' -  then ; \ ctrl
---
\ Cold start hook, final setup.
:noname
  uart-rx-init  312 UARTRD io! \ Set baud rate to 115200
  ['] sdinit catch drop
  IN io@  4 #bit and if   \  S2 held: boot with serial console
    ['] tx is emit   ['] rx! is key
  else  \ otherwise, normal config
    vid   ['] vemit is emit   ['] kbdkey is key
  then
  ei ;
is oncold
blkbuf ramtop !    ' cold  u2/  0 !
remarker empty
---
