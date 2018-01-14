\ Icoboard support code

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

\ ----------------------------------------------------------------------
\ Text mode video display

\ Overwrites a section of video memory with a given value.
: vfill  ( v-addr u x -- )
  VWA io@ >r    \ save cursor position
  >r          \ stash cell to write
  swap VWA io!  \ set up write address
  begin
    dup
  while ( count ) ( R: vwa x )
    1- r@ VWD io!
  repeat
  rdrop drop
  r> VWA io!  \ restore old cursor
  ;

variable vcols  \ columns in the text display
variable vrows  \ rows in the text display
variable vatt   \ attributes for text in top 8 bits

: vsize vcols @ vrows @ u* ;

\ Sets the current color to the given fore and back color indices.
: vcolor!  ( back fore -- )
  4 lshift or 8 lshift  vatt ! ;

\ Fills a section of text RAM with spaces in the current color.
: vclr  ( v-addr u -- )
  bl vatt @ or vfill ;

\ Clears the screen, filling it with spaces in the current color, and resets
\ the cursor to the lower-left corner. As a side effect, this resets the text
\ window scroll to the start of video memory.
: vpage
  0  \ start of memory
  vsize  \ size of a screen
  vclr     \ clear a screen-sized area of text RAM
  0 VC0 io!   \ make it the active screen
  vsize vcols @ - VWA io!   \ cursor to lower left
  ;

\ Scrolls the display up, revealing a blank line at the bottom. Leaves the
\ cursor address unchanged (i.e. it moves up on the display).
: vscroll
  VC0 io@  vcols @ +  VC0 io!
  VC0 io@  vsize vcols @ - + $7FF and  vcols @  vclr
  ;

\ "Types" a character without control character interpretation. Advances the
\ cursor. Scrolls the display as needed.
: vputc ( c -- )
  vatt @ or VWD io!   \ store the character with attributes
  VWA io@  VC0 io@ -  $7FF and    \ get distance from start of display
  vsize = if   \ if we've run off the end
    vscroll  \ reveal another line
  then ;

\ "Types" a character with control character interpretation, simulating a
\ terminal.
: vemit ( c -- )
  7 over = if drop exit then   \ ignore BEL

  8 over = if drop  \ backspace
    VC0 io@ VWA io@ xor if  VWA io@ 1- $7FF and VWA io! then
    exit
  then

  10 over = if drop \ line feed
    vscroll
    VWA io@  vcols @ + $7FF and  VWA io!
    exit
  then

  12 over = if drop \ form feed / page
    vpage exit
  then

  13 over = if drop \ carriage return
    \ TODO broken broken ?
    \ Compute current offset into the display
    VWA io@  VC0 io@  -  $7FF and
    \ Round down by division and multiplication
    vcols @ u/  vcols @ u*
    \ Project back into the display and assign
    VC0 io@ + $7FF and VWA io!
    exit
  then

  vputc ;


: vid
  103 VTH io!
  207 VTH 4 + io!
  639 VTH 6 + io!
  100 VTV io!
  121 VTV 4 + io!
  399 VTV 6 + io!
  80 vcols !
  25 vrows !
  0 15 vcolor!
  vpage
  ;
( ----------------------------------------------------------- )
( SD Card )


variable sdcyc  50 sdcyc !
: sddelay sdcyc @  ?dup if cycles then ;

2 outpin >sdclk
3 outpin >sdmosi
4 outpin >sdcs_

: sdx1  ( bits -- bits' )
  $8000 over and >sdmosi
  1 lshift

  sddelay  1 >sdclk
  sddelay

  IN io@ 2 and 1 rshift  or
  
  0 >sdclk ;

: sdx  ( tx -- rx )
  8 lshift
  sdx1 sdx1 sdx1 sdx1
  sdx1 sdx1 sdx1 sdx1 ;

: sdidle $FF sdx drop ;

: sdr1
  $FF sdx
  dup $FF = if
    drop sdr1 exit
  then
  sdidle ;

: sdcmd  ( arglo arghi cmd -- )
  $40 or sdx drop         \ start bit + cmd
  dup 8 rshift sdx drop   \ arg[31:24]
  sdx drop                \ arg[23:16]
  dup 8 rshift sdx drop   \ arg[15:8]
  sdx drop                \ arg[7:0]
  $95 sdx drop ;          \ checksum, hardcoded for CMD0

: sdacmd  ( arglo arghi cmd -- )
  0 0 55 sdcmd sdr1 drop
  sdcmd ;

: sdcmd0
  0 0 0 sdcmd
  sdr1 ;

: sdacmd41
  0 0 41 sdacmd sdr1 ;

: sdinit
  \ Use slow clock.
  50 sdcyc !
  \ Raise MOSI and CS
  1 >sdmosi   1 >sdcs_
  \ Send 9 bytes with MOSI high (=81 edges, > required 74)
  9 begin
    dup
  while
    1-
    $FF sdx drop
  repeat drop

  0 >sdcs_   \ select card
  \ Send CMD0
  sdcmd0
  \ Require a $01 response
  1 <> 1 and throw
  \ Send CMD1 until we get a 0 back
  begin
    sdacmd41 0=
  until
  0 sdcyc !   \ high speed
  ;

: sdcrc16  ( c-addr u -- crc )
  0   \ initial CRC seed
  [:
    \ Swap bytes
    dup 8 lshift swap 8 rshift or
    xor
    $FF over and 4 rshift xor
    dup 12 lshift xor
    $FF over and 5 lshift xor
  ;] sfoldl ;

: sdrd ( dest seclo sechi -- )
  17 sdcmd sdr1 throw
  begin $FF sdx $FF xor until

  dup >r    \ stash the buffer address
  512 bounds begin
    over over xor
  while
    $FF sdx over c!
    1+
  repeat 2drop
  $FF sdx 8 lshift $FF sdx or   \ read the CRC16
  r> 512 sdcrc16
  xor throw
  sdidle ;

: sdwr ( src seclo sechi -- )
  24 sdcmd sdr1 throw
  sdidle

  $FE sdx drop
  dup >r
  512 bounds begin
    over over xor
  while
    dup c@ sdx drop
    1+
  repeat 2drop
  r> 512 sdcrc16
  dup 8 rshift sdx drop sdx drop

  begin
    $FF sdx
    dup $FF =
  while
    drop
  repeat \ leaving response code on stack
  $1F and

  begin $FF sdx $FF = until

  sdidle

  5 <> throw ;

\ -------------------------------------------------------------------
\ Block support

\ We place a single 1kiB block buffer at the top of SRAM.
0 1024 - constant blkbuf

\ If bit 0 of blkstat is set, the buffer is allocated to a disk block. If bit 1
\ of blkstat is also set, the buffer is dirty.
variable blkstat

\ If the buffer is allocated, blkall stores the block number.
variable blkall

\ Convert a block number to an LBA.
: blk>sec  ( u -- u )  1- 2* ;

\ Marks the contents of the current buffer as dirty. If the current buffer is
\ not allocated, this is a no-op.
: update  blkstat @  2 or  blkstat ! ;

\ Flush modified buffers to disk and unassign.
: flush
  \ If the buffer is both assigned and dirty,
  blkstat @ 3 and 3 = if
    blkall @ blk>sec >r
    blkbuf        r@    9 lshift  r@    7 rshift  sdwr
    blkbuf 512 +  r@ 1+ 9 lshift  r> 1+ 7 rshift  sdwr
  then
  \ Unassign.
  0 blkstat ! ;

\ Arrange for a block to be assigned to a buffer, and return its address.
: block  ( u -- addr )
  \ If the requested block is already loaded, just return its address.
  blkall @ over =  blkstat @ 1 and  and if  drop blkbuf exit  then

  \ This is written in an attempt to keep I/O exceptions from corrupting state.
  flush
  dup blk>sec >r
  blkbuf        r@    9 lshift  r@    7 rshift  sdrd
  blkbuf 512 +  r@ 1+ 9 lshift  r> 1+ 7 rshift  sdrd
  blkall !
  1 blkstat !
  blkbuf ;

\ -------------------------------------------------------------------
\ Block load and dev support.

variable blk
  \ Holds the block index currently being used as source, or 0 if
  \ source is coming from somewhere else.

\ Interprets source code in block u.
: load  ( i*x u -- j*x )
  \ Save source state.
  blk @ >r   SOURCE >r >r   >IN @ >r
  \ Set up input to read from the block.
  dup blk !
  1024 swap block 'SOURCE 2!  0 >IN !
  \ Try to interpret.
  ['] interpret catch
  \ Whether that succeeded or failed, restore the old input spec.
  r> >IN !  r> r> swap 'SOURCE 2!  r> dup blk !  ( except old-blk )
  \ If we were loading from a block before LOAD, the address is likely
  \ wrong. Update it.
  ?dup if  block 'SOURCE !  then
  throw ;

\ Loads a sequence of blocks
: thru  ( i*x u1 u2 -- j*x )
  begin
    >r dup >r
    load
    r> r>
    over over xor
  while
    swap 1+ swap
  repeat
  2drop ;

\ Lists source code in a block using the conventional 64x16 format.
: list  ( u -- )
  block 1024 bounds
  begin
    over over xor
  while
    dup 64 cr type
    64 +
  repeat
  2drop ;

\ -------------------------------------------------------------------
\ PS/2 keyboard GPIO interface

variable kbdbuf
variable kbd#bit

: kbdisr
  3 #bit  IN io@ and  12 lshift     \ get data value in bit 15
  kbdbuf @  1 rshift or  kbdbuf ! \ insert it into shift register
  kbd#bit @ 1- kbd#bit !d        \ decrement bit counter
  0= if   \ we're done
    irq#negedge irq-off           \ disable this IRQ
    12 #bit OUTSET io!               \ pull clock low
  else
    IN io!d                         \ acknowledge IRQ
  then ;

: kbd@
  11 kbd#bit !                    \ expecting 11 bits
  12 #bit OUTCLR io!                 \ release clock line
  IN io!d                           \ clear pending negedge IRQ
  irq#negedge irq-on              \ enable IRQ
  begin kbd#bit @ 0= until        \ wait for all bits
  kbdbuf @ 6 rshift $FF and       \ extract bits
  ;

: kbdinit
  12 #bit OUTSET io!                 \ pull clock low
  ;

: kbdscan
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
  then
  ;

create kbdlt
0 c,    0 c,      \ 0: unused
0 c,    0 c,      \ F9
0 c,    0 c,      \ 2: unused
0 c,    0 c,      \ F5
0 c,    0 c,      \ F3
0 c,    0 c,      \ F1
0 c,    0 c,      \ F2
0 c,    0 c,      \ F12
0 c,    0 c,      \ 8: unused
0 c,    0 c,      \ F10
0 c,    0 c,      \ F8
0 c,    0 c,      \ F6
0 c,    0 c,      \ F4
9 c,    9 c,      \ TAB
'`' c,  '~' c,
0 c,    0 c,      \ 0F: unused
0 c,    0 c,      \ 10: unused
0 c,    0 c,      \ L ALT
0 c,    0 c,      \ L SHIFT
0 c,    0 c,      \ 13: unused
0 c,    0 c,      \ L CTRL
'q' c,  'Q' c,
'1' c,  '!' c,
0 c,    0 c,      \ 17: unused
0 c,    0 c,      \ 18: unused
0 c,    0 c,      \ 19: unused
'z' c,  'Z' c,
's' c,  'S' c,
'a' c,  'A' c,
'w' c,  'W' c,
'2' c,  '@' c,
0 c,    0 c,      \ 1F: unused
0 c,    0 c,      \ 20: unused
'c' c,  'C' c,
'x' c,  'X' c,
'd' c,  'D' c,
'e' c,  'E' c,
'4' c,  '$' c,
'3' c,  '#' c,
0 c,    0 c,      \ 27: unused
0 c,    0 c,      \ 28: unused
bl c,   bl c,
'v' c,  'V' c,
'f' c,  'F' c,
't' c,  'T' c,
'r' c,  'R' c,
'5' c,  '%' c,
0 c,    0 c,      \ 2F: unused
0 c,    0 c,      \ 30: unused
'n' c,  'N' c,
'b' c,  'B' c,
'h' c,  'H' c,
'g' c,  'G' c,
'y' c,  'Y' c,
'6' c,  '^' c,
0 c,    0 c,      \ 37: unused
0 c,    0 c,      \ 38: unused
0 c,    0 c,      \ 39: unused
'm' c,  'M' c,
'j' c,  'J' c,
'u' c,  'U' c,
'7' c,  '&' c,
'8' c,  '*' c,
0 c,    0 c,      \ 3F: unused
0 c,    0 c,      \ 40: unused
',' c,  '<' c,
'k' c,  'K' c,
'i' c,  'I' c,
'o' c,  'O' c,
'0' c,  ')' c,
'9' c,  '(' c,
0 c,    0 c,      \ 47: unused
0 c,    0 c,      \ 48: unused
'.' c,  '>' c,
'/' c,  '?' c,
'l' c,  'L' c,
';' c,  ':' c,
'p' c,  'P' c,
'-' c,  '_' c,
0 c,    0 c,      \ 4F unused
0 c,    0 c,      \ 50 unused
0 c,    0 c,      \ 51 unused
''' c,  '"' c,
0 c,    0 c,      \ 53 unused
'[' c,  '{' c,
'=' c,  '+' c,
0 c,    0 c,      \ 56 unused
0 c,    0 c,      \ 57 unused
0 c,    0 c,      \ CAPS
0 c,    0 c,      \ R SHIFT
13 c,   13 c,     \ ENTER
']' c,  '}' c,
0 c,    0 c,      \ 5C unused
'\' c,  '|' c,
0 c,    0 c,      \ 5E unused
0 c,    0 c,      \ 5F unused
0 c,    0 c,      \ 60 unused
0 c,    0 c,      \ 61 unused
0 c,    0 c,      \ 62 unused
0 c,    0 c,      \ 63 unused
0 c,    0 c,      \ 64 unused
0 c,    0 c,      \ 65 unused
8 c,    8 c,      \ backspace

variable kbdmod
: kbdkey
  kbdscan
  $12 over =  over $59 = or if  \ shift make
    drop  kbdmod @ 1 or kbdmod !
    kbdkey exit
  then
  $8012 over =  over $8059 = or if  \ shift break
    drop kbdmod @ 1 invert and kbdmod !
    kbdkey exit
  then
  $14 over =  over $114 = or if \ ctrl make
    drop  kbdmod @ 2 or kbdmod !
    kbdkey exit
  then
  $8014 over =  over $8114 = or if  \ ctrl break
    drop kbdmod @ 2 invert and kbdmod !
    kbdkey exit
  then

  \ Having processed the modifiers, ignore any large values, including
  \ break codes.
  dup  $66 u> if  drop kbdkey exit  then

  \ We now have a value in-range for the lookup table.
  cells   \ convert to offset
  kbdmod @ 1 and +  \ mix in shift offset
  kbdlt + c@

  \ The result might be zero for unused/non-ASCII codes.
  dup 0= if drop kbdkey exit then

  \ Apply control.
  kbdmod @ 2 and if  $20 invert and  '@' -  then ;

( ----------------------------------------------------------- )
( Demo wiring below )

\ Vector table
' kbdisr vectors irq#negedge cells + !

:noname
  uart-rx-init
  312 UARTRD io! \ Set baud rate to 115200

  \ Take a best-effort crack at initializing the disk
  ['] sdinit catch drop

  IN io@  4 #bit and if   \ If S2 is held, boot with serial console
    ['] tx 'emit !
    ['] rx! 'key !
  else  \ otherwise, normal config
    vid
    ['] vemit 'emit !
    ['] kbdkey 'key !
  then
  ei ;
oncold !

blkbuf ramtop !

( install cold as the reset vector )
' cold  u2/  0 !

remarker empty

.( Compilation complete. HERE is... )
here host.
