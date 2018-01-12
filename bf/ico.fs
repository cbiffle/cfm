\ Icoboard support code

: #bit  1 swap lshift ;

( ----------------------------------------------------------- )
( Interrupt Controller )

$B000 constant IRQST  ( status / enable trigger )
\ $D802 constant IRQEN  ( enable )
$B004 constant IRQSE  ( set enable )
$B006 constant IRQCE  ( clear enable )

( Atomically enables interrupts and returns. This is intended to be tail )
( called from the end of an ISR. )
: ei  IRQST !d ;

: irq-off  ( u -- )  #bit IRQCE ! ;
: irq-on   ( u -- )  #bit IRQSE ! ;

13 constant irq#m1
14 constant irq#m0
15 constant irq#negedge

( ----------------------------------------------------------- )
( I/O ports )

\ $8000 constant outport      ( literal value)
$8002 constant OUTSET  ( 1s set pins, 0s do nothing)
$8004 constant OUTCLR  ( 1s clear pins, 0s do nothing)
$8006 constant OUTTOG  ( 1s toggle pins, 0s do nothing)

: outpin
  create #bit ,
  does> @ swap if OUTSET else OUTCLR then ! ;

$9000 constant IN

( ----------------------------------------------------------- )
( Timer )

$A000 constant TIMV
$A002 constant TIMF
$A004 constant TIMM0
$A006 constant TIMM1

( ----------------------------------------------------------- )
( UART receive queue and flow control )

8 constant uart-#rx
variable uart-rx-buf  uart-#rx 1- cells allot
variable uart-rx-hd
variable uart-rx-tl

1 outpin >CTS_N

: rxq-empty? uart-rx-hd @ uart-rx-tl @ = ;
: rxq-full? uart-rx-hd @ uart-rx-tl @ - uart-#rx = ;

( Inserts a cell into the receive queue. This is intended to be called from )
( interrupt context, so if it encounters a queue overrun, it simply drops )
( data. )
: >rxq
  rxq-full? if drop exit then

  uart-rx-buf  uart-rx-hd @ [ uart-#rx 1- 2* ] literal and +  !
  2 uart-rx-hd +! ;

( Takes a cell from the receive queue. If the queue is empty, spin. )
: rxq>
  begin rxq-empty? 0= until
  uart-rx-buf  uart-rx-tl @ [ uart-#rx 1- 2* ] literal and +  @
  2 uart-rx-tl +! ;

\ Receives a byte from RX. In the event of an error (e.g. framing) the sign bit
\ is set.
: rx  ( -- x )
  rxq>
  rxq-empty? if 0 >CTS_N then  \ allow sender to resume if we've emptied the queue.
  ;

( ----------------------------------------------------------- )
( Hard UART )

$D000 constant UARTST
$D002 constant UARTRD
$D004 constant UARTTX
$D006 constant UARTRX

9 constant irq#rxne

: tx
  \ Wait for transmitter to be free
  begin UARTST @ 2 and until
  UARTTX ! ;

: rx-isr
  UARTRX @ >rxq
  1 >CTS_N ;

: uart-rx-init
  \ Clear any pending queued character.
  UARTRX @ drop
  \ Enable the IRQ.
  irq#rxne irq-on
  0 >CTS_N ;

( ----------------------------------------------------------- )
( Icestick board features )

: ledtog  5 + #bit OUTTOG ! ;

\ ----------------------------------------------------------------------
\ Text mode video display

$C000 constant VTH  \ video - timing - horizontal
$C008 constant VTV  \ video - timing - vertical
$C010 constant VPX  \ video - pixel count
$C012 constant VIA  \ video - interrupt acknowledge
$C014 constant VFB  \ video - font base
$C016 constant VWA  \ video - write address
$C018 constant VWD  \ video - write data
$C01A constant VC0  \ video - character 0

\ Overwrites a section of video memory with a given value.
: vfill  ( v-addr u x -- )
  VWA @ >r    \ save cursor position
  >r          \ stash cell to write
  swap VWA !  \ set up write address
  begin
    dup
  while ( count ) ( R: vwa x )
    1- r@ VWD !
  repeat
  rdrop drop
  r> VWA !  \ restore old cursor
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
  0 VC0 !   \ make it the active screen
  vsize vcols @ - VWA !   \ cursor to lower left
  ;

\ Scrolls the display up, revealing a blank line at the bottom. Leaves the
\ cursor address unchanged (i.e. it moves up on the display).
: vscroll
  vcols @ VC0 +!
  VC0 @  vsize vcols @ - + $7FF and  vcols @  vclr
  ;

\ "Types" a character without control character interpretation. Advances the
\ cursor. Scrolls the display as needed.
: vputc ( c -- )
  vatt @ or VWD !   \ store the character with attributes
  VWA @  VC0 @ -  $7FF and    \ get distance from start of display
  vsize = if   \ if we've run off the end
    vscroll  \ reveal another line
  then ;

\ "Types" a character with control character interpretation, simulating a
\ terminal.
: vemit ( c -- )
  7 over = if drop exit then   \ ignore BEL

  8 over = if drop  \ backspace
    VC0 @ VWA @ xor if  VWA @ 1- $7FF and VWA ! then
    exit
  then

  10 over = if drop \ line feed
    vscroll
    VWA @  vcols @ + $7FF and  VWA !
    exit
  then

  12 over = if drop \ form feed / page
    vpage exit
  then

  13 over = if drop \ carriage return
    \ TODO broken broken ?
    \ Compute current offset into the display
    VWA @  VC0 @  -  $7FF and
    \ Round down by division and multiplication
    vcols @ u/  vcols @ u*
    \ Project back into the display and assign
    VC0 @ + $7FF and VWA !
    exit
  then

  vputc ;


: vid
  103 VTH !
  207 VTH 4 + !
  639 VTH 6 + !
  100 VTV !
  121 VTV 4 + !
  399 VTV 6 + !
  80 vcols !
  25 vrows !
  0 15 vcolor!
  vpage
  ;
( ----------------------------------------------------------- )
( SD Card )


: cycles ( u -- )   \ delays for at least u cycles
  >r
  TIMV @
  begin   ( start )
    TIMV @ over -   ( start delta )
    r@ u< 0= if
      rdrop drop exit
    then
  again ;

variable sdcyc  50 sdcyc !
: sddelay sdcyc @ cycles ;

2 outpin >sdclk
3 outpin >sdmosi
4 outpin >sdcs_

: sdx1  ( bits -- bits' )
  $8000 over and >sdmosi
  1 lshift

  sddelay  1 >sdclk
  sddelay

  IN @ 2 and 1 rshift  or
  
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
  1 sdcyc !   \ high speed
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
$8000 1024 - constant blkbuf

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
  block 'SOURCE !  1024 'SOURCE cell+ !  0 >IN !
  \ Try to interpret.
  ['] interpret catch
  \ Whether that succeeded or failed, restore the old input spec.
  r> >IN !  r> r> 'SOURCE cell+ !  'SOURCE !  r> dup blk !  ( except old-blk )
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
  3 #bit  IN @ and  12 lshift     \ get data value in bit 15
  kbdbuf @  1 rshift or  kbdbuf ! \ insert it into shift register
  kbd#bit @ 1 - kbd#bit !d        \ decrement bit counter
  0= if   \ we're done
    irq#negedge irq-off           \ disable this IRQ
    12 #bit OUTSET !               \ pull clock low
  else
    IN !d                         \ acknowledge IRQ
  then ;

: kbd@
  11 kbd#bit !                    \ expecting 11 bits
  12 #bit OUTCLR !                 \ release clock line
  IN !d                           \ clear pending negedge IRQ
  irq#negedge irq-on              \ enable IRQ
  begin kbd#bit @ 0= until        \ wait for all bits
  kbdbuf @ 6 rshift $FF and       \ extract bits
  ;

: kbdinit
  12 #bit OUTSET !                 \ pull clock low
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

: delay 0 begin 1+ dup 0= until drop ;

create vectors  16 cells allot

: isr
  \ Vectored interrupt dispatcher.
  \ Which interrupts are active?
  15 IRQST @
  begin   ( vector# irqst )
    dup \ while any remain
  while
    $8000 over and if
      over cells  vectors + @ execute
    then
    1 lshift
    swap 1 - swap
  repeat
  drop drop
  \ Patch up interrupt return address.
  r> 2 - >r
  \ Atomically enable interrupts and return.
  ei ;

\ Vector table
' rx-isr vectors 9 cells + !
' kbdisr vectors irq#negedge cells + !

create TIB 80 allot

: rx! rx dup 0< if rx! exit then ;

: quit
  0 RSP!
  0 handler !
  postpone [
  begin
    TIB 'SOURCE !
    80  'SOURCE cell+ !
    0 >IN !
    SOURCE accept  'SOURCE cell+ !
    space
    ['] interpret catch
    ?dup if
      true over = if
        \ abort isn't supposed to print
        drop
      else 
        . '!' emit
      then
    else
      STATE @ 0= if  
        'o' emit 'k' emit
      then
    then
    cr
  again ;

: cold
  \ Initialize user area
  $7B80 U0 !
  0 handler !
  10 base !
  forth definitions

  uart-rx-init
  312 UARTRD ! \ Set baud rate to 115200

  \ Take a best-effort crack at initializing the disk
  ['] sdinit catch drop

  IN @  4 #bit and if   \ If S2 is held, boot with serial console
    ['] tx 'emit !
    ['] rx! 'key !
  else  \ otherwise, normal config
    vid
    ['] vemit 'emit !
    ['] kbdkey 'key !
  then
  ei
  ." bsforth; last definition: "
  CURRENT @ @ cell+ count type cr
  quit ;

( install cold as the reset vector )
' cold  u2/  0 !
( install isr as the interrupt vector )
' isr  u2/  2 !

remarker empty

.( Compilation complete. HERE is... )
here host.
