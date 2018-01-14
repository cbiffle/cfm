\ SPI Flash bootstrap loader

\ This is intended to get swapped in for RAM starting at address 0 on cold boot.
\ Its goal is to read 16 kiW from SPI flash into RAM, unmap the ROM, and jump
\ into the code in RAM.

\ We leave the processor in nearly its reset state, except that we leave the
\ SPI flash unselected.

\ Assembler primitives
$6023 alu: !a            ( a b -- b )
$6033 alu: io!a          ( a b -- b )
$6081 alu: dup           ( x -- x x )
$6103 alu: drop          ( x -- )
$6123 alu: !d            ( a b -- a )
$6133 alu: io!d          ( a b -- a )
$6147 alu: >r            ( a --  R: -- a )
$6180 alu: swap          ( a b -- b a )
$6181 alu: over          ( a b -- a b a )
$6203 alu: +             ( a b -- a+b)
$6303 alu: and           ( a b -- a&b)
$6403 alu: or            ( a b -- a|b)
$6703 alu: =             ( a b -- a=b )
$6781 alu: 2dup_=        ( a b -- a b a=b )
$6903 alu: rshift        ( a b -- a>>b )
$6a03 alu: -             ( a b -- a-b)
$6c00 alu: @             ( x -- [x] )
$6c10 alu: io@           ( x -- [x] )
$6d03 alu: lshift        ( a b -- a<<b )


2 org \ leave room for the reset vector

\ Port definitions
$0002 constant OUTSET
$0004 constant OUTCLR
$2000 constant IN
$8020 constant VPAL0
$8030 constant VPAL8

\ Loaded RAM region
0 constant RAM_BEGIN
$4000 constant RAM_END

: io! io!a drop ;
: 1- 1 - ;
: 2* 1 lshift ;

: select $800 OUTCLR io! ;
: deselect $800 OUTSET io! ;

: spibits   ( data n -- data' )
  swap
  $400 over 15 rshift if OUTSET else OUTCLR then io!
  1 lshift
  $200 OUTSET io!    \ sclk high
  IN io@ 5 rshift 1 and or
  $200 OUTCLR io!    \ sclk low
  swap
  1 - dup if spibits exit then
  drop ;

: >spi> 16 spibits ;

: >spi >spi> drop ;

\ Fills a buffer from 'start' to 'end' (exclusive) with bytes read from SPI.
: (read)  ( end start -- )
  2dup_= if drop drop exit then
  0 >spi>  dup 8 lshift  swap 8 rshift or
  swap !a 2 + (read) ;

\ Polls the SPI flash until it responds with a sane ID byte. This is critical
\ for booting after loading the FPGA SRAM with icoprog, because icoprog is slow
\ to release the SPI bus. Until it does so, we can't talk to the Flash.
: poll
  select
  $AB00 >spi 0 >spi
  0 8 spibits
  deselect
  dup 0 =  swap $FF = or if poll exit then ;

: palette-low
  dup 2* VPAL0 + io!d
  dup if 1- palette-low exit then
  drop ;

: palette-high
  dup 56 + over 2* VPAL8 + io!
  dup if 1- palette-high exit then
  drop ;

: go
  7 palette-low
  7 palette-high
  deselect
  poll

  select
  $0304 >spi $0000 >spi
  RAM_END RAM_BEGIN (read)
  deselect

  0 >r ;

0 org
: reset-vector go ;
