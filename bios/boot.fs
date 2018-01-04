\ SPI Flash bootstrap loader

\ This is intended to get swapped in for RAM starting at address 0 on cold boot.
\ Its goal is to read 16 kiW from SPI flash into RAM, unmap the ROM, and jump
\ into the code in RAM.

\ We leave the processor in nearly its reset state, except that we leave the
\ SPI flash unselected.

\ Assembler primitives
0x6023 alu: !a            ( a b -- b )
0x6081 alu: dup           ( x -- x x )
0x6103 alu: drop          ( x -- )
0x6123 alu: !d            ( a b -- a )
0x6147 alu: >r            ( a --  R: -- a )
0x6180 alu: swap          ( a b -- b a )
0x6181 alu: over          ( a b -- a b a )
0x6203 alu: +             ( a b -- a+b)
0x6303 alu: and           ( a b -- a&b)
0x6403 alu: or            ( a b -- a|b)
0x6703 alu: =             ( a b -- a=b )
0x6781 alu: 2dup_=        ( a b -- a b a=b )
0x6903 alu: rshift        ( a b -- a>>b )
0x6a03 alu: -             ( a b -- a-b)
0x6c00 alu: @             ( x -- [x] )
0x6d03 alu: lshift        ( a b -- a<<b )


2 org \ leave room for the reset vector

\ Port definitions
0x8002 constant OUTSET
0x8004 constant OUTCLR
0x9000 constant IN

\ Loaded RAM region
0 constant RAM_BEGIN
0x8000 constant RAM_END

: >pin  ( ? mask -- )
  swap if OUTSET else OUTCLR then !d drop ;
  \ 7 cells

: >cs_n 0x800 >pin ;
: >mosi 0x400 >pin ;
: >sclk 0x200 >pin ;
  \ 2 cells ea

: spibits   ( data n -- data' )
  swap
  dup 15 rshift >mosi
  1 lshift
  1 >sclk
  IN @ 5 rshift 1 and or
  0 >sclk
  swap
  1 - dup if spibits exit then
  drop ;
  \ 25 cells

: >spi> 16 spibits ;
  \ 2 cells

: >spi >spi> drop ;

\ Fills a buffer from 'start' to 'end' (exclusive) with bytes read from SPI.
: (read)  ( end start -- )
  2dup_= if drop drop exit then
  0 >spi> swap !a 2 + (read) ;
  \ 10 cells

\ Polls the SPI flash until it responds with a sane ID byte. This is critical
\ for booting after loading the FPGA SRAM with icoprog, because icoprog is slow
\ to release the SPI bus. Until it does so, we can't talk to the Flash.
: poll
  0 >cs_n
  0xAB00 >spi 0 >spi
  0 8 spibits
  1 >cs_n
  dup 0 =  swap 0xFF = or if poll exit then ;
  
: go
  1 >cs_n
  poll

  0 >cs_n
  0x0304 >spi 0x0000 >spi
  RAM_END RAM_BEGIN (read)
  1 >cs_n

  0 >r ;

0 org
: reset-vector go ;
