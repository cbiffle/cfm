\ Test input for assembler.

: blinky
  0
  begin   ( count )
    0x7FFF invert   ( count ioaddr )
    2dup! drop      ( count )
    1 +   ( count' )
  again ;

