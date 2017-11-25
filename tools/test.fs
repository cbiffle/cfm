\ Test input for assembler.

\ Primitive ALU instruction definitions
0x6180 alu: swap
0x6020 alu: 2dup!
0x6103 alu: drop
0x6600 alu: invert
0x6203 alu: +

2 org
: blinky
  0
  begin   ( count )
    0x7FFF invert   ( count ioaddr )
    2dup! drop      ( count )
    1 +   ( count' )
  again ;

0 org
: main blinky ;
