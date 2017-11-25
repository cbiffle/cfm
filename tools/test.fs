\ Test input for assembler.

\ Primitive ALU instruction definitions
0x6180 alu: swap
0x6020 alu: 2dup!
0x6103 alu: drop
0x6600 alu: invert
0x6203 alu: +
0x6a03 alu: -
0x6700 alu: over/=

4 org
\ Delays for u iterations, which in practice means 5u + 3 cycles.
\ TODO drops to 5u + 2 with drop-return fusion.
: delay   ( u -- )
  begin
    1 -   ( u' )
    0 over/=
  until drop ;

: blinky
  0
  begin   ( count )
    0x7FFF invert   ( count ioaddr )
    2dup! drop      ( count )
    1 +   ( count' )
    1000 delay
  again ;

0 org
: main blinky ;
