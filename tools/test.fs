\ Test input for assembler.

\ Primitive ALU instruction definitions
0x6180 alu: swap          ( a b -- b a )
0x6020 alu: 2dup/!        ( a b -- a b )
0x6381 alu: 2dup/and      ( a b -- a b a&b )
0x6103 alu: drop          ( x -- )
0x6600 alu: invert        ( x -- ~x )
0x6203 alu: +             ( a b -- a+b)
0x6a03 alu: -             ( a b -- a-b)
0x6700 alu: over/=        ( a b -- a a=b )
0x6123 alu: over/swap/!   ( a b -- a )
0x6903 alu: rshift        ( a b -- a>>b )
0x6d03 alu: lshift        ( a b -- a<<b )
0x6403 alu: or            ( a b -- a|b)
0x6081 alu: dup           ( x -- x x )

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
    2dup/! drop      ( count )
    1 +   ( count' )
    1000 delay
  again ;

0x7FFF constant ~outport

\ For 19200 bps at 40MHz core clock
: bit-delay 416 delay ;

: ! over/swap/! drop ;

: bit  ( c -- c' )
  1 2dup/and          ( c 1 lsb )
  ~outport invert !   ( c 1 )
  rshift              ( c' )
  bit-delay ;

: tx  ( c -- )
  1 lshift      \ evacuate start bit
  0x200 or      \ set stop bit
  bit           \ start bit
  bit bit bit bit
  bit bit bit bit \ data bits
  bit           \ stop bit
  drop
  500 delay   \ helps my terminal synchronize
  ;

: chatty
  begin
    0x48 tx
    0x65 tx
    0x6c dup tx tx
    0x6f tx
    0x21 tx
    0x0d tx
    0x0a tx
  again ;

0 org
: main chatty ;
