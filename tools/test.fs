\ Test input for assembler.

\ Primitive ALU instruction definitions
0x6180 alu: swap          ( a b -- b a )
0x6020 alu: 2dup/!        ( a b -- a b )
0x6381 alu: 2dup/and      ( a b -- a b a&b )
0x6103 alu: drop          ( x -- )
0x6600 alu: invert        ( x -- ~x )
0x6203 alu: +             ( a b -- a+b)
0x6a03 alu: -             ( a b -- a-b)
0x6903 alu: rshift        ( a b -- a>>b )
0x6d03 alu: lshift        ( a b -- a<<b )
0x6303 alu: and           ( a b -- a&b)
0x6403 alu: or            ( a b -- a|b)
0x6081 alu: dup           ( x -- x x )
0x6c00 alu: @             ( x -- [x] )
0x6703 alu: =             ( a b -- a=b )
0x6f03 alu: u<            ( a b -- a<b )
0x6181 alu: over          ( a b -- a b a )

4 org

\ Delays for u iterations, which in practice means 5u + 2 cycles.
: delay   ( u -- )
  begin
    1 -   ( u' )
    0 over =
  until drop ;

\ I/O port addresses are defined as their complements, so they can be loaded by a
\ literal instruction and inverted before use. TODO - the assembler should probably
\ do this for us.
0x7FFF constant ~outport
0x5FFF constant ~inport

\ For 19200 bps at 40MHz core clock
: bit-delay 416 delay ;
: half-bit-delay 208 delay ;

\ Treating 'c' as a shift register, transmits its LSB and shifts it to the right.
: bit>  ( c -- c' )
  1 2dup/and          ( c 1 lsb )
  ~outport invert !   ( c 1 )
  rshift              ( c' )
  bit-delay ;

\ Transmits a byte with no parity, one stop bit.
: tx  ( c -- )
  1 lshift      \ evacuate start bit
  0x200 or      \ set stop bit
  bit>          \ start bit
  bit> bit> bit> bit>
  bit> bit> bit> bit> \ data bits
  bit>          \ stop bit
  drop ;

\ Samples the status of the RX pin into bit 15.
: rx?  ( -- ? ) ~inport invert @ 15 lshift ;

\ Spins until observing a high-to-low transition on RX.
: ...start
  begin rx?     until
  begin rx? 0 = until ;

: >bit  ( x -- x' )
  1 rshift
  rx? or
  bit-delay ;

\ Receives a byte from the RX pin and returns both the bits received, and a success
\ flag. The receive may be unsuccessful if there was a framing error.
: rx  ( -- c ? )
  ...start half-bit-delay   \ Delay until halfway into the suspected start bit.
  0 >bit                    \ Record the start bit level.
  0 >bit >bit >bit >bit
    >bit >bit >bit >bit     \ Record the data bits
    8 rshift                \ and shift
  swap >bit  14 rshift      \ Record the stop bit with the start bit and shift.
  2 = \ Stop bit high, start bit low => binary 10 => 2
  ;

\ UART demo
: cr 0x0d tx 0x0a tx ;
: space 0x20 tx ;

: .nib  ( c -- c' )
  dup 12 rshift   \ extract top nibble
  9 over u< if 7 + then 0x30 +   \ convert to hex
  tx
  4 lshift ;

: .hex .nib .nib .nib .nib space drop ;

: hello
  0x48 tx
  0x65 tx
  0x6c dup tx tx
  0x6f tx
  0x21 tx
  cr ;

: chatty
  1 bit> drop           \ Ensure TX is high for a bit time before beginning.
  hello
  0x2152 invert .hex cr
  begin
    rx 0 = if 0x21 tx 0x3F tx cr then
    dup tx cr
    .hex cr
  again ;

0 org
: main chatty ;
