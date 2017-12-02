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
0x6803 alu: <             ( a b -- a<b )
0x6181 alu: over          ( a b -- a b a )
0x6e81 alu: depth         ( a b -- a b a )
0x6147 alu: >r            ( a --  R: -- a )
0x6b8d alu: r>            ( -- a  R: a -- )

\ Skip over the reset and interrupt vectors before compiling actual code.
4 org

: execute  ( i*x xt -- j*x ) >r ;

\ I/O port addresses are defined as their complements, so they can be loaded by
\ a literal instruction and inverted before use. TODO - the assembler should
\ probably do this for us.
0x7FFF constant ~outport      ( 8000 )
0x7FFD constant ~outport-set  ( 8002 )
0x7FFB constant ~outport-clr  ( 8004 )
0x7FF9 constant ~outport-tog  ( 8006 )
0x5FFF constant ~inport       ( A000 )
0x3FFF constant ~timer-ctr    ( C000 )
0x3FFD constant ~timer-flags  ( C002 )
0x3FFB constant ~timer-m0     ( C004 )
0x3FF9 constant ~timer-m1     ( C006 )
0x0FFF constant ~irqcon-st    ( F000 )
0x0FFD constant ~irqcon-en    ( F002 )
0x0FFB constant ~irqcon-se    ( F004 )
0x0FF9 constant ~irqcon-ce    ( F006 )

: ledon  4 +  1 swap lshift  ~outport-set invert ! ;
: ledoff 4 +  1 swap lshift  ~outport-clr invert ! ;
: ledtog 0xF0 ~outport-tog invert ! ;


\ UART support.

\ For 19200 bps, one bit = 52083.333 ns
\ At 48MHz core clock, 1 cycle = 20.833 ns
\ Thus: 2500.04 cycles / bit
2500 constant cycles/bit
1250 constant cycles/bit/2

variable uart-tx-bits   \ Used to hold bits as they're shifted out
variable uart-tx-count  \ Number of bits left to transmit
: uart-tx-init
  0 uart-tx-count ! ;

\ Invoked by the timer when we need to transmit the next bit.
: tx-isr
  1 ~timer-flags invert !       \ acknowledge interrupt
  uart-tx-bits @                ( bits )
  1 over and                    ( bits lsb )
  if ~outport-set else ~outport-clr then invert ( bits regaddr )
  1 swap !                      ( bits )
  1 rshift uart-tx-bits !       ( )

  uart-tx-count @  1 -  dup  uart-tx-count !
  if
    ~timer-ctr invert @  cycles/bit +  ~timer-m1 invert !
  else
    \ disable interrupt
    0x2000 ~irqcon-ce invert !
  then ;

: tx
  \ Wait for the transmitter to be free
  begin uart-tx-count @ 0 = until
  \ Frame the byte
  1 lshift              \ add a start bit
  0x200 or              \ add a stop bit
  uart-tx-bits !        \ stash it where the ISR can find it
  10 uart-tx-count !    \ prepare to transmit 10 bits
  \ enable interrupt
  0x2000 ~irqcon-se invert ! ;

variable uart-rx-bits   \ Used to hold bits as they're shifted in
variable uart-rx-count  \ Number of bits left to receive

: uart-rx-init
  0 uart-rx-count ! ;

\ Triggered when we're between frames and RX drops.
: rx-negedge-isr
  \ We don't need to clear the IRQ condition, because we won't be re-enabling
  \ it any time soon. Mask our interrupt.
  0x7FFF invert ~irqcon-ce invert !

  \ Set up the timer to interrupt us again halfway into the start bit.
  \ First, the timer may have rolled over while we were waiting for a new
  \ frame, so clear its pending interrupt status.
  2 ~timer-flags invert !
  \ Next set the match register to the point in time we want.
  ~timer-ctr invert @  cycles/bit/2 +  ~timer-m0 invert !
  \ Now enable its interrupt.
  0x4000 ~irqcon-se invert ! ;

\ Triggered at each sampling point during an RX frame.
: rx-timer-isr
  \ Sample the input port into the high bit of a word.
  ~inport invert @  15 lshift
  \ Load this into the frame shift register.
  uart-rx-bits @  1 rshift  or  uart-rx-bits !
  \ Decrement the bit count, keeping the result around.
  uart-rx-count @ 1 -  dup  uart-rx-count !
  if  \ we have more bits to receive
    \ Clear the interrupt condition.
    2 ~timer-flags invert !
    \ Reset the timer for the next sample point.
    ~timer-ctr invert @  cycles/bit +  ~timer-m0 invert !
  else  \ we're done, disable interrupt
    0x4000 ~irqcon-ce invert !
  then ;

\ Receives a byte from RX, returning the bits and a valid flag. The valid flag may
\ be false in the event of a framing error.
: rx  ( -- c ? )
  \ Prepare the receive state machine.
  10 uart-rx-count !

  \ Clear any pending negedge condition
  0 ~inport invert !
  \ Enable the initial negedge ISR to detect the start bit.
  0x7FFF invert ~irqcon-se invert !

  \ Spin until the frame is complete.
  begin  uart-rx-count @ 0 =  until

  \ Dissect the frame and check for framing error. The frame is in the
  \ upper bits of the word.
  uart-rx-bits @  6 rshift
  dup 1 rshift 0xFF and   \ extract the data bits
  swap 0x201 and          \ extract the start/stop bits.
  0x200 = ;               \ check for valid framing


\ Simple monitor
: cr 0x0d tx 0x0a tx ;
: space 0x20 tx ;

: .nib  ( c -- c' )
  dup 12 rshift   \ extract top nibble
  9 over u< if 7 + then [char] 0 +   \ convert to hex
  tx
  4 lshift ;

: .hex .nib .nib .nib .nib space drop ;

: rx!
  rx if exit then
  rx! ;

: >nib  ( x -- x' ? )
  begin
    rx!
    3 over = if drop 0 exit then
    32 over < if
      dup tx
      [char] 0 -  9 over u< if 7 - then
      0xF over u< if
        drop  7 tx  8 tx
      else
        swap 4 lshift or
        1 exit
      then
    else
      drop 7 tx
    then
  again ;

: read-word
  0 >nib if >nib if >nib if >nib if 1 exit then then then then
  drop 0 ;

: dump
  begin
    over over = if drop drop exit then
    dup .hex space
    dup @ .hex cr
    2 +
  again ;

: cmd
  depth .hex [char] > tx space
  rx! dup tx

  [char] r over = if drop
    read-word if
      space @ .hex
    else
      [char] ? tx
    then
    cr exit
  then

  [char] w over = if drop
    read-word if
      space read-word if
        swap ! [char] ! tx
      else
        [char] ? tx
      then
    else
      [char] ? tx
    then
    cr exit
  then

  [char] t over = if drop
    space dup .hex cr exit
  then

  [char] p over = if drop
    read-word if
    else
      [char] ? tx
    then
    cr exit
  then

  [char] d over = if drop
    drop cr exit
  then

  [char] x over = if drop
    read-word if
      cr
      execute
    else
      [char] ? tx cr
    then
    exit
  then

  [char] v over = if drop
    read-word if
      space read-word if
        cr swap dump
      else
        [char] ? tx cr
      then
    else
      [char] ? tx cr
    then
    exit
  then

  drop
  [char] ? tx cr ;

: hello
  [char] H tx
  [char] e tx
  [char] l dup tx tx
  [char] o tx
  [char] ! tx
  cr ;


\ Return from an interrupt handler. Must be called from tail position.
: reti
  \ Adjust the return address.
  r> 2 - >r
  \ Re-enable interrupts
  ~irqcon-st invert 2dup/! drop ;

variable isr-count

: generic-isr
  ~irqcon-st invert @
  0x4000 over and if
    rx-timer-isr
  then
  0x7FFF invert over and if
    rx-negedge-isr
  then
  0x2000 over and if
    tx-isr
  then
  drop
  reti ;

: chatty
  uart-tx-init
  0 ~irqcon-st invert ! \ Enable interrupts
  0xFF tx   \ Ensure TX has been high for a while
  hello
  begin cmd again ;

0 org
: main chatty ;
: vechack generic-isr ;
