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
: 0= 0 = ;
: +!  ( x addr -- )  swap over @ + swap ! ;

\ I/O port addresses.
0x8000 constant outport
0x8002 constant outport-set
0x8004 constant outport-clr
0x8006 constant outport-tog
0xA000 constant inport
0xC000 constant timer-ctr
0xC002 constant timer-flags
0xC004 constant timer-m0
0xC006 constant timer-m1
0xF000 constant irqcon-st
0xF002 constant irqcon-en
0xF004 constant irqcon-se
0xF006 constant irqcon-ce

13 constant irq-timer-m1
14 constant irq-timer-m0
15 constant irq-inport-negedge

: #bit  ( u -- x )  1 swap lshift ;

: ledon  4 +  #bit  outport-set ! ;
: ledoff 4 +  #bit  outport-clr ! ;
: ledtog 0xF0 outport-tog ! ;

\ Decrements a counter variable and leaves the decremented value on the stack.
: -counter  ( addr -- u )
  dup @ 1 - swap 2dup/! drop ;

: disable-irq  ( u -- )  #bit irqcon-ce ! ;
: enable-irq   ( u -- )  #bit irqcon-se ! ;

: enable-interrupts  irqcon-st 2dup/! drop ;

\ Return from an interrupt handler. Must be called from tail position.
: reti
  \ Adjust the return address.
  r> 2 - >r
  \ Re-enable interrupts
  enable-interrupts ;


\ Polls a variable until it contains zero.
: poll0  ( addr -- )  begin dup @ 0= until drop ;

\ UART support.

\ (48e6 cycle / sec) / (19200 bit / sec) = 2500 cycle / bit
2500 constant cycles/bit
1250 constant cycles/bit/2

variable uart-tx-bits   \ Used to hold bits as they're shifted out
variable uart-tx-count  \ Number of bits left to transmit
: uart-tx-init
  0 uart-tx-count ! ;

\ Invoked by the timer when we need to transmit the next bit.
: tx-isr
  1 timer-flags !       \ acknowledge interrupt
  uart-tx-bits @                ( bits )
  1 over and                    ( bits lsb )
  if outport-set invert else outport-clr invert then invert ( bits regaddr )
  1 swap !                      ( bits )
  1 rshift uart-tx-bits !       ( )

  uart-tx-count -counter if
    timer-ctr @  cycles/bit +  timer-m1 !
  else
    irq-timer-m1 disable-irq
  then ;

: tx
  \ Wait for the transmitter to be free
  uart-tx-count poll0
  \ Frame the byte
  1 lshift              \ add a start bit
  0x200 or              \ add a stop bit
  uart-tx-bits !        \ stash it where the ISR can find it
  10 uart-tx-count !    \ prepare to transmit 10 bits
  \ enable interrupt
  irq-timer-m1 enable-irq ;

variable uart-rx-bits   \ Used to hold bits as they're shifted in
variable uart-rx-bitcount  \ Number of bits left to receive

variable uart-rx-buf
variable uart-rx-buf1 \ HACK use create and allot once I have 'em
variable uart-rx-buf2
variable uart-rx-buf3

variable uart-rx-hd
variable uart-rx-tl

: rxq-empty? uart-rx-hd @ uart-rx-tl @ = ;
: rxq-full? uart-rx-hd @ uart-rx-tl @ - 4 = ;
\ Inserts a cell into the receive queue. This is intended to be called from
\ interrupt context, so if it encounters a queue overrun, it simply drops data.
: >rxq
  rxq-full? if
    drop
  else
    uart-rx-buf  uart-rx-hd @ 6 and +  !
    2 uart-rx-hd +!
  then ;
\ Takes a cell from the receive queue. If the queue is empty, spin.
: rxq>
  begin rxq-empty? 0= until
  uart-rx-buf  uart-rx-tl @ 6 and +  @
  2 uart-rx-tl +! ;

: uart-rx-init
  \ Clear any pending negedge condition
  0 inport !
  \ Enable the initial negedge ISR to detect the start bit.
  irq-inport-negedge enable-irq ;

\ Triggered when we're between frames and RX drops.
: rx-negedge-isr
  \ We don't need to clear the IRQ condition, because we won't be re-enabling
  \ it any time soon. Mask our interrupt.
  irq-inport-negedge disable-irq

  \ Prepare to receive a ten bit frame.
  10 uart-rx-bitcount !

  \ Set up the timer to interrupt us again halfway into the start bit.
  \ First, the timer may have rolled over while we were waiting for a new
  \ frame, so clear its pending interrupt status.
  2 timer-flags !
  \ Next set the match register to the point in time we want.
  timer-ctr @  cycles/bit/2 +  timer-m0 !
  \ Now enable its interrupt.
  irq-timer-m0 enable-irq ;

\ Triggered at each sampling point during an RX frame.
: rx-timer-isr
  \ Sample the input port into the high bit of a word.
  inport @  15 lshift
  \ Load this into the frame shift register.
  uart-rx-bits @  1 rshift  or  uart-rx-bits !
  \ Decrement the bit count.
  uart-rx-bitcount -counter if \ we have more bits to receive
    \ Clear the interrupt condition.
    2 timer-flags !
    \ Reset the timer for the next sample point.
    timer-ctr @  cycles/bit +  timer-m0 !
  else  \ we're done, disable timer interrupt
    irq-timer-m0 disable-irq
    \ Enqueue the received frame
    uart-rx-bits @ >rxq
    \ Clear any pending negedge condition
    0 inport !
    \ Enable the initial negedge ISR to detect the start bit.
    irq-inport-negedge enable-irq
  then ;

\ Receives a byte from RX, returning the bits and a valid flag. The valid flag may
\ be false in the event of a framing error.
: rx  ( -- c ? )
  rxq>

  \ Dissect the frame and check for framing error. The frame is in the
  \ upper bits of the word.
  6 rshift
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

: generic-isr
  irqcon-st @
  0x4000 over and if
    rx-timer-isr
  then
  0x8000 over and if
    rx-negedge-isr
  then
  0x2000 over and if
    tx-isr
  then
  drop
  reti ;

: chatty
  uart-tx-init
  uart-rx-init
  enable-interrupts
  0xFF tx   \ Ensure TX has been high for a while
  hello
  begin cmd again ;

0 org
: main chatty ;
: vechack generic-isr ;
