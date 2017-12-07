( Bootstrap Forth, my first Forth for the CFM. )


( --------------------------------------------------------------------------- )
( Kernel code. )

( Instruction primitives that are hard to express at a higher level. )
( We directly comma literal instructions into definitions here. )
( This is obviously not portable ;-)
: +      [ $6203 asm, ] ;
: swap   [ $6180 asm, ] ;
: over   [ $6181 asm, ] ;
: nip    [ $6003 asm, ] ;
: lshift [ $6d03 asm, ] ;
: rshift [ $6903 asm, ] ;
: dup    [ $6081 asm, ] ;
: =      [ $6703 asm, ] ;
: drop   [ $6103 asm, ] ;
: invert [ $6600 asm, ] ;
: @      [ $6c00 asm, ] ;
: or     [ $6403 asm, ] ;
: and    [ $6303 asm, ] ;
: -      [ $6a03 asm, ] ;
: <      [ $6803 asm, ] ;

( Has the effect of a store that only drops the address. )
: 2dup_!_drop [ $6123 asm, ] ;

: ! 2dup_!_drop drop ;

( Access to the system variables block )
4 constant LATEST
6 constant DP
8 constant U0
10 constant STATE
12 constant FREEZEP

$FFFF constant true
0 constant false
2 constant cell

: +!  swap over @ + swap ! ;
: 0= 0 = ;
: <> = 0= ;
: 2dup over over ;

: here  ( -- addr )  DP @ ;
: allot  DP +! ;
: freeze  here FREEZEP ! ;
: cells  1 lshift ;
: aligned  dup 1 and + ;

: raw,  here !  cell allot ;
: , raw, freeze ;

( Assembles an instruction into the dictionary, with smarts. )
: asm,
  here FREEZEP @ = if  ( Can't do anything clever, just... )
    raw,
  else  ( Fusion is a possibility... )
    here cell - @   ( new-inst prev-inst )

    over $700C = if ( if we're assembling a bare return instruction... )
      dup $704C and $6000 = if  ( ...on a non-returning ALU instruction )
        $FFFF cells allot
        nip  $100C or  asm, exit
      then
      dup $E000 and $4000 = if  ( ...on a call )
        $FFFF cells allot
        nip $1FFF and  asm, exit
      then
    then

    ( No patterns matched. )
    drop raw,
  then ;

: >r  $6147 asm, ; immediate
: r>  $6b8d asm, ; immediate
: r@  $6b81 asm, ; immediate
: exit  $700c asm, ; immediate

: execute  ( i*x xt -- j*x )  >r ; ( NOINLINE )

: compile,  ( xt -- )
  ( Convert the XT into an assembly instruction. )
  dup @  $F00C and  $700C = if  ( Is the destination a fused op-return? )
    ( Inline it with the return effect stripped. )
    @ $EFF3 and
  else
    ( Convert the CFA into a call. )
    1 rshift $4000 or
  then
  asm, ;

: align  DP @  aligned  DP ! ;

<TARGET-EVOLVE> ( make bootstrap aware of dictionary words )

( Byte access. These words access the bytes within cells in little endian. )
: c@  dup @
      swap 1 and if ( lsb set )
        8 rshift
      else
        $FF and
      then ;

: c!  dup >r
      1 and if ( lsb set )
        8 lshift
        r@ @ $FF and or
      else
        $FF and
        r@ @ $FF00 and or
      then
      r> ! ;

: c,  here c!  1 allot ;

: begin freeze here ; immediate
: again 1 rshift asm, ; immediate
: until 1 rshift $2000 or asm, ; immediate

: if freeze here $2000 asm, ; immediate
: then freeze dup @  here 1 rshift or  swap ! ; immediate
: else
  freeze here $0000 asm, swap
  dup @  here 1 rshift or  swap ! ; immediate

( Compares a string to the name field of a header. )
: name= ( c-addr u nfa -- ? )
  >r  ( stash the NFA )
  r@ c@ over = if  ( lengths equal )
    r> 1 + swap   ( c-addr c-addr2 u )
    begin
      >r
      over @ over @ <> if
        r>
        drop drop drop 0 exit
      then
      1 + swap 1 +
      r> 1 -
      dup 0=
    until
    drop drop drop true
  else
    r> drop drop drop 0
  then ;

( Variant of standard FIND that uses a modern string and returns the flags. )
: sfind  ( c-addr u -- c-addr u 0 | xt flags true )
  LATEST @ begin          ( c-addr u lfa )
    dup 0= if             ( c-addr u 0 )
      exit
    then
    >r  ( stash the LFA ) ( c-addr u )              ( R: lfa )
    2dup                  ( c-addr u c-addr u )     ( R: lfa )
    r@ cell +             ( c-addr u c-addr u nfa ) ( R: lfa )
    name= if              ( c-addr u )              ( R: lfa )
      nip                 ( u )                     ( R: lfa )
      r> cell +           ( u nfa )
      1 +  +  aligned     ( ffa )
      dup cell +          ( ffa cfa )
      swap @              ( cfa flags )
      true exit           ( cfa flags true )
    then    ( c-addr u ) ( R: lfa )
    r>      ( c-addr u lfa )
     @      ( c-addr u lfa' )
  again ;

<TARGET-EVOLVE>  ( for sfind )

: literal
  dup 0 < if  ( MSB set )
    true swap invert
  else
    false swap
  then
  $8000 or asm,
  if $6600 asm, then ; immediate

( General code above )

( ----------------------------------------------------------- )
( Icestick SoC support code )

: #bit  1 swap lshift ;

( ----------------------------------------------------------- )
( Interrupt Controller )

$F000 constant irqcon-st  ( status / enable trigger )
$F002 constant irqcon-en  ( enable )
$F004 constant irqcon-se  ( set enable )
$F006 constant irqcon-ce  ( clear enable )

( Atomically enables interrupts and returns. This is intended to be tail )
( called from the end of an ISR. )
: enable-interrupts  irqcon-st 2dup_!_drop ;

: disable-irq  ( u -- )  #bit irqcon-ce ! ;
: enable-irq   ( u -- )  #bit irqcon-se ! ;

13 constant irq-timer-m1
14 constant irq-timer-m0
15 constant irq-inport-negedge

( ----------------------------------------------------------- )
( I/O ports )

$8000 constant outport      ( literal value)
$8002 constant outport-set  ( 1s set pins, 0s do nothing)
$8004 constant outport-clr  ( 1s clear pins, 0s do nothing)
$8006 constant outport-tog  ( 1s toggle pins, 0s do nothing)

$A000 constant inport

( ----------------------------------------------------------- )
( Timer )

$C000 constant timer-ctr
$C002 constant timer-flags
$C004 constant timer-m0
$C006 constant timer-m1

( ----------------------------------------------------------- )
( UART emulation )

( Spins reading a variable until it contains zero. )
: poll0  ( addr -- )  begin dup @ 0= until drop ;

( Decrements a counter variable and leaves its value on stack )
: -counter  ( addr -- u )
  dup @   ( addr u )
  1 -     ( addr u' )
  swap    ( u' addr )
  2dup !  ( u' addr )
  drop ;

2500 constant cycles/bit
1250 constant cycles/bit/2

variable uart-tx-bits   ( holds bits as they're shifted out )
variable uart-tx-count  ( tracks the number of bits remaining )

: tx-isr
  1 timer-flags !     ( acknowledge interrupt )
  uart-tx-bits @
  1 over and
  if outport-set else outport-clr then
  1 swap !
  1 rshift uart-tx-bits !

  uart-tx-count -counter if
    timer-ctr @ cycles/bit + timer-m1 !
  else
    irq-timer-m1 disable-irq
  then ;

: tx
  ( Wait for transmitter to be free )
  uart-tx-count poll0
  ( Frame the byte )
  1 lshift
  $200 or
  uart-tx-bits !
  10 uart-tx-count !
  irq-timer-m1 enable-irq ;

variable uart-rx-bits
variable uart-rx-bitcount

variable uart-rx-buf  3 cells allot
variable uart-rx-hd
variable uart-rx-tl

: CTSon 2 outport-clr ! ;
: CTSoff 2 outport-set ! ;

: rxq-empty? uart-rx-hd @ uart-rx-tl @ = ;
: rxq-full? uart-rx-hd @ uart-rx-tl @ - 4 = ;

( Inserts a cell into the receive queue. This is intended to be called from )
( interrupt context, so if it encounters a queue overrun, it simply drops )
( data. )
: >rxq
  rxq-full? if
    drop
  else
    uart-rx-buf  uart-rx-hd @ 6 and +  !
    2 uart-rx-hd +!
  then ;

( Takes a cell from the receive queue. If the queue is empty, spin. )
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
  \ Set up the timer to interrupt us again halfway into the start bit.
  \ First, the timer may have rolled over while we were waiting for a new
  \ frame, so clear its pending interrupt status.
  2 timer-flags !
  \ Next set the match register to the point in time we want.
  timer-ctr @  cycles/bit/2 +  timer-m0 !
  \ We don't need to clear the IRQ condition, because we won't be re-enabling
  \ it any time soon. Mask our interrupt.
  irq-inport-negedge disable-irq

  \ Prepare to receive a ten bit frame.
  10 uart-rx-bitcount !

  \ Now enable its interrupt.
  irq-timer-m0 enable-irq ;

\ Triggered at each sampling point during an RX frame.
: rx-timer-isr
  \ Sample the input port into the high bit of a word.
  inport @  15 lshift
    \ Reset the timer for the next sample point.
    timer-ctr @  cycles/bit +  timer-m0 !
  \ Load this into the frame shift register.
  uart-rx-bits @  1 rshift  or  uart-rx-bits !
  \ Decrement the bit count.
  uart-rx-bitcount -counter if \ we have more bits to receive
    \ Clear the interrupt condition.
    2 timer-flags !
  else  \ we're done, disable timer interrupt
    irq-timer-m0 disable-irq
    \ Enqueue the received frame
    uart-rx-bits @ >rxq
    \ Clear any pending negedge condition
    0 inport !
    \ Enable the initial negedge ISR to detect the start bit.
    irq-inport-negedge enable-irq
    \ Conservatively deassert CTS to try and stop sender.
    CTSoff
  then ;

\ Receives a byte from RX, returning the bits and a valid flag. The valid flag may
\ be false in the event of a framing error.
: rx  ( -- c ? )
  rxq>

  \ Dissect the frame and check for framing error. The frame is in the
  \ upper bits of the word.
  6 rshift
  dup 1 rshift $FF and   \ extract the data bits
  swap $201 and          \ extract the start/stop bits.
  $200 =                 \ check for valid framing
  rxq-empty? if CTSon then ;  \ allow sender to resume if we've emptied the queue.


( ----------------------------------------------------------- )
( Icestick board features )

: ledtog  4 + #bit outport-tog ! ;


( ----------------------------------------------------------- )
( Demo wiring below )

: delay 0 begin 1 + dup 0 = until drop ;

: isr
  irqcon-st @
  $4000 over and if
    rx-timer-isr
  then
  $8000 over and if
    rx-negedge-isr
  then
  $2000 over and if
    tx-isr
  then
  drop
  r> 2 - >r
  enable-interrupts ;

: cold
  uart-rx-init
  enable-interrupts
  $FF tx
  begin
    rx if tx else drop then
  again ;

( install cold as the reset vector )
' cold  1 rshift  0 !
( install isr as the interrupt vector )
' isr  1 rshift  2 !
