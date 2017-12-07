( Bootstrap Forth, my first Forth for the CFM. )


( --------------------------------------------------------------------------- )
( Kernel code. )

( Instruction primitives that are hard to express at a higher level. )
( We directly comma literal instructions into definitions here. )
( This is obviously not portable ;-)
: +      [ $6203 , ] ;
: swap   [ $6180 , ] ;
: over   [ $6181 , ] ;
: nip    [ $6003 , ] ;
: lshift [ $6d03 , ] ;
: rshift [ $6903 , ] ;
: dup    [ $6081 , ] ;
: =      [ $6703 , ] ;
: drop   [ $6103 , ] ;
: invert [ $6600 , ] ;
: @      [ $6c00 , ] ;
: or     [ $6403 , ] ;
: and    [ $6303 , ] ;
: -      [ $6a03 , ] ;
: <      [ $6803 , ] ;

: ! [ $6123 , $6103 , ] ;

( Access to the system variables block )
4 constant LATEST
6 constant DP
8 constant U0
10 constant STATE

$FFFF constant true
0 constant false
2 constant cell

: +!  swap over @ + swap ! ;
: 0= 0 = ;
: <> = 0= ;
: 2dup over over ;

: here  ( -- addr )  DP @ ;
: allot  DP +! ;
: ,  here !  cell allot ;

: >r  $6147 , ; immediate
: r>  $6b8d , ; immediate
: r@  $6b81 , ; immediate
: exit  $700c , ; immediate

: execute  ( i*x xt -- j*x )  >r ; ( NOINLINE )

: cells  1 lshift ;
: aligned  dup 1 and + ;
: compile,  1 rshift  $4000 or  , ;
: align  DP @  aligned  DP ! ;

.( value of true: )
' true execute host.

<TARGET-EVOLVE> ( make bootstrap aware of dictionary words )

( Byte access. These words access the bytes within cells in little endian. )
: c@  dup @
      swap 1 and if ( lsb set )
        8 rshift
      else
        $FF and
      then ;

.( Length of name of c@: )
LATEST @ 2 + c@ host.

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

.( c, test, should be 1 2 3 4 )
here
1 c, 2 c, 3 c, 4 c,
dup c@ host.
dup 1 + c@ host.
dup 2 + c@ host.
dup 3 + c@ host.
drop

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

.( name= test: does its name match itself? )
LATEST @ 3 +    ( c-addr )
LATEST @ 2 + c@ ( u )
LATEST @ 2 +    ( nfa )
name= host.

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

.( sfind test: can it find itself ? )
LATEST @ 3 +
LATEST @ 2 + c@
sfind
host. host. host.

<TARGET-EVOLVE>  ( for sfind )

: literal
  dup 0 < if  ( MSB set )
    true swap invert
  else
    false swap
  then
  $8000 or ,
  if $6600 , then ; immediate

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
: enable-interrupts  0 irqcon-st ! ; ( TODO fusion )

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

.( UART control variables: )
' uart-tx-bits host.
uart-tx-bits host.
uart-tx-bits @ host.

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

( ----------------------------------------------------------- )
( Icestick board features )

: ledtog  4 + #bit outport-tog ! ;


( ----------------------------------------------------------- )
( Demo wiring below )

: delay 0 begin 1 + dup 0 = until drop ;

: isr
  r> 2 - >r
  1 ledtog
  irqcon-st @
  $2000 over and if
    tx-isr
  then
  drop
  enable-interrupts ;

: cold
  enable-interrupts
  begin
    $23 tx
  again ;

( install cold as the reset vector )
' cold  1 rshift  0 !
( install isr as the interrupt vector )
' isr  1 rshift  2 !
