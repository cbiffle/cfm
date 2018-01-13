\ Icestick support code

: #bit  1 swap lshift ;

( ----------------------------------------------------------- )
( Interrupt Controller )

$E000 constant IRQST  ( status / enable trigger )
\ $E002 constant IRQEN  ( enable )
$E004 constant IRQSE  ( set enable )
$E006 constant IRQCE  ( clear enable )

( Atomically enables interrupts and returns. This is intended to be tail )
( called from the end of an ISR. )
: ei  IRQST !d ;

: irq-off  ( u -- )  #bit IRQCE ! ;
: irq-on   ( u -- )  #bit IRQSE ! ;

15 constant irq#negedge

( ----------------------------------------------------------- )
( I/O ports )

\ $8000 constant outport      ( literal value)
$8002 constant OUTSET  ( 1s set pins, 0s do nothing)
$8004 constant OUTCLR  ( 1s clear pins, 0s do nothing)
$8006 constant OUTTOG  ( 1s toggle pins, 0s do nothing)

$A000 constant IN

( ----------------------------------------------------------- )
( UART receive queue and flow control )

8 constant uart-#rx
variable uart-rx-buf  uart-#rx 1- cells allot
variable uart-rx-hd
variable uart-rx-tl

: >CTS_N if OUTSET else OUTCLR then 1 swap ! ;

: rxq-empty? uart-rx-hd @ uart-rx-tl @ = ;
: rxq-full? uart-rx-hd @ uart-rx-tl @ - uart-#rx = ;

( Inserts a cell into the receive queue. This is intended to be called from )
( interrupt context, so if it encounters a queue overrun, it simply drops )
( data. )
: >rxq
  rxq-full? if drop exit then

  uart-rx-buf  uart-rx-hd @ [ uart-#rx 1- 2* ] literal and +  !
  2 uart-rx-hd +! ;

( Takes a cell from the receive queue. If the queue is empty, spin. )
: rxq>
  begin rxq-empty? 0= until
  uart-rx-buf  uart-rx-tl @ [ uart-#rx 1- 2* ] literal and +  @
  2 uart-rx-tl +! ;

\ Receives a byte from RX. In the event of an error (e.g. framing) the sign bit
\ is set.
: rx  ( -- x )
  rxq>
  rxq-empty? 0= >CTS_N
    \ allow sender to resume if we've emptied the queue.
  ;

( ----------------------------------------------------------- )
( Hard UART )

$C000 constant UARTST
$C002 constant UARTRD
$C004 constant UARTTX
$C006 constant UARTRX

14 constant irq#rxne

: tx
  \ Wait for transmitter to be free
  begin UARTST @ 2 and until
  UARTTX ! ;

: rx-isr
  UARTRX @ >rxq
  1 >CTS_N ;

: uart-rx-init
  \ Clear any pending queued character.
  UARTRX @ drop
  \ Enable the IRQ.
  irq#rxne irq-on
  0 >CTS_N ;

( ----------------------------------------------------------- )
( Icestick board features )

: ledtog  1 + #bit OUTTOG ! ;

( ----------------------------------------------------------- )
( Demo wiring below )

create vectors  16 cells allot

: isr
  \ Vectored interrupt dispatcher.
  \ Which interrupts are active?
  15 IRQST @
  begin   ( vector# irqst )
    dup \ while any remain
  while
    $8000 over and if
      over cells  vectors + @ execute
    then
    1 lshift
    swap 1- swap
  repeat
  2drop
  \ Patch up interrupt return address.
  r> 2 - >r
  \ Atomically enable interrupts and return.
  ei ;

\ Vector table
' rx-isr vectors irq#rxne cells + !

create TIB 80 allot

: rx! rx dup 0< if rx! exit then ;

: quit
  0 RSP!
  0 handler !
  postpone [
  begin
    TIB 'SOURCE !
    80  'SOURCE cell+ !
    0 >IN !
    SOURCE accept  'SOURCE cell+ !
    space
    ['] interpret catch
    ?dup if
      true over = if
        \ abort isn't supposed to print
        drop
      else 
        . '!' emit
      then
    else
      STATE @ 0= if
        ." ok"
      then
    then
    cr
  again ;

: cold
  \ Initialize user area
  $1C00  #user @ cells -  U0 !
  0 handler !
  10 base !
  forth definitions

  uart-rx-init
  347 UARTRD ! \ Set baud rate to 115200

  ['] tx 'emit !
  ['] rx! 'key !
  ei
  ." bsforth | "
  U0 @ here - . ." bytes free | last word: "
  CURRENT @ @ cell+ count type cr
  quit ;

( install cold as the reset vector )
' cold  u2/  0 !
( install isr as the interrupt vector )

' isr  u2/  2 !

.( Compilation complete. HERE is... )
here host.
