\ Icestick support code

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

\ Vector table
' rx-isr vectors irq#rxne cells + !

: rx! rx dup 0< if rx! exit then ;

:noname
  uart-rx-init
  347 UARTRD ! \ Set baud rate to 115200

  ['] tx 'emit !
  ['] rx! 'key !
  ei ;
oncold !

$1C00 ramtop !

( install cold as the reset vector )
' cold  u2/  0 !

.( Compilation complete. HERE is... )
here host.
