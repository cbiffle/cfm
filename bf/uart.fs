\ UART common driver

\ ---------------------------------------------------------------------
\ Reception.

\ Receive queue. The receive queue contains uart-#rx positions. It is a
\ circular queue occupying the uart-#rx cells starting at uart-rx-buf.
\ The variables uart-rx-hd and uart-rx-tl index its head and tail,
\ respectively, mod uart-#rx.
8 constant uart-#rx
create uart-rx-buf  uart-#rx cells allot
variable uart-rx-hd
variable uart-rx-tl

\ Queries whether the receive queue is empty.
: rxq-empty?  ( -- ? )  uart-rx-hd @ uart-rx-tl @ = ;
\ Queries whether the receive queue is full.
: rxq-full?  ( -- ? )  uart-rx-hd @ uart-rx-tl @ - uart-#rx = ;

\ Inserts a cell into the receive queue. This is intended to be called
\ from an ISR, so if the queue is full, it just drops data. Flow control
\ signaling should prevent this in practice.
: >rxq
  rxq-full? if drop exit then

  uart-rx-buf  uart-rx-hd @ [ uart-#rx 1- 2* ] literal and +  !
  2 uart-rx-hd +! ;

\ Takes a cell from the receive queue. If the queue is empty, spin.
: rxq>
  begin rxq-empty? 0= until
  uart-rx-buf  uart-rx-tl @ [ uart-#rx 1- 2* ] literal and +  @
  2 uart-rx-tl +! ;

\ Receives a byte from RX. In the event of an error (e.g. framing) the
\ sign bit is set.
: rx  ( -- x )
  rxq>
  rxq-empty? 0= >CTS_N
    \ allow sender to resume if we've emptied the queue.
  ;

\ Receives a byte from RX, discarding any errors and retrying.
: rx! rx dup 0< if rx! exit then ;

\ Interrupt handler, registered to receive the UART's RXNE interrupt.
: rx-isr
  UARTRX io@ >rxq
  1 >CTS_N ;
' rx-isr vectors irq#rxne cells + !

: uart-rx-init
  \ Clear any pending queued character.
  UARTRX io@ drop
  \ Enable the IRQ.
  irq#rxne irq-on
  0 >CTS_N ;

\ ---------------------------------------------------------------------
\ Transmission.

: tx
  \ Wait for transmitter to be free
  begin UARTST io@ 2 and until
  UARTTX io! ;
