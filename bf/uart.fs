\ vim: syntax=forth:cc=64
\ UART common driver
\ The platform support layer is expected to have defined the
\ following words:
' UARTRX drop  ' UARTST drop  ' UARTTX drop  ' >CTS_N drop
---
\ The receive queue contains uart-#rx positions. It is a 
\ circular queue occupying the uart-#rx cells starting at
\ uart-rx-buf. The variables uart-rx-hd and uart-rx-tl index
\ its head and tail, respectively, mod uart-#rx.
---
\ Circular receive queue.
8 constant uart-#rx   variable uart-rx-hd   variable uart-rx-tl
create uart-rx-buf  uart-#rx cells allot
: rxq-empty?  ( -- ? )  uart-rx-hd @ uart-rx-tl @ = ;
: rxq-full?  ( -- ? )  uart-rx-hd @ uart-rx-tl @ - uart-#rx = ;
: >rxq  ( x -- )  \ enqueue or drop if full
  rxq-full? if drop exit then
  uart-rx-buf  uart-rx-hd @ [ uart-#rx 1- 2* ] literal and +  !
  2 uart-rx-hd +! ;
: rxq>  ( -- x )  \ dequeue, blocking as needed.
  begin rxq-empty? 0= until
  uart-rx-buf  uart-rx-tl @ [ uart-#rx 1- 2* ] literal and +  @
  2 uart-rx-tl +! ;
---
\ Reception, transmission.
: rx  ( -- x ) rxq> rxq-empty? 0= >CTS_N ;
: rx! rx dup 0< if rx! exit then ; \ ignores errors, retries
: uart-rx-init
  UARTRX io@ drop   \ clear pending queued character.
  irq#rxne irq-on   0 >CTS_N ;
: rx-isr
  UARTRX io@ >rxq
  1 >CTS_N ;
: tx  ( c -- )
  begin UARTST io@ 2 and until
  UARTTX io! ;

' rx-isr  vectors irq#rxne cells +  !
---
