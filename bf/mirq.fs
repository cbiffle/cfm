\ Multi-IRQ controller support

\ Prior to sourcing this file, the following symbols must be defined:
' IRQST drop  \ status/enable register
' IRQSE drop  \ interrupt set enable register
' IRQCE drop  \ interrupt clear enable register

\ Atomically enables interrupts and returns. This is intended to be tail
\ called from the end of an ISR to effect a return-from-interrupt, but
\ can be used in other places.
: ei  IRQST io!d ;

\ Disable an interrupt by index.
: irq-off  ( u -- )  #bit IRQCE io! ;
\ Enable an interrupt by index.
: irq-on   ( u -- )  #bit IRQSE io! ;

\ Per-source vector table
create vectors  16 cells allot

\ General interrupt dispatcher. Chews on the IRQ controller status
\ word to figure out which vectors to call.
: isr
  \ Vectored interrupt dispatcher.
  \ Which interrupts are active?
  15 IRQST io@
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

\ Record it as hardware interrupt vector.
' isr  u2/  2 !
