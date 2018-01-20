\ vim: syntax=forth:cc=64
\ Multi-IRQ controller support
\ The platform support layer is expected to have defined the
\ following words:
' IRQST drop  ' IRQSE drop  ' IRQCE drop
\ Enable interrupts. Serves as atomic return-from-ISR when used
\ in tail position.
: ei  IRQST io!d ;
: irq-off  ( u -- )  #bit IRQCE io! ; \ Disable IRQ by index.
: irq-on   ( u -- )  #bit IRQSE io! ; \ Enable IRQ by index.
---
\ Multi-IRQ controller: software vectoring
create vectors  16 cells allot
: isr \ Vectored interrupt dispatcher.
  15 IRQST io@  \ Which interrupts are active?
  begin   ( vector# irqst )
    dup \ while any remain
  while
    $8000 over and if
      over cells  vectors + @ execute
    then
    1 lshift   swap 1- swap
  repeat 2drop
  \ Patch up interrupt return address and return-from-ISR.
  r> 2 - >r ei ;
' isr  u2/  2 !
---
