\ Icestick common definitions

\ Multi-interrupt controller
$C000 constant IRQST  ( status / enable trigger )
$C002 constant IRQEN  ( enable )
$C004 constant IRQSE  ( set enable )
$C006 constant IRQCE  ( clear enable )

\ Out port.
\ $0000 constant outport      ( literal value)
$0002 constant OUTSET  ( 1s set pins, 0s do nothing)
$0004 constant OUTCLR  ( 1s clear pins, 0s do nothing)
$0006 constant OUTTOG  ( 1s toggle pins, 0s do nothing)

\ In port.
$4000 constant IN

\ UART.
$8000 constant UARTST
$8002 constant UARTRD
$8004 constant UARTTX
$8006 constant UARTRX

\ Interrupt assignments.
14 constant irq#rxne
\ 15 constant irq#negedge   (unused)

\ Output pin mapping.
: >CTS_N if OUTSET else OUTCLR then 1 swap io! ;
