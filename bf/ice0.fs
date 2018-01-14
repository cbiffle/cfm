\ Icestick common definitions

\ Multi-interrupt controller
$E000 constant IRQST  ( status / enable trigger )
\ $E002 constant IRQEN  ( enable )
$E004 constant IRQSE  ( set enable )
$E006 constant IRQCE  ( clear enable )

\ Out port.
\ $8000 constant outport      ( literal value)
$8002 constant OUTSET  ( 1s set pins, 0s do nothing)
$8004 constant OUTCLR  ( 1s clear pins, 0s do nothing)
$8006 constant OUTTOG  ( 1s toggle pins, 0s do nothing)

\ In port.
$A000 constant IN

\ UART.
$C000 constant UARTST
$C002 constant UARTRD
$C004 constant UARTTX
$C006 constant UARTRX

\ Interrupt assignments.
14 constant irq#rxne
\ 15 constant irq#negedge   (unused)

\ Output pin mapping.
: >CTS_N if OUTSET else OUTCLR then 1 swap io! ;
