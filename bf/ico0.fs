\ Icoboard common definitions

\ Multi-interrupt controller
$B000 constant IRQST  ( status / enable trigger )
\ $B002 constant IRQEN  ( enable )
$B004 constant IRQSE  ( set enable )
$B006 constant IRQCE  ( clear enable )

\ Out port.
\ $8000 constant outport      ( literal value)
$8002 constant OUTSET  ( 1s set pins, 0s do nothing)
$8004 constant OUTCLR  ( 1s clear pins, 0s do nothing)
$8006 constant OUTTOG  ( 1s toggle pins, 0s do nothing)

\ In port.
$9000 constant IN

\ Text mode video display
$C000 constant VTH  \ video - timing - horizontal
$C008 constant VTV  \ video - timing - vertical
$C010 constant VPX  \ video - pixel count
$C012 constant VIA  \ video - interrupt acknowledge
$C014 constant VFB  \ video - font base
$C016 constant VWA  \ video - write address
$C018 constant VWD  \ video - write data
$C01A constant VC0  \ video - character 0

\ UART.
$D000 constant UARTST
$D002 constant UARTRD
$D004 constant UARTTX
$D006 constant UARTRX

\ Timer-counter.
$A000 constant TIMV
$A002 constant TIMF
$A004 constant TIMM0
$A006 constant TIMM1

13 constant irq#m1
14 constant irq#m0
15 constant irq#negedge
9 constant irq#rxne

\ Output pin mapping.
: outpin
  create #bit ,
  does> @ swap if OUTSET else OUTCLR then ! ;

1 outpin >CTS_N
