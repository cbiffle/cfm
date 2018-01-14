\ Icoboard common definitions

\ Multi-interrupt controller
$6000 constant IRQST  ( status / enable trigger )
\ $6002 constant IRQEN  ( enable )
$6004 constant IRQSE  ( set enable )
$6006 constant IRQCE  ( clear enable )

\ Out port.
\ $0000 constant outport      ( literal value)
$0002 constant OUTSET  ( 1s set pins, 0s do nothing)
$0004 constant OUTCLR  ( 1s clear pins, 0s do nothing)
$0006 constant OUTTOG  ( 1s toggle pins, 0s do nothing)

\ In port.
$2000 constant IN

\ Text mode video display
$8000 constant VTH  \ video - timing - horizontal
$8008 constant VTV  \ video - timing - vertical
$8010 constant VPX  \ video - pixel count
$8012 constant VIA  \ video - interrupt acknowledge
$8014 constant VFB  \ video - font base
$8016 constant VWA  \ video - write address
$8018 constant VWD  \ video - write data
$801A constant VC0  \ video - character 0

\ UART.
$A000 constant UARTST
$A002 constant UARTRD
$A004 constant UARTTX
$A006 constant UARTRX

\ Timer-counter.
$4000 constant TIMV
$4002 constant TIMF
$4004 constant TIMM0
$4006 constant TIMM1

13 constant irq#m1
14 constant irq#m0
15 constant irq#negedge
9 constant irq#rxne

\ Output pin mapping.
: outpin
  create #bit ,
  does> @ swap if OUTSET else OUTCLR then io! ;

1 outpin >CTS_N
