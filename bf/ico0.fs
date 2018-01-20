\ vim: syntax=forth:cc=64
\ Icoboard common definitions.
\ Multi-interrupt controller
$6000 constant IRQST \ $6002 constant IRQEN 
$6004 constant IRQSE   $6006 constant IRQCE
15 constant irq#negedge  14 constant irq#m0
13 constant irq#m1        9 constant irq#rxne
\ Out port.
( $0000 constant OUT ) $0002 constant OUTSET
$0004 constant OUTCLR  $0006 constant OUTTOG
\ In port.
$2000 constant IN
\ Text mode video display
$8000 constant VTH   $8008 constant VTV
$8010 constant VPX   $8012 constant VIA
$8014 constant VFB   $8016 constant VWA
$8018 constant VWD   $801A constant VC0
---
\ Icoboard common definitions, continued.
\ UART.
$A000 constant UARTST   $A002 constant UARTRD
$A004 constant UARTTX   $A006 constant UARTRX
\ Timer-counter.
$4000 constant TIMV    $4002 constant TIMF
$4004 constant TIMM0   $4006 constant TIMM1
\ Output pin mapping.
: outpin   create #bit ,
           does> @ swap if OUTSET else OUTCLR then io! ;
1 outpin >CTS_N
---
