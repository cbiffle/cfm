\ CFM BIOS for Icoboard

\ Assembler primitives
$6020 alu: !ad            ( a b -- a b )
$6023 alu: !a             ( a b -- b )
$6081 alu: dup            ( x -- x x )
$6103 alu: drop           ( x -- )
$6123 alu: !d             ( a b -- a )
$6147 alu: >r             ( a --  R: -- a )
$6180 alu: swap           ( a b -- b a )
$6181 alu: over           ( a b -- a b a )
$6203 alu: +              ( a b -- a+b)
$6303 alu: and            ( a b -- a&b)
$6381 alu: 2dup_and       ( a b -- a b a&b )
$6403 alu: or             ( a b -- a|b)
$6503 alu: xor            ( a b -- a^b )
$6600 alu: invert         ( x -- ~x )
$6703 alu: =              ( a b -- a=b )
$6781 alu: 2dup_=         ( a b -- a b a=b )
$6803 alu: <              ( a b -- a<b )
$6903 alu: rshift         ( a b -- a>>b )
$6a03 alu: -              ( a b -- a-b)
$6b8d alu: r>             ( -- a  R: a -- )
$6c00 alu: @              ( x -- [x] )
$6d03 alu: lshift         ( a b -- a<<b )
$6e81 alu: depths         ( -- x )
$6f03 alu: u<             ( a b -- a<b )

\ I/O definitions
$8002 constant OUTSET
$8004 constant OUTCLR

$9000 constant IN

$3000 org   \ TODO precise origin TBD

\ ----------------------------------------------------------------------------
\ Utility functions

: cell+ 2 + ;

\ Checks whether any bits in the mask are true. Returns a proper (all-bits)
\ flag.
: mask>flag  ( value mask -- ? )  and 0 = invert ;

: true $FFFF ;  \ saves 1 cell per use
: ! !d drop ;   \ saves 1 cell per use

\ ----------------------------------------------------------------------------
\ System Queries


\ ----------------------------------------------------------------------------
\ "Front Panel"

\ Reads the current state of any general-purpose user-accessible switches or
\ buttons. Returns a bitmap where 1=pressed, 0=normal.
: switches   ( -- x )
  \ There is only one unused switch on this hardware, at IN[4].
  IN @  4 rshift  1 and ;

\ Update general-purpose user-visible LEDs. The argument is a bitmap controlling
\ up to 16 LEDs, where 1=lit, 0=dark.
: lights  ( x -- )
  \ There are three lights, on OUT[8:5]. We want to update them carefully so as
  \ not to disturb other bits of OUT.
  7 and  5 lshift
  OUTSET !d
  $E0 xor OUTCLR ! ;

\ ----------------------------------------------------------------------------
\ Text display

$C000 constant VTH  \ video - timing - horizontal
$C008 constant VTV  \ video - timing - vertical
$C010 constant VPX  \ video - pixel count
$C012 constant VIA  \ video - interrupt acknowledge
$C014 constant VFB  \ video - font base
$C016 constant VWA  \ video - write address
$C018 constant VWD  \ video - write data
$C01A constant VC0  \ video - character 0

variable display.geom

\ Queries the display geometry. The result is a cell with the number of text
\ columns in bits 15:8, and the number of rows in 7:0.
: display.geom@   ( -- x )
  display.geom @ ;

\ Resets the display timing to the standard mode. The contents of the display
\ and the cursor position are unspecified.
: display.reset   ( -- )
  \ Reprogram horizontal timing to show a 640-pixel window
  119 VTH !a
  cell+ 127 swap !a
  cell+ 167 swap !a
  cell+ 638 swap !
  \ Reprogram vertical timing to show a 400-pixel window
  100 VTV !a
  cell+   3 swap !a
  cell+ 122 swap !a
  cell+ 399 swap !
  \ Record geometry. $50 = #80; $19 = #25
  $5019 display.geom ! ;

\ Moves the display cursor to the given character offset from the upper-left
\ corner.
: display.cursor!   ( offset -- )
  VC0 @ +  $7FF and  VWA ! ;

\ Gets the cursor position as an offset from the upper-left corner.
: display.cursor@   ( -- offset )
  VWA @  VC0 @ -  $7FF and ;

\ Shifts the display by a character offset. 
: display.shift  ( offset -- )
  VC0 @ + VC0 ! ;

\ Types a character onto the display at the current cursor position. Advances
\ the cursor. The display is not automatically scrolled.
\ Bits 7:0 are the character.
\ Bits 11:8 are the background color.
\ Bits 15:12 are the foreground color.
: display.type  ( x -- )
  VWD ! ;

\ ----------------------------------------------------------------------------
\ Serial port.

$D000 constant UARTST
$D002 constant UARTRD
$D004 constant UARTTX
$D006 constant UARTRX

\ Sets the serial port rate divisor. Each bit cycle will consist of this many
\ core cycles.
: ser.div!  ( u -- )  UARTRD ! ;

\ Gets the serial port rate divisor.
: ser.div@  ( -- u )  UARTRD @ ;

\ Checks the serial port status.
\ Bit 0: transmitter idle
\ Bit 1: transmit holding register empty
\ Bit 2: receive holding register not empty
\ Bit 3: CTS (0=clear to send, 1=not)
: ser.status  ( -- x )
  UARTST @ 7 and
  \ TODO wire CTS to a bit in IN and read it here
  ;

\ Transmits a character on the serial port. Fails if the transmit holding
\ register is not empty.
: ser.tx  ( c -- ? )
  UARTST @ 2 and if UARTTX ! true exit then
  0 ;

\ Takes the most recently received character from the serial port's holding
\ register. If a framing error occurred, or no character is available, the
\ result will be negative. (You can distinguish these cases by checking
\ status.)
: ser.rx  ( -- x )
  UARTST @ 4 and if UARTRX @ exit then
  true ;

: irq-dispatch ;

: bios-dispatch ;

: hang hang ;
: cold
  display.reset
  hang ;

$0000 org   \ TODO

: cold-vector cold ;
: irq-vector irq-dispatch ;

: bios-vector bios-dispatch ;
