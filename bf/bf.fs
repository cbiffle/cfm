( Bootstrap Forth, my first Forth for the CFM. )


( --------------------------------------------------------------------------- )
( Kernel code. )

( Instruction primitives that are hard to express at a higher level. )
( We directly comma literal instructions into definitions here. )
( This is obviously not portable ;-)
: +      [ $6203 , ] ;
: swap   [ $6180 , ] ;
: over   [ $6181 , ] ;
: lshift [ $6d03 , ] ;
: rshift [ $6903 , ] ;
: dup    [ $6081 , ] ;
: =      [ $6703 , ] ;
: drop   [ $6103 , ] ;
: invert [ $6600 , ] ;
: >r     [ $6147 , ] ;
: @      [ $6c00 , ] ;
: or     [ $6403 , ] ;

: ! [ $6123 , $6103 , ] ;

: execute  ( i*x xt -- j*x )  >r ; ( NOINLINE )
: 0= 0 = ;
: +!  swap over @ + swap ! ;

( Access to the system variables block )
4 constant LATEST
6 constant DP
8 constant U0
10 constant STATE

( Dictionary access )
: here  ( -- addr )  DP @ ;
2 constant cell
: cells  1 lshift ;
: allot  DP +! ;
: ,  here !  cell allot ;
: compile,  1 rshift  $4000 or  , ;

<TARGET-EVOLVE> ( make bootstrap aware of dictionary words )

( General code above )
( ----------------------------------------------------------- )
( Demo wiring below )


: #bit  1 swap lshift ;

$8006 constant outport-tog

: ledtog  4 + #bit outport-tog ! ;

: delay 0 begin 1 + dup 0 = until drop ;
: cold
  0
  begin
    dup ledtog
    1 +
    delay
    ( 4 over = if drop 0 then )
  again ;

( install cold as the reset vector )
' cold  1 rshift  0 !
