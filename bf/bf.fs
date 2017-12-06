( Bootstrap Forth, my first Forth for the CFM. )

( Instruction primitives that are hard to express at a higher level. )
( We directly comma literal instructions into definitions here. )
( This is obviously not portable ;-)
: +      [ $6203 , ] ;
: swap   [ $6180 , ] ;
: lshift [ $6d03 , ] ;
: rshift [ $6903 , ] ;
: dup    [ $6081 , ] ;
: =      [ $6703 , ] ;
: drop   [ $6103 , ] ;

: ! [ $6123 , $6103 , ] ;

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
