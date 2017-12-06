( Bootstrap Forth, my first Forth for the CFM. )


( --------------------------------------------------------------------------- )
( Kernel code. )

( Instruction primitives that are hard to express at a higher level. )
( We directly comma literal instructions into definitions here. )
( This is obviously not portable ;-)
: +      [ $6203 , ] ;
: swap   [ $6180 , ] ;
: over   [ $6181 , ] ;
: nip    [ $6003 , ] ;
: lshift [ $6d03 , ] ;
: rshift [ $6903 , ] ;
: dup    [ $6081 , ] ;
: =      [ $6703 , ] ;
: drop   [ $6103 , ] ;
: invert [ $6600 , ] ;
: @      [ $6c00 , ] ;
: or     [ $6403 , ] ;
: and    [ $6303 , ] ;
: -      [ $6a03 , ] ;

: ! [ $6123 , $6103 , ] ;

( Access to the system variables block )
4 constant LATEST
6 constant DP
8 constant U0
10 constant STATE

$FFFF constant true
2 constant cell

: +!  swap over @ + swap ! ;
: 0= 0 = ;
: <> = 0= ;
: 2dup over over ;

: here  ( -- addr )  DP @ ;
: allot  DP +! ;
: ,  here !  cell allot ;

: >r  $6147 , ; immediate
: r>  $6b8d , ; immediate
: r@  $6b81 , ; immediate
: exit  $700c , ; immediate

: execute  ( i*x xt -- j*x )  >r ; ( NOINLINE )

: cells  1 lshift ;
: aligned  dup 1 and + ;
: compile,  1 rshift  $4000 or  , ;
: align  DP @  aligned  DP ! ;

.( value of true: )
' true execute host.

<TARGET-EVOLVE> ( make bootstrap aware of dictionary words )

( Byte access. These words access the bytes within cells in little endian. )
: c@  dup @
      swap 1 and if ( lsb set )
        8 rshift
      else
        $FF and
      then ;

.( Length of name of c@: )
LATEST @ 2 + c@ host.

: c!  dup >r
      1 and if ( lsb set )
        8 lshift
        r@ @ $FF and or
      else
        $FF and
        r@ @ $FF00 and or
      then
      r> ! ;

: c,  here c!  1 allot ;

.( c, test, should be 1 2 3 4 )
here
1 c, 2 c, 3 c, 4 c,
dup c@ host.
dup 1 + c@ host.
dup 2 + c@ host.
dup 3 + c@ host.
drop

( Compares a string to the name field of a header. )
: name= ( c-addr u nfa -- ? )
  >r  ( stash the NFA )
  r@ c@ over = if  ( lengths equal )
    r> 1 + swap   ( c-addr c-addr2 u )
    begin
      >r
      over @ over @ <> if
        r>
        drop drop drop 0 exit
      then
      1 + swap 1 +
      r> 1 -
      dup 0=
    until
    drop drop drop true
  else
    r> drop drop drop 0
  then ;

.( name= test: does its name match itself? )
LATEST @ 3 +    ( c-addr )
LATEST @ 2 + c@ ( u )
LATEST @ 2 +    ( nfa )
name= host.

( Variant of standard FIND that uses a modern string and returns the flags. )
: sfind  ( c-addr u -- c-addr u 0 | xt flags true )
  LATEST @ begin          ( c-addr u lfa )
    dup 0= if             ( c-addr u 0 )
      exit
    then
    >r  ( stash the LFA ) ( c-addr u )              ( R: lfa )
    2dup                  ( c-addr u c-addr u )     ( R: lfa )
    r@ cell +             ( c-addr u c-addr u nfa ) ( R: lfa )
    name= if              ( c-addr u )              ( R: lfa )
      nip                 ( u )                     ( R: lfa )
      r> cell +           ( u nfa )
      1 +  +  aligned     ( ffa )
      dup cell +          ( ffa cfa )
      swap @              ( cfa flags )
      true exit           ( cfa flags true )
    then    ( c-addr u ) ( R: lfa )
    r>      ( c-addr u lfa )
     @      ( c-addr u lfa' )
  again ;

.( sfind test: can it find itself ? )
LATEST @ 3 +
LATEST @ 2 + c@
sfind
host. host. host.

<TARGET-EVOLVE>  ( for sfind )

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
