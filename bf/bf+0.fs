\ vim: cc=64:syntax=forth
\ Bootstrap Forth self-hosting version, volume one.
---
\ Assembler fundamentals.
: nip    [ $6003 asm, ] ;     : !a     [ $6023 asm, ] ;
: dup    [ $6081 asm, ] ;     : drop   [ $6103 asm, ] ; 
: !d     [ $6123 asm, ] ;     : io!d   [ $6133 asm, ] ;
: swap   [ $6180 asm, ] ;     : over   [ $6181 asm, ] ; 
: +      [ $6203 asm, ] ;     : and    [ $6303 asm, ] ; 
: or     [ $6403 asm, ] ;     : xor    [ $6503 asm, ] ; 
: invert [ $6600 asm, ] ;     : =      [ $6703 asm, ] ; 
: <      [ $6803 asm, ] ;     : rshift [ $6903 asm, ] ; 
: -      [ $6A03 asm, ] ;     : @      [ $6C00 asm, ] ; 
: io@    [ $6C10 asm, ] ;     : lshift [ $6D03 asm, ] ; 
: depths [ $6E81 asm, ] ;     : u<     [ $6F03 asm, ] ;
: (docon)  [ $6B8D asm, ] @ ;
---
\ Fundamental constants, sysvars, user area
-1 constant true  0 constant false  2 constant cell
$20 constant bl

4 constant U0        6 constant ROOTWL     8 constant DP
10 constant FREEZEP 12 constant PATCHES

: handler U0 @      ;    : STATE   U0 @  2 + ;
: 'SOURCE U0 @  4 + ;    : >IN     U0 @  8 + ;
: base    U0 @ 10 + ;    : CURRENT U0 @ 12 + ;
: CONTEXT U0 @ 14 + ;    : BLK     U0 @ 16 + ;
---
\ Basic Forth words, in terms of assembly prims.
: !  ( x a -- )  !d drop ;   : io!  ( x p -- )  io!d drop ;
: tuck  ( a b -- b a b )  swap over ;
: +!  ( x addr -- )  tuck @ + swap ! ;
: aligned  ( addr -- a-addr )  1 over and + ;
: 2dup  ( a b -- a b a b )  over over ;
: 2drop  ( a b -- )  drop drop ;
: 1+ 1 + ;    : 1- 1 - ;    : 2* 1 lshift ;    : u2/ 1 rshift ;
: cell+ cell + ;    : cells 2* ;    : cell- cell - ;
: negate 0 swap - ;     : ?dup dup if dup then ;
: 0= 0 = ;    : 0< 0 < ;
: <> = invert ;    : u> swap u< ;    : > swap < ;
: >= < 0= ;  : u>= u< 0= ;  : <= > 0= ; : u<= u> 0= ;
: 2!  !a cell+ ! ;    : bounds over + swap ;
: depth depths $FF and ;    : rdepth depths 8 rshift 1- ;
\ Whew!
---
\ Byte memory access, part 1 (no return stack yet)
: c@  ( c-addr -- c )
  dup @  swap 1 and if  8 rshift  else  $FF and  then ;
: count  ( c-addr -- c-addr' u )  dup 1+ swap c@ ;
---
\ The dictionary.
: here  ( -- addr )  DP @ ;      : allot  ( n -- )  DP +! ;
: align  here aligned DP ! ;
: freeze  ( -- addr )  here FREEZEP !d ;
: raw,  ( x -- )  here ! cell allot ;
: , raw, freeze drop ;    : -, -1 cells allot ;
: lastxt  ( -- xt )  \ Gives xt of last word on CURRENT.
  CURRENT @ @ cell+  ( nfa )  count + aligned  ( ffa )  cell+ ;
---
\ The low level assembler. TODO: factor
: asm,  ( inst -- )
  here FREEZEP @ xor if here cell - @ ( new prev )
    over $700C = if
      $F04C over and $6000 = if -, nip $100C or asm, exit
      then $E000 over and $4000 = if -, nip $1FFF and asm,
      exit then then
    over $F0FF and $6003 over = swap $6000 = or if
      over $0F00 and  dup $200 - $400 u< swap $700 = or if 
        $FFFE over and $6180 = if
          1 and + $FFF3 and dup 3 and 1 = $80 and or
            -, asm, exit then
      then then
    $6081 over = if over $6C00 = if $FF and or -, asm, exit
      then then drop
  then raw, ;
---
\ Immediate; State; Return Stack; things using it.
: immediate  true  lastxt cell-  ! ;
: [ 0 STATE ! ;  immediate       : ] 1 STATE ! ;

: r> $6B8D asm, ; immediate    : >r $6147 asm, ; immediate
: r@ $6B81 asm, ; immediate    : rdrop $600C asm, ; immediate
: exit $700C asm, ; immediate

: execute >r ;      : rot  >r swap r> swap ;
: c!  ( c c-addr -- )
  dup >r 1 and if 8 lshift $FF
             else $FF and $FF00
             then r@ @ and or r> ! ;
: c,  here c!  1 allot  freeze drop ;
---
\ Basic control structures, conditionals, simple loops
: mark<  ( -- dest )  freeze ;
: <resolve  ( dest template -- )  swap u2/ or asm, ;
: mark>  ( template -- orig )  freeze swap asm, ;
: >resolve  ( orig -- )  dup @  freeze u2/ or  swap ! ;

: if  $2000 mark> ; immediate
: then >resolve ; immediate
: else  0 mark>  swap >resolve ; immediate

: begin  mark< ;  immediate
: again  0 <resolve ; immediate
: until  $2000 <resolve ; immediate
---
\ The inlining XT compiler, and the number compiler.
: compile,  ( xt -- )
  dup @ $F04C and $700C = if    \ first inst returns
    @ $EFF3 and \ inline w/o return effect
  else
    u2/ $4000 or \ convert to a call
  then asm, ;
: literal  ( C: x -- ) ( -- x )
  dup 0< if invert true else false then
  swap $8000 or asm,  if $6600 asm, then ;  immediate
\ We can now use the bootstrap emulated POSTPONE.
---
\ More flow control: loops, semi
: while  postpone if swap ; immediate
: repeat  postpone again  postpone then ; immediate

: ;  postpone exit  postpone [ [ immediate ] ;
---
\ Stack pointer manipulation, the hard way - ewwwwwww
\ At least we have loops now.
: SP!
  1+ depth - dup 0< if
    begin ?dup if nip 1+ else exit then again
  else
    begin ?dup if dup 1- else exit then again
  then ;
: RSP!
  rdepth 1- - dup 0< if
    begin ?dup if r> rdrop >r 1+ else exit then again
  else
    begin ?dup if r@ >r 1- else exit then again
  then ;
---
\ Exceptions!
: catch
  depth >r      handler @ >r    rdepth handler !
  execute
  r> handler !  rdrop   0 ;
: throw
  ?dup if
    handler @ RSP!      r> handler !    r> swap >r
    SP! drop r>
  then ;
---
\ String comparison
: s= ( c-addr1 u1 c-addr2 u2 -- ? )
  \ Early exit if the lengths are different.
  rot over xor if drop 2drop false exit then
  ( c-addr1 c-addr2 u )
  begin
    ?dup
  while  ( c-addr1 c-addr2 u )
    >r
    over c@ over c@ xor if 2drop rdrop false exit then
    1+ swap 1+
    r> 1-
  repeat ( c-addr1 c-addr2 )
  2drop true ;
---
\ Dictionary search - within one wordlist.
: find-in  ( c-addr u wl -- c-addr u 0 | xt flags true )
  begin          ( c-addr u lfa )
    @ dup
  while
    >r  2dup  r@ cell+ count
    s= if                 ( c-addr u )              ( R: lfa )
      2drop r>            ( lfa )
      cell+ count + aligned ( ffa )
      dup cell+           ( ffa cfa )
      swap @              ( cfa flags )
      true exit           ( cfa flags true )
    then    ( c-addr u ) ( R: lfa )
    r> repeat ;     ( c-addr u lfa )
---
\ Dictionary search - in CONTEXT and CURRENT
: sfind  ( c-addr u -- c-addr u 0 | xt flags true )
  CONTEXT @ find-in if true exit then
  CURRENT @ find-in ;
---
\ Anonymous definitions and random Forth words
: :noname  align freeze ] ;
: (doquot)  r@ cell+  r> @ 2* >r ;
: [:  postpone (doquot)  0 mark> ;  immediate
: ;]  postpone exit      >resolve ;  immediate

: min  ( n1 n2 -- n ) 2dup < if drop exit else nip exit then ;
---
\ String processing
: /string   ( c-addr u n -- c-addr' u' )
  >r  r@ - swap  r> + swap ;
: sfoldl  ( c-addr u x0 xt -- x )
  >r >r bounds begin
    over over xor
  while
    dup c@ r> r@ execute >r 1+
  repeat 2drop r> rdrop ;
: skip-while  ( c-addr u xt -- c-addr' u' )
  >r begin  over c@ r@ execute over and
     while 1 /string
     repeat rdrop ;
: s,  ( c-addr u -- ) dup c,  0 [: swap c, ;] sfoldl drop
  align ;
---
\ Input processing
: SOURCE  ( -- c-addr u )  'SOURCE dup @ swap cell+ @ ;
: scan  ( pred -- c-addr u )
  SOURCE  >IN @  /string   over >r   rot skip-while
  2dup  1 min  +  'SOURCE @ -  >IN !   drop r> tuck - ;
: skip  ( pred -- )
  SOURCE  >IN @  /string             rot skip-while
  drop            'SOURCE @ -  >IN !                  ;
: parse-name ( -- c-addr u )
  [: bl u<= ;] skip   [: bl u> ;] scan ;
---
\ Defining words, vocabularies.
: >link  ( addr -- )  align here   swap dup @ ,  ! ;
: :  CURRENT @ >link  parse-name s,  0 ,  ] ;
: create    :  postpone [  [: r> ;] compile,  freeze drop ;
: constant  :  postpone [  postpone (docon)   , ;
: variable  create 0 , ;
: does>  [: r> u2/ $4000 or lastxt ! ;] compile,
         postpone r> ; immediate
variable #user  16 #user !
: user  create  #user @ ,  cell #user +!  does> @  U0 @ + ;
: vocabulary   create  PATCHES >link
                       CURRENT @ @ ,
               does> cell+ CONTEXT ! ;
: definitions  CONTEXT @ CURRENT ! ;
vocabulary forth   forth definitions
---
\ Parsing words that cannot fail.
: \  SOURCE nip >IN ! ;  immediate
: (  [: ')' <> ;] scan 2drop ;  immediate
: S" [: '"' <> ;] scan
     [: r> count over over + aligned  >r ;] compile,
     s, ;  immediate
---
\ Deferred words and tick
: defer :  [: -1 throw ;] compile,  postpone ;  ;
: defer!  ( xt dxt -- )  swap u2/ swap ! ;
: defer@  ( dxt -- xt )  @ 2* ;
defer ??
: '  parse-name dup if sfind if  drop exit  then then ?? ;
:noname 2drop -13 throw ;  ' ??  defer!
: [']  '  postpone literal ; immediate
: is  STATE @ if  postpone [']  postpone defer!
            else  '  defer!
            then ;  immediate
: action-of  STATE @ if  postpone [']  postpone defer@
                   else  '  defer@
                   then ;  immediate
---
\ Postpone, the one case where a smudge bit would be nice.
: postpone_
  parse-name dup if
    sfind if  ( xt flags )
      if compile, \ immediate
      else postpone literal postpone compile, \ normal
      then exit
    then
  then
  ?? ; immediate
8  CURRENT @ @ cell+  c!  \ fix the length
---
\ 16-bit division and modulo
: u/mod  ( num denom -- remainder quotient )
  0 0 16 begin  ( n d r q i )
    >r >r >r              ( n d ) ( R: i q r )
    over 15 rshift        ( n d n[15] ) ( R: i q r )
    r> 2* or              ( n d 2*r+n[15] )  ( R: i q )
    >r >r 2*              ( 2*n ) ( R: i q 2*r+n[15] d )
    r> r> r> 2* >r        ( 2*n d 2*r+n[15] ) ( R: i 2*q )
    2dup u<= if
      over -
      r> 1 or >r
    then
    r> r> 1- dup 0=
  until ( n d r q i ) drop >r nip nip r> ;
: u/  u/mod nip ;
: umod  u/mod drop ;
---
\ 16-bit multiplication.
: u*
  >r 0    ( a 0 ) ( R: b )
  begin
    over
  while
    r@ 1 and if over + then
    swap 2* swap
    r> u2/ >r
  repeat
  rdrop nip ;
: #bit  1 swap lshift ;
---
\ Basic terminal I/O.
defer key   :noname 0 ; is key
defer emit  :noname drop ; is emit
: space bl emit ;   : beep 7 emit ;   : cr $D emit $A emit ;
: type  0 [: swap emit ;] sfoldl drop ;
: u.
  here 16 + begin  ( u c-addr )
    1-  swap base @ u/mod  >r  ( c-addr rem ) ( R: quot )
    9 over u< 7 and + '0' +  over c!  ( c-addr ) ( R: quot )
    r> ?dup       ( c-addr quot | c-addr 0 )
  while swap      ( u' c-addr )
  repeat
  here 16 + over - type space ;
: . dup 0< if  '-' emit negate  then u. ;
: ."  postpone S"  postpone type ; immediate
:noname type  '?' emit cr  -13 throw ; is ??
---
\ ACCEPT
: accept  ( c-addr u -- u )
  >r 0 begin ( c-addr pos ) ( R: limit )
    key
    $1F over u< if  \ Printable character
      over r@ u< if   \ in bounds
        dup emit  >r over over + r>  ( c-addr pos dest c )
        swap c! 1+ 0  ( c-addr pos' 0 )
      else beep then then
    3 over = if   \ ^C - abort
      2drop 0 $D then  \ act like a CR
    8 over = if   \ Backspace
      drop dup if 8 emit  space  8 emit 1- else beep then
      0 then
    $D = until rdrop nip ;
---
\ Digit to integer conversion.
: digit  ( c -- x )
  dup '0' - 10 u<  over 'A' - 25 u<  or 0= -13 and throw
  '0' - 9 over u< 7 and -   base @ 1- over u< -13 and throw ;
---
\ Number parsing.
: number  ( c-addr u -- x )
  3 over = if   \ check for char literal
    over  dup c@ ''' =  swap 2 + c@ ''' =  and if
      drop 1+ c@ exit then then
  1 over u< if  \ check for prefix
    over c@ '-' = if  \ negative
      1 /string number negate exit then
    over c@ '#' - 2 u< if  \ number prefix
      \ Note: this exploits the fact that the decimal prefix
      \ '#' and the hex prefix '$' are adjacent numerically.
      base @ >r
      over c@ '#' - 6 u* 10 + base !  1 /string
      [ ' number ] literal catch
      r> base !  throw exit then then
  0 [: base @ u*  swap digit + ;] sfoldl ;
---
\ Interpreter for a buffer of text.
: interpret
  begin parse-name ?dup while
    sfind if ( xt flags )
      if execute
      else   STATE @ if compile, else execute then
      then
    else  \ word unknown
      2dup >r >r  \ save string
      [ ' number ] literal catch
      ?dup if r> r> ??
      else rdrop rdrop   \ discard saved string
           STATE @ if postpone literal then
      then
    then
  repeat drop ;
---
\ Text interpreter loop.
create TIB 80 allot
: quit
  0 RSP!  0 handler !  postpone [
  begin
    80 TIB 'SOURCE 2!  0 >IN !  SOURCE accept  'SOURCE cell+ !
    space ['] interpret catch ?dup if
      true over = if drop else . '!' emit then
    else
      STATE @ 0= if ." ok" then
    then cr
  again ;
---
\ Cold start routine.
variable oncold  variable ramtop
: cold
  ramtop @ #user @ cells - U0 !
  0 handler !  10 base !  forth definitions
  oncold @ ?dup if execute then
  ." bsforth | " U0 @ here - u. ." bytes free | last word: "
  CURRENT @ @ cell+ count type cr
  quit ;
---
