\ vim: cc=64:syntax=forth
\ Bootstrap Forth self-hosting version, volume one.
---
\ First, we need a way to refer to CFM machine instructions.
\ Forth words containing only a single machine instruction
\ with a fused return will be inlined (by both the bootstrapper
\ and the system we're building). As a result, such words can
\ function as mnemonics for instructions like the ones defined
\ below.

\ Note that the raw instructions are inserted without the fused
\ return. The bootstrap implementation of ; will insert it.

\ The other piece we need to start doing useful work is the
\ target code used to implement CONSTANT, (docon) . For more
\ on how CONSTANT uses (docon) , see its definition below (the
\ bootstrapper mimics it).
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
\ The bootstrapper and our Forth need to agree on the layout of
\ certain areas of memory (called the system and user vars)
\ and the value of certain constants. With our newfound
\ ability to define constants, perform arithmetic, and access
\ memory, we can introduce the definitions that make up the
\ bootstrapping ABI.
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
\ And now, a grab-bag of basic Forth words. These are defined
\ largely in terms of the primitives above, but also in terms
\ of one another.

\ There's no obvious ordering, so they're somewhat random.
\ They're almost entirely from the standard, so they're light
\ on docs.

\ Basically, this isn't the fun part, so I'm getting it over
\ with.
---
\ Basic Forth words, in terms of assembly prims and each other.
: tuck  ( a b -- b a b )  swap over ;
: 2dup  over over ;                        : 2drop  drop drop ;
: ?dup dup if dup then ;
: aligned  ( addr -- a-addr )  1 over and + ;
: 1+ 1 + ;    : 1- 1 - ;    : 2* 1 lshift ;    : u2/ 1 rshift ;
: cell+ cell + ;         : cells 2* ;          : cell- cell - ;
: !  ( x a -- )  !d drop ;       : io!  ( x p -- )  io!d drop ;
: +!  ( x addr -- )  tuck @ + swap ! ;       : 2!  !a cell+ ! ;
: negate 0 swap - ;      : 0= 0 = ;            : 0< 0 < ;
: <> = invert ;          : u> swap u< ;        : > swap < ;
: >= < 0= ;    : u>= u< 0= ;    : <= > 0= ;    : u<= u> 0= ;
: depth depths $FF and ;          : rdepth depths 8 rshift 1- ;
: min  ( n1 n2 -- n ) 2dup < if drop exit else nip exit then ;
: bounds over + swap ;
\ Whew!
---
\ The dictionary access words let us build up our Forth a cell
\ at a time. They are fairly traditional. The main exception
\ is freeze .

\ As we add machine instructions to a definition, the low level
\ assembler will look for opportunities to fuse instructions
\ together. This is usually good, but it's a bad idea if one
\ instruction is (say) before the end of a loop, and the next
\ is outside it. In cases like this we block fusion by using
\ freeze , which updates the FREEZEP value (which follows along
\ behind HERE ).
---
\ The dictionary.
: here  ( -- addr )  DP @ ;      : allot  ( n -- )  DP +! ;
: align  here aligned DP ! ;
: freeze  ( -- addr )  here FREEZEP !d ;
: raw,  ( x -- )  here ! cell allot ;
: , raw, freeze drop ;    : -, -1 cells allot ;
---
\ The low-level assembler is responsible for taking single
\ machine instructions and appending them to the definition at
\ the end of the dictionary. It applies the instruction fusion
\ optimizations.

\ It is currently very ugly.
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
\ The CFM's address space is byte-addressed, but the hardware
\ only provides cell-sized memory operations. If we want to
\ load and store bytes, we have to implement it ourselves.
---
\ Byte-sized memory access.
: c@  ( c-addr -- c )
  dup @  swap 1 and if  8 rshift  else  $FF and  then ;
: c!  ( c c-addr -- )
  1 over and if swap  8 lshift  over @ $FF and or  swap ! exit
           then swap $FF and over @ $FF00 and or swap ! ;
: c,  here c!  1 allot  freeze drop ;
: count  ( c-addr -- c-addr' u )  dup 1+ swap c@ ;
---
\ We now have enough ability to manipulate the dictionary that
\ we can extend our simple assembler with macros -- that is,
\ IMMEDIATE words. IMMEDIATE words execute at compile time.
\ On CFM we need them to compile any references to the return
\ stack.

\ At this stage, we can also define [ and ] , which shadow the
\ bootstrap emulated versions instantly upon definition.
---
\ Immediate; State; Return Stack; things using it.
: lastxt  ( -- xt )  \ Gives xt of last word on CURRENT.
  CURRENT @ @ cell+  ( nfa )  count + aligned  ( ffa )  cell+ ;
: immediate  true  lastxt cell-  ! ;
: [ 0 STATE ! ;  immediate       : ] 1 STATE ! ;

: r> $6B8D asm, ; immediate    : >r $6147 asm, ; immediate
: r@ $6B81 asm, ; immediate    : rdrop $600C asm, ; immediate
: exit $700C asm, ; immediate

: execute >r ;      : rot  >r swap r> swap ;
---
\ With the ability to define IMMEDIATE words, we can start
\ adding syntax. We'll begin with IF ELSE THEN and simple
\ BEGIN UNTIL / AGAIN loops. We've been relying on bootstrap
\ emulation of IF ELSE THEN so far to access the CFM's
\ conditional branch instruction, but the bootstrapper hasn't
\ given us any loop primitives (other than tail recursion), so
\ this will really level up our expressive powers.
---
\ Basic control structures, conditionals, simple loops
\ 'template' is a branch instruction with zero operand.
: mark>  ( template -- orig )  freeze swap asm, ;
: >resolve  ( orig -- )  dup @  freeze u2/ or  swap ! ;
  \ > here indicates forward control flow edges.
\ $2000 = 0branch, $0000 = unconditional branch
: if  $2000 mark> ; immediate
: then >resolve ; immediate
: else  $0000 mark>  swap >resolve ; immediate

: mark<  ( -- dest )  freeze ;
: <resolve  ( dest template -- )  swap u2/ or asm, ;
  \ < here indicates backward control flow edges.
: begin  mark< ;  immediate
: again  $0000 <resolve ; immediate
: until  $2000 <resolve ; immediate
---
\ Most Forth flow control words can be defined in terms of what
\ we have now. In particular, we can add WHILE and REPEAT .
\ These use the bootstrap emulated version of POSTPONE since
\ we haven't got our own yet.

\ While we're POSTPONE-ing things to effect control flow, let's
\ define ; . This is tricky, because target definitions shadow
\ bootstrap definitions *immediately* to allow tail recursion,
\ and the definition of ; needs to use ; to end its definition.
\ (Got it?)

\ This only requires one trick, which is setting ; IMMEDIATE
\ from *within* the definition. This causes the compiler to
\ call the unfinished definition to *finish compiling itself.*
\ Tying the knot in the Forth compiler.
---
\ More flow control: loops, semi
: while  postpone if swap ; immediate
: repeat  postpone again  postpone then ; immediate

: ;  [ immediate ]  postpone exit  postpone [  ;
---
\ Now that we have loops, we can use them to mess with the
\ stacks. The current CFM doesn't have instructions to write
\ the stack pointers (though I am very much considering it).
\ Thus to move the stacks to a particular level, on abort or
\ throw, we have to do it the hard way.

\ With the ability to set the stack pointers, we can implement
\ ANS-style exceptions, which we will need to do basically
\ anything interesting in the interpreter. Exceptions work by
\ recording state to be restored on the return stack, and then
\ recording the return stack level in the user var HANDLER .
\ Our code matches the ANS reference code nearly exactly.
---
\ Stack pointer manipulation (ewwww) and exceptions (yay!)
: SP!  ( u -- it's complicated )
  1+ depth - dup 0< if   begin ?dup while nip 1+ repeat
  else   begin ?dup while dup 1- repeat then ;
: RSP!  ( u -- )  ( R: it's complicated )
  rdepth 1- - dup 0< if  begin ?dup while r> rdrop >r 1+ repeat
  else  begin ?dup while r@ >r 1- repeat then ;
: catch  ( i*x xt -- j*x 0 | i*x n )
  depth >r      handler @ >r    rdepth handler !
  execute
  r> handler !  rdrop   0 ;
: throw  ( i*x n -- i*x | j*x n )
  ?dup if
    handler @ RSP!      r> handler !    r> swap >r
    SP! drop r>
  then ;
---
\ Okay, back to the compiler. The words COMPILE, and LITERAL
\ form the low-level compiler (cf low-level assembler).

\ COMPILE, takes an XT and assembles machine code to call it at
\ the end of the current definition. This is where inlining of
\ single-instruction definitions occurs.

\ LITERAL takes a number and assembles machine code to push it.
\ Because CFM can only push 15-bit constants, if the number has
\ its high bit set, it requires two machine instructions.

\ With these definitions we can now use the bootstrap POSTPONE
\ on non-IMMEDIATE words, a feature we will use shortly.
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
---
\ With our new POSTPONE powers we can add support for
\ anonymous definitions. We use these pretty extensively.

\ :NONAME is a standard word that defines an anonymous word; at
\ the terminating ; its XT is left on the stack.  (Typically,
\ you'd do something with it right away.)

\ [: and ;] are similar to :NONAME and ; , but define a
\ *nested* anonymous word called a quotation. When the
\ surrounding word is executed, the quotation code is skipped
\ and its XT left on the stack. (The syntax is patterned after
\ gforth.)
---
\ Anonymous definitions.
: :noname  align freeze ] ;
: [:  $4000 mark> ;  immediate
: ;]  postpone exit   >resolve  postpone r> ;  immediate
---
\ Most string processing in this Forth is functional: it chews
\ on strings using user-provided words. We can now start
\ implementing it.

\ SFOLDL applies a two-argument word to each character of a
\ string, and the result of the last application. This may be
\ easiest to understand by looking at uses below. (The use in
\ S, is arguably an abuse, so keep reading.)

\ SKIP-WHILE drops characters from the front of a string while
\ a predicate -- a word with shape (c -- ?) -- returns true.
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
\ Time to apply our string manipulation to the input source, so
\ we can start processing some code. We break input into tokens
\ using a pair of words, SCAN and SKIP .

\ SCAN returns the prefix of the remaining input such that all
\ characters match a predicate. If it doesn't reach the end of
\ input, it consumes the first character that doesn't match the
\ predicate (the delimiter).

\ SKIP discards input characters while they match a predicate.

\ With these, expressing PARSE-NAME -- which collects a Forth
\ name from the input, skipping leading whitespace -- is easy.
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
\ With the ability to parse input, we can define the words that
\ produce non-anonymous definitions. These will replace the
\ bootstrap versions as they are defined.

\ Because this is a native-code Forth, : is the most primitive
\ of the defining words. The others are defined in terms of it.
\ But because : has effects on STATE, the other words need to
\ postpone [ to switch the compiler back off.

\ This dense screen is essentially the rest of the compiler
\ back-end. It shadows the rest of the bootstrap defining words
\ and adds new powers like VARIABLE , DOES> , and VOCABULARY .
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
\ Let's add the rest of the Forth syntax words that we can,
\ given that we haven't implemented dictionary search. This
\ means words that collect input without interpreting it:
\ comments and string literals.
---
\ Parsing words that don't involve dictionary lookups.
: \  BLK @ if  >IN @ 63 + 63 invert and >IN !
     else SOURCE nip >IN !
     then ;  immediate
: (  [: ')' <> ;] scan 2drop ;  immediate
: S" [: '"' <> ;] scan
     [: r> count over over + aligned  >r ;] compile,
     s, ;  immediate
---
\ This is about as far as we can get without implementing
\ dictionary search, so let's do that.

\ The first step is defining how to compare two strings for
\ equivalence. Should I break down and make word lookup be
\ case-insensitive, this is where it would happen.
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
\ Words in the dictionary are each linked into a single list.
\ There can be multiple wordlists woven together through the
\ dictionary. The primitive action when searching for a word
\ is to search a single wordlist.
---
\ Dictionary search - within one wordlist.
: find-in  ( c-addr u wl -- c-addr u 0 | xt flags true )
  begin          ( c-addr u lfa )
    @ dup
  while
    >r  2dup  r@ cell+ count
    s= if                 ( c-addr u )       ( R: lfa )
      2drop r>            ( lfa )
      cell+ count + aligned ( ffa )
      dup cell+           ( ffa cfa )
      swap @              ( cfa flags )
      true exit           ( cfa flags true )
    then    ( c-addr u ) ( R: lfa )
    r> repeat ;     ( c-addr u lfa )
---
\ This Forth uses a FIG-Forth-style vocabulary system, where
\ there are two wordlists in scope, called CONTEXT and
\ CURRENT. When searching the dictionary, we search CONTEXT
\ first, then CURRENT.

\ SFIND implements this algorithm. (It would be called FIND,
\ but the standard includes a word called FIND that is subtly
\ different -- one of the reasons I am not attempting ANS
\ conformance.)
---
\ Dictionary search - in CONTEXT and CURRENT
: sfind  ( c-addr u -- c-addr u 0 | xt flags true )
  CONTEXT @ find-in if true exit then
  CURRENT @ find-in ;
---
\ We're about to start dealing in words that search the
\ dictionary and report errors if they can't find something.
\ This means we need a way of reporting errors. But so far we
\ haven't assumed there's a terminal, or defined any way of
\ interacting with it. This makes it hard to report which word
\ was not found.

\ Deferred words to the rescue. We'll defer the definition of
\ ?? until later, when we can do proper error reporting. But
\ we can start using it immediately, since its default
\ behavior will be to throw.

\ Using this, we can implement ' , and using that, we can
\ finish the implementation of deferred words.
---
\ Deferred words and tick
: defer :  [: -1 throw ;] compile,  postpone ;  ;
: defer!  ( xt dxt -- )  swap u2/ swap ! ;
: defer@  ( dxt -- xt )  @ 2* ;

defer ??

: '  parse-name dup if sfind if  drop exit  then then ?? ;
: [']  '  postpone literal ; immediate

: is  STATE @ if  postpone [']  postpone defer!
            else  '  defer!
            then ;  immediate
: action-of  STATE @ if  postpone [']  postpone defer@
                   else  '  defer@
                   then ;  immediate
---
\ We can also implement POSTPONE at this point. This is a bit
\ tricky, because the obvious way to implement POSTPONE is in
\ terms of POSTPONE -- but unlike with ; it doesn't call itself
\ in tail position. So we need a way to keep the definition
\ from recursing, and keep the bootstrap version in view.

\ (This is the one case where having a smudge bit like a
\ traditional Forth would be super handy.)

\ We define a word called POSTPONE_ and then shorten its length
\ by one character leaving POSTPONE. The underscore winds up
\ in the unused padding after the name.
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
\ We're about to need division and multiplication to do numeric
\ formatting, so this seems as good a time as any to define
\ them.
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
\ 16-bit multiplication and #bit
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
\ Time for terminal output. This will let us resolve the word
\ ?? we deferred earlier, though we'll defer more words so
\ the platform can define how to actually interact with the
\ terminal. We assume a vaguely ANSI-style terminal with bell
\ support, a decision that is proving awkward and is probably
\ worth revisiting later.
---
\ Basic terminal output.
defer emit  ( c -- )
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
\ And here's the real definition of ??
:noname type  '?' emit cr  -13 throw ; is ??
---
\ Terminal input, in the form of KEY and ACCEPT.

\ KEY reads a character from the console. ACCEPT is a line
\ editor.
---
\ ACCEPT
defer key
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
\ Converting digits to integers. Because of the way this is
\ used, when digit conversion fails, it throws an unknown
\ word exception.
---
\ Digit to integer conversion.
: digit  ( c -- x )
  dup '0' - 10 u<  over 'A' - 25 u<  or 0= -13 and throw
  '0' - 9 over u< 7 and -   base @ 1- over u< -13 and throw ;
---
\ Generalized numeric conversion. This accepts prefixes for
\ hex ($) and decimal (#), and character literals.
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
\ INTERPRET takes a buffer of text (in SOURCE ) and, well,
\ interprets it. This is the core of the high-level source
\ interpreter.
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
\ QUIT wraps INTERPRET with error handling, prompt, and input
\ source control. It also rewinds the return stack, making it
\ a useful way to return *with the data stack contents* from
\ some deeply nested point in a program.
---
\ Text interpreter loop.
create TIB 80 allot
: quit
  0 RSP!  0 handler !  postpone [  0 BLK !
  begin
    80 TIB 'SOURCE 2!  0 >IN !  SOURCE accept  'SOURCE cell+ !
    space ['] interpret catch ?dup if
      true over = if drop else . '!' emit then
    else
      STATE @ 0= if ." ok" then
    then cr
  again ;
---
\ And now, the cold-boot startup routine! This sets up the user
\ vars, prints a banner, and runs the interpreter.

\ To adapt this to a particular board, you must implement the
\ deferred word oncold .
---
\ Cold start routine.
defer oncold  variable ramtop
: cold
  ramtop @ #user @ cells - U0 !
  0 handler !  10 base !  forth definitions  0 BLK !
  oncold
  ." bsforth | " U0 @ here - u. ." bytes free | last word: "
  CURRENT @ @ cell+ count type cr
  quit ;
---
