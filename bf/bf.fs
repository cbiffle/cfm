( Bootstrap Forth, my first Forth for the CFM. )


\ -----------------------------------------------------------------------------
\ Instruction-level machine-code primitives.

\ The ALU instructions are written without a fused return for clarity, but the
\ effect of ; will fuse a return into the final instruction. The result is a
\ definition containing a single returning instruction, which will be noticed
\ by the inlining algorithm. As a result, these definitions function as an
\ assembler.

\ Instructions that map to traditional Forth words:
: +      [ $6203 asm, ] ;
: swap   [ $6180 asm, ] ;
: over   [ $6181 asm, ] ;
: nip    [ $6003 asm, ] ;
: lshift [ $6d03 asm, ] ;
: rshift [ $6903 asm, ] ;
: dup    [ $6081 asm, ] ;
: =      [ $6703 asm, ] ;
: drop   [ $6103 asm, ] ;
: invert [ $6600 asm, ] ;
: @      [ $6c00 asm, ] ;
: or     [ $6403 asm, ] ;
: and    [ $6303 asm, ] ;
: xor    [ $6503 asm, ] ;
: -      [ $6a03 asm, ] ;
: <      [ $6803 asm, ] ;
: u<     [ $6f03 asm, ] ;


\ Odd CFM instructions:

\ Stores x at addr, leaving x (the data, or d) on the stack.
: !d  ( x addr -- x )  [ $6123 asm, ] ;

\ Pushes a word containing the depth of the parameter stack in bits 7:0, and
\ the depth of the return stack in bits 15:8.
: depths  ( -- x )  [ $6E81 asm, ] ;

\ -----------------------------------------------------------------------------
\ Support for CONSTANT. CONSTANT is implemented as if written with DOES>, but
\ we need to start slinging constants before we have DOES> (or CREATE or : for
\ that matter) so we must roll it by hand.

\ A word created with CONSTANT will call (docon) as its only instruction.
\ Immediately following the call is a cell containing the value of the
\ constant.  Thus, (docon) must consume its return address and load the cell.

\ We're working without a definition for R> here, because we're going to write
\ an optimizing assembler before writing R> .

: (docon)  ( -- x ) ( R: addr -- )
  [ $6b8d asm, ]  ( machine code for R> )
  @ ;

\ -----------------------------------------------------------------------------
\ Useful CONSTANTs.

\ System variables. These memory locations are wired into the bootstrap
\ program.
4 constant U0  ( address of user area )
6 constant LATEST  ( head of wordlist )
8 constant DP
10 constant FREEZEP

: handler U0 @ ;
: STATE U0 @ 2 + ;
: 'SOURCE U0 @ 4 + ;
: >IN U0 @ 8 + ;
: base U0 @ 10 + ;
: CURRENT U0 @ 12 + ;

$FFFF constant true  ( also abused as -1 below, since it's cheaper )
0 constant false
2 constant cell


\ -----------------------------------------------------------------------------
\ More useful Forth words.

: tuck  ( a b -- b a b )  swap over ;
: !  ( x addr -- )  !d drop ;
: +!  ( x addr -- )  tuck @ + swap ! ;
: aligned  ( addr -- a-addr )  1 over and + ;

: c@  ( c-addr -- c )
  dup @
  swap 1 and if ( lsb set )
    8 rshift
  else
    $FF and
  then ;

: 2dup over over ;
: 2drop drop drop ;
: 1+ 1 + ;
: 1- 1 - ;
: 2* 1 lshift ;
: cell+ cell + ;
: u2/ 1 rshift ;
: ?dup dup if dup then ;
: negate 0 swap - ;
: 0= 0 = ;
: 0< 0 < ;
: <> = invert ;
: u> swap u< ;

: depth  depths $FF and ;
: rdepth  depths 8 rshift 1- ;

\ -----------------------------------------------------------------------------
\ The Dictionary and the Optimizing Assembler.

\ Because the host manipulates the dictionary, it's important to keep the
\ layout consistent between us and the host. This is why LATEST, DP, and
\ FREEZEP are part of the system variables block.

: here  ( -- addr )  DP @ ;
: allot  DP +! ;
: raw,  here !  cell allot ;
: cells  2* ;
: align  here  aligned  DP ! ;

\ Access to the CFA/xt of the most recently defined word.
: lastxt  ( -- xt )
  LATEST @  cell+      ( nfa )
  dup c@ + 1+ aligned   ( ffa )
  cell+ ;

\ "Un-comma" rewinds HERE by 1 cell. It's an implementation factor of asm, .
: -,  ( -- )  true cells allot ;

\ We've been calling the host's emulation of asm, for building words out of
\ machine code. Here's the actual definition.
: asm,
  here FREEZEP @ xor if  ( Fusion is a possibility... )
    here cell - @   ( new-inst prev-inst )

    over $700C = if ( if we're assembling a bare return instruction... )
      $F04C over and $6000 = if  ( ...on a non-returning ALU instruction )
        -,
        nip  $100C or  asm, exit
      then
      $E000 over and $4000 = if  ( ...on a call )
        -,
        nip $1FFF and  asm, exit
      then
    then

    over $F0FF and  ( new-inst prev-inst masked )
        $6003 over = ( new-inst prev-inst masked =destr? )
        swap $6000 = ( new-inst prev-inst =destr? =nd? )
        or if \ adding a simple ALU op, destructive or not
      ( new-inst prev-inst )
      over $0F00 and  dup $200 - $400 u< swap $700 = or if  \ commutes
        $FFFE over and $6180 = if  \ swap or over, Dadj=0 or 1
          \ Add the two-bit Dadj field of the two instructions.
          \ We know the swap/over Dadj field is zero or 1 from the test above.
          \ We know the ALU op's Dadj is 0 or -1 from the entry test.
          \ We know that bit 2 (in Radj) is zero. So we can add the two-bit
          \ fields by allowing overflow into Radj and then clearing it.
          1 and + $FFF3 and
          dup 3 and 1 = $80 and or \ Set TN if Dadj > 0
          -,
          asm, exit
        then
      then
    then

    $6081 over = if   \ previous instruction is DUP
      over $6C00 = if   \ just @ for now, others aren't used
        $FF and or
        -,
        asm, exit
      then
    then

    ( No patterns matched. )
    drop
  then
  ( Fusion was not possible, simply append the bits. )
  raw, ;

<TARGET-ASM>

\ Sometimes we want a clear separation between one instruction and the next.
\ For example, if the second instruction is the target of control flow like a
\ loop or if. The word freeze updates FREEZEP, preventing fusion of any
\ instructions already present in the dictionary. It returns the value of here,
\ because we basically always want that when using freeze.
: freeze  ( -- addr )
  here FREEZEP !d ;

\ Encloses a data cell in the dictionary. Prevents misinterpretation of the
\ data as instructions by using freeze . Thus using , to assemble machine
\ instructions will *work* but the results will have poor performance.
: ,  ( x -- )  raw, freeze drop ;

\ -----------------------------------------------------------------------------
\ Aside: IMMEDIATE and STATE manipulation.

\ Sets the flags on the most recent definition.
: immediate
  lastxt cell -
  true swap ! ;

\ Switches from compilation to interpretation.
: [ 0 STATE ! ; immediate
\ Switches from interpretation to compilation.
: ] 1 STATE ! ;

\ -----------------------------------------------------------------------------
\ Forth return stack words. These are machine-language primitives like we have
\ above, but since they affect the return stack, they (1) must be inlined at
\ their site of use, and (2) cannot be automatically inlined by the compiler,
\ because that would change the meaning of the code. Thus these are our first
\ IMMEDIATE definitions as they have side effects on the current definition.

\ It would be reasonable to describe this as the start of the compiler.

: r>  $6b8d asm, ; immediate
: >r  $6147 asm, ; immediate
: r@  $6b81 asm, ; immediate
: rdrop $600C asm, ; immediate
: exit  $700c asm, ; immediate

\ -----------------------------------------------------------------------------
\ Support for VARIABLE .

\ Like CONSTANT, words created with VARIABLE will call (dovar) as their only
\ instruction, thus handing us the address of the actual variable on the return
\ stack.

\ Because we don't need VARIABLE until much later in bootstrap, we were able to
\ wait until now to write its code fragment, where we have R> available.

: (dovar) r> ;

\ -----------------------------------------------------------------------------
\ EXECUTE

\ The CFM provides no indirect branch or call instructions, but it does provide
\ return. So to call an arbitrary address, we call EXECUTE (leaving the origin
\ return address), and EXECUTE inserts the address on the return stack before
\ "returning" to it.

: execute  ( i*x xt -- j*x )  >r ; ( NOINLINE )

\ -----------------------------------------------------------------------------
\ Basic control structures.

\ Records the current location as the destination of a backwards branch, yet
\ to be assembled by <resolve .
: mark<  ( -- dest )  freeze ;
\ Assembles a backwards branch (using the given template) to a location left
\ by mark< .
: <resolve  ( dest template -- )
  swap u2/  \ convert to word address
  or asm, ;

\ Assembles a forward branch (using the given template) to a yet-unknown
\ location. Leaves the address of the branch (the 'orig') on the stack for
\ fixup via >resolve .
: mark>  ( template -- orig )
  freeze
  swap asm, ;
\ Resolves a forward branch previously assembled by mark> by updating its
\ destination field.
: >resolve  ( orig -- )
  dup @
  freeze u2/ or
  swap ! ;

\ The host has been providing IF ELSE THEN until now. These definitions
\ immediately shadow the host versions.
: if  ( C: -- orig )  $2000 mark> ; immediate
: then  ( C: orig -- )  >resolve ; immediate
: else  ( C: orig1 -- orig2 )
  $0000 mark>
  swap >resolve ; immediate

\ Loop support!
: begin  ( C: -- dest )  mark< ; immediate
: again  ( C: dest -- )  0 <resolve ; immediate
: until  ( C: dest -- )  $2000 <resolve ; immediate

\ -----------------------------------------------------------------------------
\ Exception handling.

\ The CFM makes this difficult by not allowing us to write the stack pointers
\ directly. Instead, to adjust the stack pointers to a specified value, we
\ have to perform a sequence of stack manipulations.

: SP!   ( tgt -- * )
  1+ depth - dup 0< if   \ going down
    begin
      ?dup if
        nip 1+
      else
        exit
      then
    again
  else  \ going up
    begin
      ?dup if
        dup 1-
      else
        exit
      then
    again
  then ;

: RSP!   ( tgt -- * )
  rdepth 1- - dup 0< if   \ going down
    begin
      ?dup if
        r> rdrop >r 1+
      else
        exit
      then
    again
  else  \ going up
    begin
      ?dup if
        r@ >r 1-
      else
        exit
      then
    again
  then ;

: catch
  depth >r
  handler @ >r
  rdepth handler !
  execute
  r> handler !
  rdrop
  0 ;

: throw
  ?dup if
    handler @ RSP!
    r> handler !
    r> swap >r
    SP! drop r>
  then ;

<TARGET-CATCH>

\ -----------------------------------------------------------------------------
\ LITERAL

\ Compiles code to insert a computed literal into a definition.
: literal  ( C: x -- )  ( -- x )
  dup 0< if  ( MSB set )
    invert true
  else
    false
  then
  swap $8000 or asm,
  if $6600 asm, then ; immediate


\ -----------------------------------------------------------------------------
\ The inlining XT compiler.

\ Appends the execution semantics of a word to the current definition. In
\ practice, this means either compiling in a call, or inlining it (if the
\ target word contains a single returning instruction). The result goes
\ through asm, and thus may be subject to fusion.
: compile,  ( xt -- )
  \ Check if the instruction at the start of the target code field is a
  \ fused operate-return instruction.
  dup @  $F04C and  $700C = if
    \ Retrieve it and mask out its return effect.
    @ $EFF3 and
  else
    \ Convert the CFA into a call.
    u2/ $4000 or
  then
  asm, ;


\ -----------------------------------------------------------------------------
\ More loop words, implemented with POSTPONE, which we can use now.

: while  ( C: dest -- orig dest )
  postpone if
  swap ; immediate
: repeat  ( C: orig dest -- )
  postpone again
  postpone then ; immediate

\ -----------------------------------------------------------------------------
\ Useful Forth words.

: rot  ( x1 x2 x3 -- x2 x3 x1 )
  >r swap r> swap ;

: bounds over + swap ;

\ Convert a counted string to a normal string. We use counted strings rarely,
\ but they're useful in the dictionary.
: count  ( c-addr1 -- c-addr2 u )
  dup 1+ swap c@ ;

\ -----------------------------------------------------------------------------
\ Dictionary search.

\ Compares two strings.
: s= ( c-addr1 u1 c-addr2 u2 -- ? )
  \ Early exit if the lengths are different.
  rot over xor if drop 2drop false exit then
  ( c-addr1 c-addr2 u )
  >r over swap -  ( c-addr1 1-2 ) ( R: u)
  r> swap >r   ( c-addr1 u ) ( R: 1-2)
  bounds      ( c-addrE c-addrS ) ( R: 1-2)
  begin
    over over xor
  while  ( c-addrE c-addr ) ( R: 1-2 )
    dup c@  ( c-addrE c-addr c ) ( R: 1-2 )
    over r@ - c@  ( c-addrE c-addr c c2 ) ( R: 1-2 )
    xor if 2drop rdrop false exit then
    1+
  repeat
  2drop rdrop true ;

\ Searches the dictionary for a definition with the given name. This is a
\ variant of standard FIND, which uses a counted string for some reason.
: sfind  ( c-addr u -- c-addr u 0 | xt flags true )
  LATEST
  begin          ( c-addr u lfa )
    @ dup
  while
    >r  ( stash the LFA ) ( c-addr u )              ( R: lfa )
    2dup                  ( c-addr u c-addr u )     ( R: lfa )
    r@ cell+ count        ( c-addr u c-addr u c-addr u ) ( R: lfa )
    s= if                 ( c-addr u )              ( R: lfa )
      nip                 ( u )                     ( R: lfa )
      r> cell+            ( u nfa )
      1+  +  aligned      ( ffa )
      dup cell+           ( ffa cfa )
      swap @              ( cfa flags )
      true exit           ( cfa flags true )
    then    ( c-addr u ) ( R: lfa )
    r>      ( c-addr u lfa )
  repeat ;


\ -----------------------------------------------------------------------------
\ More useful Forth words.

: min  ( n1 n2 -- lesser )
  2dup < if drop else nip then ;  ( TODO could be optimized )

: c!  ( c c-addr -- )
  dup >r
  1 and if  \ LSB set
    8 lshift  \ position our bits
    $FF       \ prepare the mask
  else
    $FF and   \ ensure top bits are clear
    $FF00     \ prepare the mask
  then
  r@ @ and or r> ! ;

: c,  here c!  1 allot ;

: :noname
  align here ] ;

\ -----------------------------------------------------------------------------
\ Quotations (inline anonymous definitions).

\ A quotation is a nameless function nested within another function.
\ At compile time, we generate code to skip over the inlined quotation code,
\ and push its CFA / XT. Thus at runtime it acts as an XT literal for an
\ unfindable word.

\ Runtime implementation for [:
: ([:)
  \ Compute the CFA of the definition from the return address, and stack it.
  r@ cell+
  \ Adjust the return address to skip the definition.
  r> @ 2* >r
  ;

\ Introduces a quotation. May nest.
: [:
  postpone ([:)
  0 mark>
  ; immediate

\ Ends a quotation.
: ;]
  postpone exit
  >resolve
  ; immediate

\ Processes a string character-by-character with an accumulator parameter. The
\ xt will be executed with the presumed stack effect
\    c x -- x'
\ Initially x is x0; after that, it is the result of the last execution. The
\ final x is left on the stack.
: sfoldl  ( c-addr u x0 xt -- x )
  >r >r
  bounds
  begin
    over over xor
  while
    dup c@ r> r@ execute >r
    1+
  repeat
  2drop r> rdrop ;

\ -----------------------------------------------------------------------------
\ Basic source code input support and parsing.

\ Returns the current input as a string.
: SOURCE  ( -- c-addr u )  'SOURCE dup @ swap cell+ @ ;

: /string   ( c-addr u n -- c-addr' u' )
  >r  r@ - swap  r> + swap ;

: skip-while  ( c-addr u xt -- c-addr' u' )
  >r
  begin
    over c@ r@ execute
    over and
  while
    1 /string
  repeat
  rdrop ;

: parse-name
  SOURCE  >IN @  /string    ( c-addr u )
  [: $21 u< ;] skip-while over >r   ( c-addr' u' ) ( R: c-addr' )
  [: $20 swap u< ;] skip-while  ( sp-addr sp-u ) ( R: token-addr )
  1 min over +                          ( sp-addr rest-addr ) ( R: " )
  'SOURCE @ -  >IN !
  r> tuck - ;


\ -----------------------------------------------------------------------------
\ Header creation and defining words.

\ Encloses a string in the dictionary as a counted string.
: s,  ( c-addr u -- )
  dup c,        ( Length byte )
  0 [: swap c, ;] sfoldl drop
  align ;

\ Implementation factor of the other defining words: parses a name and creates
\ a header, without generating any code.
: (CREATE)
  ( link field )
  align here  LATEST @ ,  LATEST !
  ( name )
  parse-name s,
  ( flags )
  0 , ;

: :  (CREATE) ] ;
  \ Note that this definition gets used immediately.

: create
  (CREATE)
  postpone (dovar) ;

: constant
  (CREATE)
  postpone (docon)
  , ;

: variable create 0 , ;


\ -----------------------------------------------------------------------------
\ Semicolon. This is my favorite piece of code in the kernel, and the most
\ heavily commented punctuation character of my career thus far.

\ Recall that the Forth word ; (semicolon) has the effect of compiling in a
\ return-from-colon-definition sequence and returning to the interpreter.

\ Recall also that ; is an IMMEDIATE word (it has to be, to have those effects
\ during compilation).

\ Finally, note that BsForth never hides definitions. A definition is available
\ for recursion without further effort, in deviation from the standard.

\ Alright, that said, let's go.
: ;
  postpone exit
  postpone [

  \ Now we have a condundrum. How do we end this definition? We've been using a
  \ host-emulated version of ; to end definitions 'till now. But now that a
  \ definition exists in the target, it *immediately* shadows the emulated
  \ version. We can't simply write ; because ; is not yet IMMEDIATE. But we can
  \ fix that:
  [ immediate ]

  \ Because ; is now IMMEDIATE, we are going to recurse *at compile time.* We
  \ invoke the target definition of ; to complete the target definition of ; by
  \ performing the actions above.
  ;

\ Voila. Tying the knot in the Forth compiler.


\ -----------------------------------------------------------------------------
\ More useful Forth words.

: does>
  \ End the defining code with a non-tail call to (does>)
  [:  ( R: tail-addr -- )
      \ Patch the first instruction of the last (current) definition to contain
      \ a call to the code after DOES> .
      lastxt  r> u2/ $4000 or  swap !
  ;] compile,
  \ Control will reach this point from the call instruction at the start of the
  \ code field. We need to reveal the parameter field address by postponing r>
  postpone r>
  ; immediate

variable #user  9 #user !
  \ Holds the number of user variables that have been defined.
: user
  create  #user @ cells ,  1 #user +!
  does> @  U0 @ + ;

: u<= swap u< 0= ;

: u/mod  ( num denom -- remainder quotient )
  0 0 16
  begin                   ( n d r q i )
    >r                    ( n d r q ) ( R: i )
    >r >r                 ( n d ) ( R: i q r )
    over 15 rshift        ( n d n[15] ) ( R: i q r )
    r> 2* or              ( n d 2*r+n[15] )  ( R: i q )
    >r >r                 ( n ) ( R: i q 2*r+n[15] d )
    2*                    ( 2*n ) ( R: i q 2*r+n[15] d )
    r> r>                 ( 2*n d 2*r+n[15] ) ( R: i q )
    r> 2* >r              ( 2*n d 2*r+n[15] ) ( R: i 2*q )
    2dup u<= if
      over -
      r> 1 or >r
    then
    r> r>
    1-
    dup 0=
  until ( n d r q i )
  drop >r nip nip r> ;

: u/  u/mod nip ;
: umod  u/mod drop ;

.( Before terminal support, HERE is )
here host.

\ -----------------------------------------------------------------------------
\ User-facing terminal.

\ We assume there is a terminal that operates like a, well, terminal, without
\ fancy features like online editing. We can't assume anything about its
\ implementation, however, so we have to define terminal operations in terms
\ of hooks to be implemented later for a specific device.

\ XT storage for the key and emit vectors. This is strictly less efficient than
\ using DEFER and should probably get changed later.
variable 'key
variable 'emit
10 base !
:noname drop ; 'emit !
:noname 0 ; 'key !

: key 'key @ execute ;
: emit 'emit @ execute ;

$20 constant bl
: space bl emit ;
: beep 7 emit ;

: cr $D emit $A emit ;
  \ This assumes a traditional terminal and is a candidate for vectoring.

: type  ( c-addr u -- )
  0 [: swap emit ;] sfoldl drop ;

\ Receive a string of at most u characters, allowing basic line editing.
\ Returns the number of characters received, which may be zero.
: accept  ( c-addr u -- u )
  >r 0
  begin ( c-addr pos ) ( R: limit )
    key

    $1F over u< if  \ Printable character
      over r@ u< if   \ in bounds
        dup emit  \ echo character
        >r over over + r>  ( c-addr pos dest c )
        swap c! 1+    ( c-addr pos' )
        0  \ "key" for code below
      else  \ buffer full
        beep
      then
    then

    3 over = if   \ ^C - abort
      2drop 0     \ Reset buffer to zero
      $D          \ act like a CR
    then

    8 over = if   \ Backspace
      drop
      dup if  \ Not at start of line
        8 emit  space  8 emit   \ rub out character
        1-
      else    \ At start of line
        beep
      then
      0
    then

    $D =
  until
  rdrop nip ;

: u.
  here 16 +   \ get a transient buffer big enough for even base 2
  begin  ( u c-addr )
    1- swap
    base @ u/mod  ( c-addr rem quot )
    >r            ( c-addr rem ) ( R: quot )
    9 over u< 7 and + '0' +  over c!  ( c-addr ) ( R: quot )
    r>            ( c-addr quot )
    ?dup          ( c-addr quot quot )
  while
    swap          ( u' c-addr )
  repeat
  here 16 + over -
  type
  space ;

: .  dup 0< if  '-' emit  negate  then u. ;

\ -----------------------------------------------------------------------------
\ Text interpreter.

: ABORT true throw ;

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


: digit  ( c -- x )
  '0' -
  9 over u< 7 and -
  base @ 1- over u< -13 and throw ;


\ Converts the given string into a number, in the current base, but respecting
\ base prefixes $ (hex) and # (decimal). Throws -13 (undefined word) if parsing
\ fails.
: number  ( c-addr u -- x )
  3 over = if   \ string is exactly three characters, check for char literal
    over  dup c@ ''' =  swap 2 + c@ ''' =  and if
      drop 1+ c@ exit
    then
  then
  1 over u< if  \ string is at least two characters, check for prefix
    over c@ '-' = if  \ negative
      1 /string
      number
      negate exit
    then
    over c@ '#' - 2 u< if  \ number prefix
      \ Note: this exploits the fact that the decimal prefix '#' and the
      \ hex prefix '$' are adjacent numerically.
      base @ >r
      over c@ '#' - 6 u* 10 + base !
      1 /string
      [ ' number ] literal catch
      r> base !
      throw exit
    then
  then
  0 [: base @ u*  swap digit + ;] sfoldl ;

\ Reports an unknown word.
: ??  ( c-addr u -- * )
  type  '?' emit  cr  -13 throw ;

: interpret
  begin
    parse-name
  ?dup while
    sfind if  \ word found
      ( xt flags )
      if  \ immediate
        execute
      else  \ normal
        STATE @ if  \ compiling
          compile,
        else  \ interpreting
          execute
        then
      then
    else  \ word unknown
      2dup >r >r  \ save string
      [ ' number ] literal catch
      ?dup if \ failed
        r> r> ??
      else
        rdrop rdrop   \ discard saved string
        STATE @ if  \ compile it as a literal
          postpone literal
        then
        \ otherwise just leave it on the stack.
      then
    then
  repeat
  drop ;

\ -----------------------------------------------------------------------------
\ Parsing words and target syntax.

: '  ( "name" -- xt )
  parse-name dup if
    sfind if  ( xt flags )
      drop exit
    then
    \ Got input, but the input was bogus.
  then
  ?? ;

\ Line comments simply discard the rest of input.
: \
  SOURCE nip >IN ! ;  immediate

\ Block comments look for a matching paren.
: (
  SOURCE  >IN @  /string
  [: ')' <> ;] skip-while
  1 min +  \ consume the trailing paren
  'SOURCE @ -  >IN ! ;  immediate

: S"
  SOURCE  >IN @  /string
  over >r
  [: '"' <> ;] skip-while
  2dup  1 min +  'SOURCE @ -  >IN !
  drop r> tuck -

  [:  ( -- c-addr u )
      \ Uses its return address to locate a string literal. Pushes the
      \ literal onto the stack and updates the return address to skip
      \ it.
      r>        ( addr )
      count     ( c-addr u )
      over over + aligned  ( c-addr u end )
      >r
  ;] compile,
  s, ;  immediate

: postpone_
  parse-name dup if
    sfind if  ( xt flags )
      if  \ immediate
        compile,
      else  \ normal
        postpone literal
        postpone compile,
      then
      exit
    then
  then
  ?? ; immediate
8  LATEST @ cell+  c!  \ fix the length

: [']  ' postpone literal ; immediate

\ -----------------------------------------------------------------------------
\ Programming tools.

\ Variant on ANS MARKER that takes a flag on stack indicating whether to
\ preserve itself.
: remarker  ( ? "name" -- )
  if
    create LATEST @ , here cell+ ,
  else
    here LATEST @ create , ,
  then
  does> dup @ LATEST !  cell+ @ DP ! ;

\ 'marker foo' creates a word 'foo' that, when executed, restores the
\ dictionary and search order to the state they had before 'foo' was defined,
\ forgetting 'foo' in the process.
: marker  ( "name" -- )  false remarker ;

\ -----------------------------------------------------------------------------
\ END OF GENERAL KERNEL CODE
\ -----------------------------------------------------------------------------
.( After compiling general-purpose code, HERE is... )
here host.




( ----------------------------------------------------------- )
( Icestick SoC support code )

: #bit  1 swap lshift ;

( ----------------------------------------------------------- )
( Interrupt Controller )

$B000 constant IRQST  ( status / enable trigger )
\ $D802 constant IRQEN  ( enable )
$B004 constant IRQSE  ( set enable )
$B006 constant IRQCE  ( clear enable )

( Atomically enables interrupts and returns. This is intended to be tail )
( called from the end of an ISR. )
: ei  IRQST !d ;

: irq-off  ( u -- )  #bit IRQCE ! ;
: irq-on   ( u -- )  #bit IRQSE ! ;

13 constant irq#m1
14 constant irq#m0
15 constant irq#negedge

( ----------------------------------------------------------- )
( I/O ports )

\ $8000 constant outport      ( literal value)
$8002 constant OUTSET  ( 1s set pins, 0s do nothing)
$8004 constant OUTCLR  ( 1s clear pins, 0s do nothing)
$8006 constant OUTTOG  ( 1s toggle pins, 0s do nothing)

: outpin
  create #bit ,
  does> @ swap if OUTSET else OUTCLR then ! ;

$9000 constant IN

( ----------------------------------------------------------- )
( Timer )

$A000 constant TIMV
$A002 constant TIMF
$A004 constant TIMM0
$A006 constant TIMM1

( ----------------------------------------------------------- )
( UART receive queue and flow control )

8 constant uart-#rx
variable uart-rx-buf  uart-#rx 1- cells allot
variable uart-rx-hd
variable uart-rx-tl

1 outpin >CTS_N

: rxq-empty? uart-rx-hd @ uart-rx-tl @ = ;
: rxq-full? uart-rx-hd @ uart-rx-tl @ - uart-#rx = ;

( Inserts a cell into the receive queue. This is intended to be called from )
( interrupt context, so if it encounters a queue overrun, it simply drops )
( data. )
: >rxq
  rxq-full? if drop exit then

  uart-rx-buf  uart-rx-hd @ [ uart-#rx 1- 2* ] literal and +  !
  2 uart-rx-hd +! ;

( Takes a cell from the receive queue. If the queue is empty, spin. )
: rxq>
  begin rxq-empty? 0= until
  uart-rx-buf  uart-rx-tl @ [ uart-#rx 1- 2* ] literal and +  @
  2 uart-rx-tl +! ;

\ Receives a byte from RX. In the event of an error (e.g. framing) the sign bit
\ is set.
: rx  ( -- x )
  rxq>
  rxq-empty? if 0 >CTS_N then  \ allow sender to resume if we've emptied the queue.
  ;

( ----------------------------------------------------------- )
( Hard UART )

$D000 constant UARTST
$D002 constant UARTRD
$D004 constant UARTTX
$D006 constant UARTRX

9 constant irq#rxne

: tx
  \ Wait for transmitter to be free
  begin UARTST @ 2 and until
  UARTTX ! ;

: rx-isr
  UARTRX @ >rxq
  1 >CTS_N ;

: uart-rx-init
  \ Clear any pending queued character.
  UARTRX @ drop
  \ Enable the IRQ.
  irq#rxne irq-on
  0 >CTS_N ;

( ----------------------------------------------------------- )
( Icestick board features )

: ledtog  5 + #bit OUTTOG ! ;

\ ----------------------------------------------------------------------
\ Text mode video display

$C000 constant VTH  \ video - timing - horizontal
$C008 constant VTV  \ video - timing - vertical
$C010 constant VPX  \ video - pixel count
$C012 constant VIA  \ video - interrupt acknowledge
$C014 constant VFB  \ video - font base
$C016 constant VWA  \ video - write address
$C018 constant VWD  \ video - write data
$C01A constant VC0  \ video - character 0

\ Overwrites a section of video memory with a given value.
: vfill  ( v-addr u x -- )
  VWA @ >r    \ save cursor position
  >r          \ stash cell to write
  swap VWA !  \ set up write address
  begin
    dup
  while ( count ) ( R: vwa x )
    1- r@ VWD !
  repeat
  rdrop drop
  r> VWA !  \ restore old cursor
  ;

variable vcols  \ columns in the text display
variable vrows  \ rows in the text display
variable vatt   \ attributes for text in top 8 bits

: vsize vcols @ vrows @ u* ;

\ Sets the current color to the given fore and back color indices.
: vcolor!  ( back fore -- )
  4 lshift or 8 lshift  vatt ! ;

\ Fills a section of text RAM with spaces in the current color.
: vclr  ( v-addr u -- )
  bl vatt @ or vfill ;

\ Clears the screen, filling it with spaces in the current color, and resets
\ the cursor to the lower-left corner. As a side effect, this resets the text
\ window scroll to the start of video memory.
: vpage
  0  \ start of memory
  vsize  \ size of a screen
  vclr     \ clear a screen-sized area of text RAM
  0 VC0 !   \ make it the active screen
  vsize vcols @ - VWA !   \ cursor to lower left
  ;

\ Scrolls the display up, revealing a blank line at the bottom. Leaves the
\ cursor address unchanged (i.e. it moves up on the display).
: vscroll
  vcols @ VC0 +!
  VC0 @  vsize vcols @ - + $7FF and  vcols @  vclr
  ;

\ "Types" a character without control character interpretation. Advances the
\ cursor. Scrolls the display as needed.
: vputc ( c -- )
  vatt @ or VWD !   \ store the character with attributes
  VWA @  VC0 @ -  $7FF and    \ get distance from start of display
  vsize = if   \ if we've run off the end
    vscroll  \ reveal another line
  then ;

\ "Types" a character with control character interpretation, simulating a
\ terminal.
: vemit ( c -- )
  7 over = if drop exit then   \ ignore BEL

  8 over = if drop  \ backspace
    VC0 @ VWA @ xor if  VWA @ 1- $7FF and VWA ! then
    exit
  then

  10 over = if drop \ line feed
    vscroll
    VWA @  vcols @ + $7FF and  VWA !
    exit
  then

  12 over = if drop \ form feed / page
    vpage exit
  then

  13 over = if drop \ carriage return
    \ TODO broken broken ?
    \ Compute current offset into the display
    VWA @  VC0 @  -  $7FF and
    \ Round down by division and multiplication
    vcols @ u/  vcols @ u*
    \ Project back into the display and assign
    VC0 @ + $7FF and VWA !
    exit
  then

  vputc ;


: vid
  119 VTH !
  167 VTH 4 + !
  639 VTH 6 + !
  100 VTV !
  122 VTV 4 + !
  399 VTV 6 + !
  80 vcols !
  25 vrows !
  0 15 vcolor!
  vpage
  ;
( ----------------------------------------------------------- )
( Programming tools )

: words
  LATEST
  begin
    @ dup
  while
    2 over +  \ compute address of name field
    count     \ convert to counted string
    type space
  repeat
  drop ;

( ----------------------------------------------------------- )
( SD Card )


: cycles ( u -- )   \ delays for at least u cycles
  >r
  TIMV @
  begin   ( start )
    TIMV @ over -   ( start delta )
    r@ u< 0= if
      rdrop drop exit
    then
  again ;

variable sdcyc  50 sdcyc !
: sddelay sdcyc @ cycles ;

2 outpin >sdclk
3 outpin >sdmosi
4 outpin >sdcs_

: sdx1  ( bits -- bits' )
  $8000 over and >sdmosi
  1 lshift

  sddelay  1 >sdclk
  sddelay

  IN @ 2 and 1 rshift  or
  
  0 >sdclk ;

: sdx  ( tx -- rx )
  8 lshift
  sdx1 sdx1 sdx1 sdx1
  sdx1 sdx1 sdx1 sdx1 ;

: sdidle $FF sdx drop ;

: sdr1
  $FF sdx
  dup $FF = if
    drop sdr1 exit
  then
  sdidle ;

: sdcmd  ( arglo arghi cmd -- )
  $40 or sdx drop         \ start bit + cmd
  dup 8 rshift sdx drop   \ arg[31:24]
  sdx drop                \ arg[23:16]
  dup 8 rshift sdx drop   \ arg[15:8]
  sdx drop                \ arg[7:0]
  $95 sdx drop ;          \ checksum, hardcoded for CMD0

: sdacmd  ( arglo arghi cmd -- )
  0 0 55 sdcmd sdr1 drop
  sdcmd ;

: sdcmd0
  0 0 0 sdcmd
  sdr1 ;

: sdacmd41
  0 0 41 sdacmd sdr1 ;

: sdinit
  \ Use slow clock.
  50 sdcyc !
  \ Raise MOSI and CS
  1 >sdmosi   1 >sdcs_
  \ Send 9 bytes with MOSI high (=81 edges, > required 74)
  9 begin
    dup
  while
    1-
    $FF sdx drop
  repeat drop

  0 >sdcs_   \ select card
  \ Send CMD0
  sdcmd0
  \ Require a $01 response
  1 <> 1 and throw
  \ Send CMD1 until we get a 0 back
  begin
    sdacmd41 0=
  until
  1 sdcyc !   \ high speed
  ;

: sdcrc16  ( c-addr u -- crc )
  0   \ initial CRC seed
  [:
    \ Swap bytes
    dup 8 lshift swap 8 rshift or
    xor
    $FF over and 4 rshift xor
    dup 12 lshift xor
    $FF over and 5 lshift xor
  ;] sfoldl ;

: sdrd ( dest seclo sechi -- )
  17 sdcmd sdr1 throw
  begin $FF sdx $FF xor until

  dup >r    \ stash the buffer address
  512 bounds begin
    over over xor
  while
    $FF sdx over c!
    1+
  repeat 2drop
  $FF sdx 8 lshift $FF sdx or   \ read the CRC16
  r> 512 sdcrc16
  xor throw
  sdidle ;

: sdwr ( src seclo sechi -- )
  24 sdcmd sdr1 throw
  sdidle

  $FE sdx drop
  dup >r
  512 bounds begin
    over over xor
  while
    dup c@ sdx drop
    1+
  repeat 2drop
  r> 512 sdcrc16
  dup 8 rshift sdx drop sdx drop

  begin
    $FF sdx
    dup $FF =
  while
    drop
  repeat \ leaving response code on stack
  $1F and

  begin $FF sdx $FF = until

  sdidle

  5 <> throw ;

\ -------------------------------------------------------------------
\ Block support

\ We place a single 1kiB block buffer at the top of SRAM.
$8000 1024 - constant blkbuf

\ If bit 0 of blkstat is set, the buffer is allocated to a disk block. If bit 1
\ of blkstat is also set, the buffer is dirty.
variable blkstat

\ If the buffer is allocated, blkall stores the block number.
variable blkall

\ Convert a block number to an LBA.
: blk>sec  ( u -- u )  1- 2* ;

\ Marks the contents of the current buffer as dirty. If the current buffer is
\ not allocated, this is a no-op.
: update  blkstat @  2 or  blkstat ! ;

\ Flush modified buffers to disk and unassign.
: flush
  \ If the buffer is both assigned and dirty,
  blkstat @ 3 and 3 = if
    blkall @ blk>sec >r
    blkbuf        r@    9 lshift  r@    7 rshift  sdwr
    blkbuf 512 +  r@ 1+ 9 lshift  r> 1+ 7 rshift  sdwr
  then
  \ Unassign.
  0 blkstat ! ;

\ Arrange for a block to be assigned to a buffer, and return its address.
: block  ( u -- addr )
  \ If the requested block is already loaded, just return its address.
  blkall @ over =  blkstat @ 1 and  and if  drop blkbuf exit  then

  \ This is written in an attempt to keep I/O exceptions from corrupting state.
  flush
  dup blk>sec >r
  blkbuf        r@    9 lshift  r@    7 rshift  sdrd
  blkbuf 512 +  r@ 1+ 9 lshift  r> 1+ 7 rshift  sdrd
  blkall !
  1 blkstat !
  blkbuf ;

\ -------------------------------------------------------------------
\ Block load and dev support.

variable blk
  \ Holds the block index currently being used as source, or 0 if
  \ source is coming from somewhere else.

\ Interprets source code in block u.
: load  ( i*x u -- j*x )
  \ Save source state.
  blk @ >r   SOURCE >r >r   >IN @ >r
  \ Set up input to read from the block.
  dup blk !
  block 'SOURCE !  1024 'SOURCE cell+ !  0 >IN !
  \ Try to interpret.
  ['] interpret catch
  \ Whether that succeeded or failed, restore the old input spec.
  r> >IN !  r> r> 'SOURCE cell+ !  'SOURCE !  r> dup blk !  ( except old-blk )
  \ If we were loading from a block before LOAD, the address is likely
  \ wrong. Update it.
  ?dup if  block 'SOURCE !  then
  throw ;

\ Lists source code in a block using the conventional 64x16 format.
: list  ( u -- )
  block 1024 bounds
  begin
    over over xor
  while
    dup 64 cr type
    64 +
  repeat
  2drop ;

\ -------------------------------------------------------------------
\ PS/2 keyboard GPIO interface

variable kbdbuf
variable kbd#bit

: kbdisr
  3 #bit  IN @ and  12 lshift     \ get data value in bit 15
  kbdbuf @  1 rshift or  kbdbuf ! \ insert it into shift register
  kbd#bit @ 1 - kbd#bit !d        \ decrement bit counter
  0= if   \ we're done
    irq#negedge irq-off           \ disable this IRQ
    9 #bit OUTSET !               \ pull clock low
  else
    IN !d                         \ acknowledge IRQ
  then ;

: kbd@
  11 kbd#bit !                    \ expecting 11 bits
  9 #bit OUTCLR !                 \ release clock line
  IN !d                           \ clear pending negedge IRQ
  irq#negedge irq-on              \ enable IRQ
  begin kbd#bit @ 0= until        \ wait for all bits
  kbdbuf @ 6 rshift $FF and       \ extract bits
  ;

: kbdinit
  9 #bit OUTSET !                 \ pull clock low
  ;

: kbdscan
  kbd@
  $E0 over = if   \ extended scan code set 0
    drop kbdscan  8 #bit or  exit
  then
  $E1 over = if   \ hello, pause
    drop
    kbdscan @ drop
    kbdscan @ drop
    $200 exit
  then
  $F0 over = if   \ break code
    drop kbdscan  15 #bit or  exit
  then
  ;

create kbdlt
0 c,    0 c,      \ 0: unused
0 c,    0 c,      \ F9
0 c,    0 c,      \ 2: unused
0 c,    0 c,      \ F5
0 c,    0 c,      \ F3
0 c,    0 c,      \ F1
0 c,    0 c,      \ F2
0 c,    0 c,      \ F12
0 c,    0 c,      \ 8: unused
0 c,    0 c,      \ F10
0 c,    0 c,      \ F8
0 c,    0 c,      \ F6
0 c,    0 c,      \ F4
9 c,    9 c,      \ TAB
'`' c,  '~' c,
0 c,    0 c,      \ 0F: unused
0 c,    0 c,      \ 10: unused
0 c,    0 c,      \ L ALT
0 c,    0 c,      \ L SHIFT
0 c,    0 c,      \ 13: unused
0 c,    0 c,      \ L CTRL
'q' c,  'Q' c,
'1' c,  '!' c,
0 c,    0 c,      \ 17: unused
0 c,    0 c,      \ 18: unused
0 c,    0 c,      \ 19: unused
'z' c,  'Z' c,
's' c,  'S' c,
'a' c,  'A' c,
'w' c,  'W' c,
'2' c,  '@' c,
0 c,    0 c,      \ 1F: unused
0 c,    0 c,      \ 20: unused
'c' c,  'C' c,
'x' c,  'X' c,
'd' c,  'D' c,
'e' c,  'E' c,
'4' c,  '$' c,
'3' c,  '#' c,
0 c,    0 c,      \ 27: unused
0 c,    0 c,      \ 28: unused
bl c,   bl c,
'v' c,  'V' c,
'f' c,  'F' c,
't' c,  'T' c,
'r' c,  'R' c,
'5' c,  '%' c,
0 c,    0 c,      \ 2F: unused
0 c,    0 c,      \ 30: unused
'n' c,  'N' c,
'b' c,  'B' c,
'h' c,  'H' c,
'g' c,  'G' c,
'y' c,  'Y' c,
'6' c,  '^' c,
0 c,    0 c,      \ 37: unused
0 c,    0 c,      \ 38: unused
0 c,    0 c,      \ 39: unused
'm' c,  'M' c,
'j' c,  'J' c,
'u' c,  'U' c,
'7' c,  '&' c,
'8' c,  '*' c,
0 c,    0 c,      \ 3F: unused
0 c,    0 c,      \ 40: unused
',' c,  '<' c,
'k' c,  'K' c,
'i' c,  'I' c,
'o' c,  'O' c,
'0' c,  ')' c,
'9' c,  '(' c,
0 c,    0 c,      \ 47: unused
0 c,    0 c,      \ 48: unused
'.' c,  '<' c,
'/' c,  '?' c,
'l' c,  'L' c,
';' c,  ':' c,
'p' c,  'P' c,
'-' c,  '_' c,
0 c,    0 c,      \ 4F unused
0 c,    0 c,      \ 50 unused
0 c,    0 c,      \ 51 unused
''' c,  '"' c,
0 c,    0 c,      \ 53 unused
'[' c,  '{' c,
'=' c,  '+' c,
0 c,    0 c,      \ 56 unused
0 c,    0 c,      \ 57 unused
0 c,    0 c,      \ CAPS
0 c,    0 c,      \ R SHIFT
13 c,   13 c,     \ ENTER
']' c,  '}' c,
0 c,    0 c,      \ 5C unused
'\' c,  '|' c,
0 c,    0 c,      \ 5E unused
0 c,    0 c,      \ 5F unused
0 c,    0 c,      \ 60 unused
0 c,    0 c,      \ 61 unused
0 c,    0 c,      \ 62 unused
0 c,    0 c,      \ 63 unused
0 c,    0 c,      \ 64 unused
0 c,    0 c,      \ 65 unused
8 c,    8 c,      \ backspace

variable kbdmod
: kbdkey
  kbdscan
  $12 over =  over $59 = or if  \ shift make
    drop  kbdmod @ 1 or kbdmod !
    kbdkey exit
  then
  $8012 over =  over $8059 = or if  \ shift break
    drop kbdmod @ 1 invert and kbdmod !
    kbdkey exit
  then
  $14 over =  over $114 = or if \ ctrl make
    drop  kbdmod @ 2 or kbdmod !
    kbdkey exit
  then
  $8014 over =  over $8114 = or if  \ ctrl break
    drop kbdmod @ 2 invert and kbdmod !
    kbdkey exit
  then

  \ Having processed the modifiers, ignore any large values, including
  \ break codes.
  dup  $66 u> if  drop kbdkey exit  then

  \ We now have a value in-range for the lookup table.
  cells   \ convert to offset
  kbdmod @ 1 and +  \ mix in shift offset
  kbdlt + c@

  \ The result might be zero for unused/non-ASCII codes.
  dup 0= if drop kbdkey exit then

  \ Apply control.
  kbdmod @ 2 and if  $20 invert and  '@' -  then ;

( ----------------------------------------------------------- )
( Demo wiring below )

: delay 0 begin 1+ dup 0= until drop ;

create vectors  16 cells allot

: isr
  \ Vectored interrupt dispatcher.
  \ Which interrupts are active?
  15 IRQST @
  begin   ( vector# irqst )
    dup \ while any remain
  while
    $8000 over and if
      over cells  vectors + @ execute
    then
    1 lshift
    swap 1 - swap
  repeat
  drop drop
  \ Patch up interrupt return address.
  r> 2 - >r
  \ Atomically enable interrupts and return.
  ei ;

\ Vector table
' rx-isr vectors 9 cells + !
' kbdisr vectors irq#negedge cells + !

create TIB 80 allot

: rx! rx dup 0< if rx! exit then ;

: quit
  0 RSP!
  0 handler !
  postpone [
  begin
    TIB 'SOURCE !
    80  'SOURCE cell+ !
    0 >IN !
    SOURCE accept  'SOURCE cell+ !
    space
    ['] interpret catch
    ?dup if
      true over = if
        \ abort isn't supposed to print
        drop
      else 
        . '!' emit
      then
    else
      STATE @ 0= if  
        'o' emit 'k' emit
      then
    then
    cr
  again ;

: cold
  \ Initialize user area
  $7B80 U0 !
  0 handler !

  uart-rx-init
  347 UARTRD ! \ Set baud rate to 115200

  \ Take a best-effort crack at initializing the disk
  ['] sdinit catch drop

  IN @  4 #bit and if   \ If S2 is held, boot with serial console
    ['] tx 'emit !
    ['] rx! 'key !
  else  \ otherwise, normal config
    vid
    ['] vemit 'emit !
    ['] kbdkey 'key !
  then
  ei
  10 base !
  35 emit
  LATEST @ cell+ count type
  35 emit cr
  quit ;

( install cold as the reset vector )
' cold  u2/  0 !
( install isr as the interrupt vector )
' isr  u2/  2 !

true remarker empty

.( Compilation complete. HERE is... )
here host.
