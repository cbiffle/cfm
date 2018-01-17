\ Bootstrap Forth, my first Forth for the CFM. Volume one.


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
\ Stores x at addr, leaving addr on the stack.
: !a  ( x addr -- addr )  [ $6023 asm, ] ;

\ Pushes a word containing the depth of the parameter stack in bits 7:0, and
\ the depth of the return stack in bits 15:8.
: depths  ( -- x )  [ $6E81 asm, ] ;

\ Pokes x into an I/O port, leaving x.
: io!d  ( x port -- x )  [ $6133 asm, ] ;

\ Reads from an I/O port.
: io@  ( port -- x )  [ $6c10 asm, ] ;

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
6 constant ROOTWL  ( head of wordlist )
8 constant DP
10 constant FREEZEP
12 constant VOC-LINK

: handler U0 @ ;
: STATE U0 @ 2 + ;
: 'SOURCE U0 @ 4 + ;
: >IN U0 @ 8 + ;
: base U0 @ 10 + ;
: CURRENT U0 @ 12 + ;
: CONTEXT U0 @ 14 + ;

$FFFF constant true  ( also abused as -1 below, since it's cheaper )
0 constant false
2 constant cell
$20 constant bl

\ -----------------------------------------------------------------------------
\ More useful Forth words.

: tuck  ( a b -- b a b )  swap over ;
: !  ( x addr -- )  !d drop ;
: io!  ( x port -- )  io!d drop ;
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

: 2!  ( x1 x0 a-addr -- )  !a cell+ ! ;

: depth  depths $FF and ;
: rdepth  depths 8 rshift 1- ;

\ -----------------------------------------------------------------------------
\ The Dictionary and the Optimizing Assembler.

\ Because the host manipulates the dictionary, it's important to keep the
\ layout consistent between us and the host. This is why ROOTWL, DP, and
\ FREEZEP are part of the system variables block.

: here  ( -- addr )  DP @ ;
: allot  DP +! ;
: raw,  here !  cell allot ;
: cells  2* ;
: align  here  aligned  DP ! ;

\ Access to the CFA/xt of the most recently defined word.
: lastxt  ( -- xt )
  CURRENT @ @  cell+      ( nfa )   \ TODO define actual LATEST
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
  begin
    ?dup
  while  ( c-addr1 c-addr2 u )
    >r
    over c@ over c@ xor if 2drop rdrop false exit then
    1+ swap 1+
    r> 1-
  repeat ( c-addr1 c-addr2 )
  2drop true ;

\ Searches a wordlist for a definition with the given name. This is a
\ variant of standard FIND, which uses a counted string for some reason.
: find-in  ( c-addr u wl -- c-addr u 0 | xt flags true )
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

\ Searches CONTEXT, then CURRENT, for a definition with the given name. This is
\ a variant of standard FIND, which uses a counted string for some reason.
: sfind  ( c-addr u -- c-addr u 0 | xt flags true )
  CONTEXT @ find-in if true exit then
  CURRENT @ find-in ;

\ -----------------------------------------------------------------------------
\ More useful Forth words.

: min  ( n1 n2 -- lesser )
  2dup < if drop else nip then ;  ( TODO could be optimized )

: u<= swap u< 0= ;

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
  align freeze ] ;

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

\ Consume source code characters while they match a predicate, plus the
\ first character to match (if one does). Return the matched section as a
\ string. If the end of input is reached, the string will be zero-length.
: scan  ( pred -- c-addr u )
  SOURCE  >IN @  /string    \ Get unconsumed tail of input.
  over >r                   \ Stash the start address.
  rot skip-while            \ Skip characters matching the predicate.
  2dup  1 min  +            \ Compute new start of input, eating delim.
  'SOURCE @ -  >IN !        \ Advance input to there.
  drop r> tuck -            \ Compute string bounds.
  ;

\ Skip source code characters while they match a predicate.
: skip  ( pred -- )
  SOURCE  >IN @  /string    \ Get unconsumed tail of input.
  rot skip-while            \ Skip characters matching the predicate.
  drop                      \ We only care about the start address.
  'SOURCE @ -  >IN !        \ Advance input to there.
  ;

: parse-name
  [: bl u<= ;] skip
  [: bl u> ;] scan ;


\ -----------------------------------------------------------------------------
\ Header creation and defining words.

\ Encloses a string in the dictionary as a counted string.
: s,  ( c-addr u -- )
  dup c,        ( Length byte )
  0 [: swap c, ;] sfoldl drop
  align ;

\ Given the head of a linked list starting at 'addr', creates a new link at
\ HERE by comma-ing the old head into place and storing HERE as the new
\ head. Implementation factor.
: >link  ( addr -- )
  align here  ( head newlink )
  swap dup @ ,  ( newlink head )
  ! ;

\ Since Forth code is effectively equivalent to machine code on CFM, colon is
\ the simplest of the words that introduce headers. Other words are defined
\ in terms of it. This is unusual; CREATE is more often the shared factor.
: :
  CURRENT @ >link
  \ Name
  parse-name s,
  \ Flags
  0 ,
  \ And begin compiling
  ] ;
  \ Note that this definition gets used immediately.

: create
  :
  postpone [        \ Turn compiler back off
  postpone (dovar)  \ Compile code to push address.
  ;

: constant
  :
  postpone [        \ Turn compiler back off.
  postpone (docon)  \ Compile code to push following cell.
  ,                 \ Compile constant value.
  ;

: variable create 0 , ;

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

variable #user  8 #user !
  \ Holds the number of user variables that have been defined.
: user
  create  #user @ cells ,  1 #user +!
  does> @  U0 @ + ;

: vocabulary
  create  VOC-LINK >link
          CURRENT @ @ ,
  does> cell+ CONTEXT ! ;

: definitions  CONTEXT @ CURRENT ! ;

vocabulary forth
forth definitions

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
  dup '0' - 10 u<
  over 'A' - 25 u<
  or 0= -13 and throw
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
  [: ')' <> ;] scan 2drop ;  immediate

: S"
  [: '"' <> ;] scan

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

: ."
  postpone S"
  postpone type
  ; immediate

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
8  CURRENT @ @ cell+  c!  \ fix the length

: [']  ' postpone literal ; immediate


\ -------------------------------------------------------------------
\ Terminal input buffer and text interpreter

\ TODO the TIB should probably be in the user area.

create TIB 80 allot

: quit
  0 RSP!
  0 handler !
  postpone [
  begin
    80 TIB 'SOURCE 2!
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
        ." ok"
      then
    then
    cr
  again ;

\ -------------------------------------------------------------------
\ Cold start skeleton

variable oncold
variable ramtop

: cold
  \ Set up user area at end of RAM.
  ramtop @  #user @ cells -  U0 !
  \ Initialize user variables known to the system
  0 handler !
  10 base !
  forth definitions

  \ Run system cold hook, if provided.
  oncold @ ?dup if execute then

  ." bsforth | "
  U0 @ here - u. ." bytes free | last word: "
  CURRENT @ @ cell+ count type cr
  quit ;

\ Converts a bit index into a bit mask.
: #bit  ( u -- mask )  1 swap lshift ;

.( Volume 1 compiled, size:)
here host.
