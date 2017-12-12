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

\ Useful compound instructions are named for the equivalent sequence of Forth
\ words:
: 2dup_!_drop  ( x addr -- x )  [ $6123 asm, ] ;

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
4 constant LATEST  ( head of wordlist )
6 constant DP  ( dictionary pointer, read by HERE )
8 constant U0  ( address of user area )
10 constant STATE  ( compiler state )
12 constant FREEZEP  ( high-water-mark for code immune to fusion )

$FFFF constant true  ( also abused as -1 below, since it's cheaper )
0 constant false
2 constant cell


\ -----------------------------------------------------------------------------
\ More useful Forth words.

: tuck  ( a b -- b a b )  swap over ;
: !  ( x addr -- )  2dup_!_drop drop ;
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
: u2/ 1 rshift ;

\ -----------------------------------------------------------------------------
\ The Dictionary and the Optimizing Assembler.

\ Because the host manipulates the dictionary, it's important to keep the
\ layout consistent between us and the host. This is why LATEST, DP, and
\ FREEZEP are part of the system variables block.

: here  ( -- addr )  DP @ ;
: allot  DP +! ;
: raw,  here !  cell allot ;
: cells  1 lshift ;
: align  here  aligned  DP ! ;

\ Access to the CFA/xt of the most recently defined word.
: lastxt  ( -- xt )
  LATEST @  cell +      ( nfa )
  dup c@ + 1+ aligned   ( ffa )
  cell + ;

\ We've been calling the host's emulation of asm, for building words out of
\ machine code. Here's the actual definition.
: asm,
  here FREEZEP @ xor if  ( Fusion is a possibility... )
    here cell - @   ( new-inst prev-inst )

    over $700C = if ( if we're assembling a bare return instruction... )
      $F04C over and $6000 = if  ( ...on a non-returning ALU instruction )
        true cells allot
        nip  $100C or  asm, exit
      then
      $E000 over and $4000 = if  ( ...on a call )
        true cells allot
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
          true cells allot
          asm, exit
        then
      then
    then

    $6081 over = if   \ previous instruction is DUP
      over $6C00 = if   \ just @ for now, others aren't used
        $FF and or
        true cells allot
        asm, exit
      then
    then

    ( No patterns matched. )
    drop
  then
  ( Fusion was not possible, simply append the bits. )
  raw, ;

\ Sometimes we want a clear separation between one instruction and the next.
\ For example, if the second instruction is the target of control flow like a
\ loop or if. The word freeze updates FREEZEP, preventing fusion of any
\ instructions already present in the dictionary. It returns the value of here,
\ because we basically always want that when using freeze.
: freeze  ( -- addr )
  here FREEZEP 2dup_!_drop ;

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
\ LITERAL

\ Compiles code to insert a computed literal into a definition.
: literal  ( C: x -- )  ( -- x )
  dup 0 < if  ( MSB set )
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
\ Our first evolution. This jettisons the host's implementation of the XT
\ compiler and dictionary maintenance words, and switches to using the target
\ versions, thus improving performance (and ensuring correctness).

<TARGET-EVOLVE>


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
: while  ( C: dest -- orig dest )
  $2000 mark> swap ; immediate
: repeat  ( C: orig dest -- )
  $0000 <resolve
  >resolve ; immediate


\ -----------------------------------------------------------------------------
\ Dictionary search.

: rot  ( x1 x2 x3 -- x2 x3 x1 )
  >r swap r> swap ;

: bounds over + swap ;

\ Compares two strings.
: s= ( c-addr1 u1 c-addr2 u2 -- ? )
  rot over xor if drop 2drop false exit then
  ( c-addr1 c-addr2 u )
  >r  2dup -  ( c-addr1 c-addr2 1-2 ) ( R: u )
  r> swap >r  ( c-addr1 c-addr2 u ) ( R: 1-2 )
  nip         ( c-addr1 u ) ( R: 1-2 )
  bounds      ( c-addrE c-addrS ) ( R: 1-2)
  begin
    over over xor
  while
    dup c@  over r@ - c@ xor if 2drop rdrop false exit then
    1 +
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
    r@ cell +             ( c-addr u c-addr u nfa ) ( R: lfa )
    dup 1+ swap c@        ( c-addr u c-addr u c-addr u ) ( R: lfa )
    s= if                 ( c-addr u )              ( R: lfa )
      nip                 ( u )                     ( R: lfa )
      r> cell +           ( u nfa )
      1+  +  aligned      ( ffa )
      dup cell +          ( ffa cfa )
      swap @              ( cfa flags )
      true exit           ( cfa flags true )
    then    ( c-addr u ) ( R: lfa )
    r>      ( c-addr u lfa )
  repeat ;

\ Jettison the host's dictionary search code.
<TARGET-EVOLVE>


\ -----------------------------------------------------------------------------
\ More useful Forth words.

: execute  ( i*x xt -- j*x )  >r ; ( NOINLINE )
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

: 0= 0 = ;
: <> = invert ;

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
  r@ cell +
  \ Adjust the return address to skip the definition.
  r> @ 1 lshift >r
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

\ -----------------------------------------------------------------------------
\ Support for VARIABLE .

\ Because we don't need VARIABLE until much later in bootstrap, we can write
\ its code fragment more clearly than (docon) .

: (dovar) r> ;


\ -----------------------------------------------------------------------------
\ Basic source code input support and parsing.

\ Address and length of current input SOURCE.
variable 'SOURCE  cell allot
\ Returns the current input as a string.
: SOURCE  ( -- c-addr u )  'SOURCE dup @ swap cell + @ ;

\ Holds the number of characters consumed from SOURCE so far.
variable >IN

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
  bounds   ( c-addr-end c-addr-start )
  begin
    over over xor    ( cheap inequality test )
  while
    dup c@ c,
    1+
  repeat
  2drop align ;

\ Implementation factor of the other defining words: parses a name and creates
\ a header, without generating any code.
: (CREATE)
  ( link field )
  align here  LATEST @ ,  LATEST !
  ( name )
  parse-name s,
  ( flags )
  0 , ;

<TARGET-EVOLVE>
  \ Cause the host to notice 'SOURCE and >IN, which enables the use of target
  \ parsing words. Because our definitions for CONSTANT , VARIABLE , and : are
  \ about to shadow the host emulated versions, this support is important!

TARGET-PARSER: :

: :  (CREATE) ] ;
  \ Note that this definition gets used immediately.

TARGET-PARSER: create

: create
  (CREATE)
  postpone (dovar) ;

TARGET-PARSER: constant

: constant
  (CREATE)
  postpone (docon)
  , ;

TARGET-PARSER: variable

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
      lastxt  r> u2/ $4000 or  swap !
  ;] compile, freeze drop
  \ Control will reach this point from the call instruction at the start of the
  \ code field. We need to reveal the parameter field address by postponing r>
  postpone r>
  ; immediate

variable #user
  \ Holds the number of user variables that have been defined.
TARGET-PARSER: user
: user
  create  #user @ cells ,  1 #user +!
  does> @  U0 @ + ;


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

: key 'key @ execute ;
: emit 'emit @ execute ;

$20 constant bl
: space bl emit ;
: beep 7 emit ;

: cr $D emit $A emit ;
  \ This assumes a traditional terminal and is a candidate for vectoring.

: type  ( c-addr u -- )
  bounds
  begin
    over over xor
  while
    dup c@ emit
    1+
  repeat 2drop ;

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
        0  \ "key" for code above
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
        1 -
      else    \ At start of line
        beep
      then
      0
    then

    $D =
  until
  rdrop nip ;

: .
  begin
    dup  12 rshift  9 over u< 7 and +  $30 +  emit
    4 lshift
    dup 0=
  until drop space ;

\ -----------------------------------------------------------------------------
\ Text interpreter.

variable 'ABORT
: ABORT  'ABORT @ execute ;

: u*
  >r 0    ( a 0 ) ( R: b )
  begin
    over
  while
    r@ 1 and if over + then
    swap 1 lshift swap
    r> 1 rshift >r
  repeat
  rdrop nip ;

user base  16 base !

: digit  ( c -- x )
  $30 -
  9 over u< 7 and -
  base @ 1 - over u< if ABORT then ;

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

: number  ( c-addr u -- x )
  0 [: base @ u*  swap digit + ;] sfoldl ;

: interpret
  begin
    parse-name
  dup while
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
      number
      STATE @ if
        postpone literal
      then
    then
  repeat
  2drop ;

\ -----------------------------------------------------------------------------
\ Parsing words and target syntax.

\ These can't use the TARGET-PARSER: support, because they are "partial
\ functions" of the input text -- that is, there are input sequences that need
\ to trigger failure, but the target has no good way of indicating that at this
\ point. Thus we use TARGET-MASK: instead to keep using the host emulation.

TARGET-MASK: '
: '  ( "name" -- xt )
  parse-name dup if
    sfind if  ( xt flags )
      drop exit
    then
    \ Got input, but the input was bogus:
    type
    $3F emit
    cr
  then \ Bogus input or end-of-input:
  ABORT ;

TARGET-MASK: \
\ Line comments simply discard the rest of input.
: \
  SOURCE nip >IN ! ;  immediate

TARGET-MASK: (
\ Block comments look for a matching paren.
: (
  SOURCE  >IN @  /string  ( c-addr u )
  [: $29 <> ;] skip-while  ( c-addr' u' )
  1 min +  \ consume the trailing paren
  'SOURCE @ -  >IN ! ;  immediate

: S"
  SOURCE  >IN @  /string
  over >r
  [: $22 <> ;] skip-while
  2dup  1 min +  'SOURCE @ -  >IN !
  drop r> tuck -

  [:  ( -- c-addr u )
      \ Uses its return address to locate a string literal. Pushes the
      \ literal onto the stack and updates the return address to skip
      \ it.
      r>        ( addr )
      dup 1+
      swap c@   ( c-addr u )
      over over + aligned  ( c-addr u end )
      >r
  ;] compile,
  s, ;  immediate

TARGET-MASK: postpone
: postpone
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
    type
    $3F emit
    cr
  then
  ABORT ;


\ -----------------------------------------------------------------------------
\ END OF GENERAL KERNEL CODE
\ -----------------------------------------------------------------------------
<TARGET-EVOLVE>  \ Clear stats on host emulated word usage.
.( After compiling general-purpose code, HERE is... )
here host.




( ----------------------------------------------------------- )
( Icestick SoC support code )

: #bit  1 swap lshift ;

( ----------------------------------------------------------- )
( Interrupt Controller )

$F000 constant IRQST  ( status / enable trigger )
\ $F002 constant IRQEN  ( enable )
$F004 constant IRQSE  ( set enable )
$F006 constant IRQCE  ( clear enable )

( Atomically enables interrupts and returns. This is intended to be tail )
( called from the end of an ISR. )
: ei  IRQST 2dup_!_drop ;

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

$A000 constant IN

( ----------------------------------------------------------- )
( Timer )

$C000 constant TIMV
$C002 constant TIMF
$C004 constant TIMM0
$C006 constant TIMM1

( ----------------------------------------------------------- )
( UART emulation )

.( Compiling soft UART... )
here

2500 constant cyc/bit
1250 constant cyc/bit/2

variable uart-tx-bits
  \ Holds bits as they're shifted out.
  \ Because the final bit to be shifted out, the stop bit, is a 1,
  \ this also serves as a "transmitter available" indicator when 0.

: tx-isr
  \ Acknowledge interrupt by writing 1 to bit 0 of TIMF.
  \ Keep the 1 around; we're going to use it to save cycles.
  1 TIMF 2dup_!_drop
  \ Read the transmit shift register, and derive its LSB using the 1.
  uart-tx-bits @  over over and  ( 1 bits lsb )
  if OUTSET else OUTCLR then  ( 1 bits regaddr )
  1 swap !  ( 1 bits )
  swap rshift   ( bits' )
  uart-tx-bits 2dup_!_drop  ( bits' )

  if  \ bits remain
    TIMV @ cyc/bit + TIMM1 !
  else  \ bits all gone
    irq#m1 irq-off
  then ;

: tx
  ( Wait for transmitter to be free )
  begin uart-tx-bits @ 0= until
  ( Frame the byte )
  1 lshift
  $200 or
  uart-tx-bits !
  irq#m1 irq-on ;

variable uart-rx-bits

8 constant uart-#rx
variable uart-rx-buf  uart-#rx 1 - cells allot
variable uart-rx-hd
variable uart-rx-tl

: CTSon 2 OUTCLR ! ;
: CTSoff 2 OUTSET ! ;

: rxq-empty? uart-rx-hd @ uart-rx-tl @ = ;
: rxq-full? uart-rx-hd @ uart-rx-tl @ - uart-#rx = ;

( Inserts a cell into the receive queue. This is intended to be called from )
( interrupt context, so if it encounters a queue overrun, it simply drops )
( data. )
: >rxq
  rxq-full? if
    drop
  else
    uart-rx-buf  uart-rx-hd @ [ uart-#rx 1 - 1 lshift ] literal and +  !
    2 uart-rx-hd +!
  then ;

( Takes a cell from the receive queue. If the queue is empty, spin. )
: rxq>
  begin rxq-empty? 0= until
  uart-rx-buf  uart-rx-tl @ [ uart-#rx 1 - 1 lshift ] literal and +  @
  2 uart-rx-tl +! ;

: uart-rx-init
  \ Clear any pending negedge condition
  0 IN !
  \ Enable the initial negedge ISR to detect the start bit.
  irq#negedge irq-on
  CTSon ;

\ Triggered when we're between frames and RX drops.
: rx-negedge-isr
  \ Set up the timer to interrupt us again halfway into the start bit.
  \ First, the timer may have rolled over while we were waiting for a new
  \ frame, so clear its pending interrupt status.
  2 TIMF !
  \ Next set the match register to the point in time we want.
  TIMV @  cyc/bit/2 +  TIMM0 !
  \ We don't need to clear the IRQ condition, because we won't be re-enabling
  \ it any time soon. Mask our interrupt.
  irq#negedge irq-off

  \ Prepare to receive a ten bit frame.
  10 #bit uart-rx-bits !

  \ Now enable its interrupt.
  irq#m0 irq-on ;

\ Triggered at each sampling point during an RX frame.
: rx-timer-isr
  \ Sample the input port into the high bit of a word.
  IN @  15 lshift
  \ Reset the timer for the next sample point.
  TIMV @  cyc/bit +  TIMM0 !
  \ Load this into the frame shift register.
  uart-rx-bits @  u2/  or  uart-rx-bits 2dup_!_drop
  \ Check the LSB to see if we're done.
  1 and if  \ all done
    irq#m0 irq-off
    \ Enqueue the received frame
    uart-rx-bits @ >rxq
    \ Clear any pending negedge condition
    0 IN !
    \ Enable the initial negedge ISR to detect the start bit.
    irq#negedge irq-on
    \ Conservatively deassert CTS to try and stop sender.
    CTSoff
  else  \ more bits to receive
    \ Clear the interrupt condition.
    2 TIMF !
  then ;

\ Receives a byte from RX, returning the bits and a valid flag. The valid flag may
\ be false in the event of a framing error.
: rx  ( -- c ? )
  rxq>

  \ Dissect the frame and check for framing error. The frame is in the
  \ upper bits of the word.
  6 rshift
  dup u2/ $FF and   \ extract the data bits
  swap $201 and          \ extract the start/stop bits.
  $200 =                 \ check for valid framing
  rxq-empty? if CTSon then  \ allow sender to resume if we've emptied the queue.
  ;


( ----------------------------------------------------------- )
( Icestick board features )

: ledtog  4 + #bit OUTTOG ! ;


( ----------------------------------------------------------- )
( Demo wiring below )

: delay 0 begin 1+ dup 0= until drop ;

: isr
  IRQST @
  $4000 over and if
    rx-timer-isr
  then
  $8000 over and if
    rx-negedge-isr
  then
  $2000 over and if
    tx-isr
  then
  drop
  r> 2 - >r
  ei ;

create TIB 80 allot

: rx! rx 0= if rx! exit then ;

: quit
  postpone [
  begin
    TIB 'SOURCE !
    80  'SOURCE cell + !
    0 >IN !
    SOURCE accept  'SOURCE cell + !
    space
    interpret
    STATE @ 0= if  
      $6F emit $6B emit
    then
    cr
  again ;

' tx 'emit !
' rx! 'key !
' quit 'ABORT !

: cold
  1 OUTSET !   \ raise TX line soon after reset
  uart-rx-init
  ei
  16 base !
  35 emit
  LATEST @ cell + 1 + 4 type
  35 emit
  quit ;

( install cold as the reset vector )
' cold  u2/  0 !
( install isr as the interrupt vector )
' isr  u2/  2 !
( adjust U0 to mapped RAM for the Icestick )
$1700 U0 !

.( Compilation complete. HERE is... )
here host.
