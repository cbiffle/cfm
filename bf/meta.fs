\ vim: cc=64:syntax=forth
\ Virtual target bootstrap engine.

\ This is a port of the Haskell BsForth bootstrapper to
\ BsForth, using the MMU to create an efficient (if potentially
\ crashy) emulator. It's based around Frank Sergeant's model
\ from 'A 3-Instruction Forth for Embedded Systems Work'.

\ The basic idea: we'll "virtualize" a second copy of the
\ processor in an isolated section of memory, and exert total
\ control over it. We'll start by poking numbers into its
\ memory, and gradually we'll start instructing it to run
\ some of those numbers as code. In this way a complete Forth
\ environment can be brought up without ever going through a
\ traditional assembler bootstrap.

\ Any emulator would do, but we'll use the MMU.
---
\ Bootstrap: basics
vocabulary bootstrapper  bootstrapper definitions
variable tgt-base-page  8 tgt-base-page !
  \ Physical page where target image starts.
variable tgt-block  variable tgt-eblock  \ source block range
\ Cached target XTs relevant to bootstrapping:
variable tgt-asm-xt    variable tgt-compile-xt
variable tgt-docon-xt  variable tgt-catch-xt
variable tgt-find-xt
: tgt-reset  \ Reset target memory map and vars
  0 tgt-eblock ! 0 tgt-block !
  0 tgt-asm-xt ! 0 tgt-compile-xt ! 0 tgt-docon-xt !
  0 tgt-catch-xt ! 0 tgt-find-xt !
  8 begin
    1-  MMUMP io!d  tgt-base-page @ over + MMUM1 io!  ?dup 0=
  until ;
---
\ Bootstrap: target memory access.
\ We use page 6 in the host memory map as a moving window into
\ target space, like the block buffer. tgt-tran makes a target
\ address visible in the window, and returns its host virtual
\ address.
: tgt-tran  ( t-addr -- addr )
  6 MMUMP io!
  dup 13 rshift  tgt-base-page @ +  MMUM0 io!
  $1FFF and $C000 + ;
---
\ A note on conventions:

\ I'm using angle brackets to distinguish operations on state
\ inside the target. So <@> is like @ except it reads target
\ memory. This extends to stack comments; addr is an address
\ in host memory, while <addr> is an address in the target
\ and can't be safely dereferenced without <@> / tgt-tran .

\ In general, we're going to build up a subset of bsforth
\ here that acts on the target, and all these words will be
\ bracketed.
---
\ Bootstrap: Sergeant operations and friends.
: <@> tgt-tran @ ;    : <c@> tgt-tran c@ ;
: <!> tgt-tran ! ;    : <c!> tgt-tran c! ;
: <+!> tgt-tran +! ;
\ Calls <xt> in the target, giving it control over the stacks
\ until return. Once exceptions are a thing, this indirects
\ through CATCH and re-THROWs anything that goes wrong.
: <execute>  ( i*x <xt> -- j*x )
  tgt-catch-xt @ ?dup if ['] throw >r then
  \ Assemble a map switch escape sequence at target $FFFA.
  MMUCS invert $8000 or  $FFFA <!> $6600 $FFFC <!>
  $713F $FFFE <!>
  \ Arrange return addresses to jump into target, then escape
  $FFFA >r >r   MMUCS io!d ;
---
\ Initially we'll pretend there is a compiled Forth system in
\ the target, by directly manipulating certain data structures.
\ For this to work, we need to know where they are and how they
\ are shaped.
---
\ Bootstrap: layout of system and user vars in target.
 0 constant <ResetVector>  2 constant <IrqVector>
 4 constant <U0>           6 constant <RootWl>
 8 constant <DP>          10 constant <FREEZEP>
12 constant <VOC-LINK>    14 constant <#sysvars>
: tuser  create ,  does> @ <U0> <@> + ;
 0 tuser <HANDLER>         2 tuser <STATE>
 4 tuser <'SOURCE>
 8 tuser <>IN>            10 tuser <BASE>
12 tuser <CURRENT>        14 tuser <CONTEXT>
16 tuser <BLK>
: <SOURCE> <'SOURCE> dup <@> swap cell+ <@> ;
\ Lower buffers to dodge emulator trampoline area at $FFFA!
$FBFA constant <blkbuf>  $FB7A constant <initial-U0>
---
\ Bootstrap: target system/user var initialization.
: tgt-init-vars
  \ system variables
  0   <ResetVector> <!>   1        <IrqVector> <!>
  <initial-U0> <U0> <!>   0           <RootWl> <!>
  <#sysvars>   <DP> <!>   <#sysvars> <FREEZEP> <!>
  0      <VOC-LINK> <!>
  \ user area
  0        <HANDLER> <!>  0          <STATE> <!>
  0        <'SOURCE> <!>  0  <'SOURCE> cell+ <!>
  0            <>IN> <!>  10          <BASE> <!>
  <RootWl> <CURRENT> <!>  <RootWl> <CONTEXT> <!>
  0            <BLK> <!> ;
---
\ We'll feed the target text, up to and including blocks of
\ source code, by copying them into <blkbuf> . str>tgt is the
\ general form, while block>tgt acts on mass storage.
---
\ Bootstrap: mass storage block transfer
: str>tgt  ( c-addr u -- <c-addr> u )
  swap >r
  0 begin
    over over xor
  while ( u i ) ( R: c-addr )
    r@ over + c@  over <blkbuf> + <c!>
    1+
  repeat rdrop drop <blkbuf> swap ;
: block>tgt  ( n -- )
  block 1024 str>tgt 2drop ;
---
\ Bootstrap: using definitions in the target once available.

\ We can run code in the target at full speed with <execute> ,
\ which is much faster than simulating its effect with <@> and
\ friends. Once certain words become available we record their
\ addresses and use ?cached to short-circuit the host version.

\ Note that this word uses RDROP. If the given addr contains a
\ non-zero address, it discards its caller's return address,
\ effectively returning *past* the caller after calling target
\ code. This must be used carefully.

: ?cached ( addr -- it's complicated )
  @ ?dup if rdrop <execute> exit then ;
---
\ Here is a set of words for accessing the target dictionary.
\ They are a direct gloss of the corresponding words in bsforth
\ but liberally sprinkled with angle brackets.

\ Note that some words, like CELLS, do not have angle brackets.
\ We are deliberately punning the host implementation where we
\ know the two are equivalent. If the target's Forth model were
\ significantly different from the host's, we'd have to stop
\ doing that.
---
\ Bootstrap: target basics
: <here> <DP> <@> ;
: <allot> <DP> <+!> ;
: <raw,>  <here> <!> cell <allot> ;
: <c,> <here> <c!> 1 <allot> ;
: <-,> -1 cells <allot> ;
: <align> <here> aligned <DP> <!> ;
: <freeze> <here> dup <FREEZEP> <!> ;
: <,> <raw,> <freeze> drop ;
---
\ The low-level target assembler is another direct gloss from
\ bsforth (and just as ugly). Note, however, our first use of
\ ?cached . If tgt-asm-xt contains an <xt> (a target execution
\ token), the corresponding target code will be called instead
\ of the code after ?cached .
---
\ Bootstrap: target emulated assembler
: <asm,> tgt-asm-xt ?cached
  <here> <FREEZEP> <@> xor if <here> cell - <@> ( new prev )
    over $700C = if
      $F04C over and $6000 = if <-,> nip $100C or <asm,> exit
      then $E000 over and $4000 = if <-,> nip $1FFF and <asm,>
      exit then then
    over $F0FF and $6003 over = swap $6000 = or if
      over $0F00 and  dup $200 - $400 u< swap $700 = or if 
        $FFFE over and $6180 = if
          1 and + $FFF3 and dup 3 and 1 = $80 and or
            <-,> <asm,> exit then
      then then
    $6081 over = if over $6C00 = if $FF and or <-,> <asm,> exit
      then then drop
  then <raw,> ;
---
\ The next several screens are more glosses of bsforth words,
\ which I'll present without significant commentary.
---
\ Bootstrap: low-level compiler
: <literal>
  dup 0< if invert true else false then
  swap $8000 or <asm,> if $6600 <asm,> then ;
: <compile,>    tgt-compile-xt ?cached
  dup <@> $F04C and $700C = if <@> $EFF3 and
  else u2/ $4000 or
  then <asm,> ;
---
\ Bootstrap: target text processing
: <skip-while>  ( <c-addr> u xt -- <c-addr'> u' )
  >r begin over <c@> r@ execute over and
     while 1 /string repeat rdrop ;
: <scan>
  <SOURCE> <>IN> <@> /string
  over >r rot <skip-while>
  2dup 1 min +
  <'SOURCE> <@> - <>IN> <!>
  drop r> tuck - ;
: <skip>
  <SOURCE> <>IN> <@> /string
  rot <skip-while> drop
  <'SOURCE> <@> - <>IN> <!> ;
: <parse-name>  [: bl u<= ;] <skip> [: bl u> ;] <scan> ;
---
: <count> dup 1+ swap <c@> ;
: <s=>  rot over xor if drop 2drop false exit then
  begin ?dup while
    >r  over <c@> over <c@> xor if 2drop rdrop false exit then
    1+ swap 1+ r> 1-
  repeat 2drop true ;
: str>host ( <c-addr> u -- c-addr u )
  flush dup >r begin
    ?dup
  while
    1- over over + <c@> over blkbuf + c!
  repeat drop blkbuf r> ;
---
: <sfoldl>
  >r >r bounds begin
    over over xor while
      dup <c@> r> r@ execute >r
      1+
    repeat 2drop r> rdrop ;
: <s,> dup <c,> 0 [: swap <c,> ;] <sfoldl> drop <align> ;
---
: <find-in>  ( <c-addr> u <wl> -- <c-addr> u 0 | <xt> flags -1)
  begin <@> dup while
    >r  2dup  r@ cell+ <count> <s=> if
      nip  r> cell+ 1+ + aligned   dup cell+ swap <@> true exit
    then r>
  repeat ;
: <find>  tgt-find-xt ?cached
  <CONTEXT> <@> <find-in> if true exit then
  <CURRENT> <@> <find-in> ;
---
: <]> 1 <STATE> <!> ;   : <[> 0 <STATE> <!> ;
: <exit>  $700C <asm,> ;
---
\ Okay! Now things get more interesting.

\ These routines are responsible for inspecting the target's
\ wordlists to find <xt>s we wish to cache. We'll invoke
\ tgt-rescan at each block boundary. Note that once an <xt>
\ has been found, we lock it in and quit searching for it.
---
: tgt-cache!  ( c-addr u addr -- )
  dup @ if drop 2drop exit then

  >r  2dup  str>tgt <find> if  ( c-addr u <xt> flags ) ( R: a )
    drop  ." caching " dup .  r> ! type cr
  else  ( c-addr u c-addr u ) ( R: a) 2drop 2drop rdrop then ;
: tgt-rescan
  S" asm," tgt-asm-xt tgt-cache!
  S" compile," tgt-compile-xt tgt-cache!
  S" (docon)" tgt-docon-xt tgt-cache!
  S" catch" tgt-catch-xt tgt-cache!
  S" sfind" tgt-find-xt tgt-cache!
  ;
---
\ Once we are interpreting source code within the target, we
\ need to have emulated stand-ins for certain words available.
\ The set of such words required by bsforth is fairly small,
\ and mostly revolves around defining words and things like
\ comma for recording literal data.

\ We follow the muForth model, and define two lists of such
\ stand-in words. "Inside" words are available inside
\ definitions, and "outside" words are available outside.

\ Both lists are consulted as last resort *only*, so as soon
\ as a definition with the same name exists in the target, the
\ stand-in will no longer be used.
---
variable 'inside  variable 'outside
  \ Wordlist roots. These aren't VOCABULARY because they must
  \ never come into scope on the host!
: x:  CURRENT @  swap CURRENT !  :  CURRENT ! ;
  \ Gymnastics for creating a new host definition on one of our
  \ non-vocabulary wordlists, without coming into scope.
: outside: 'outside x: ;
: inside:  'inside  x: ;
---
\ <interpret> is nearly a gloss of INTERPRET from bsforth, but
\ with added code to consult 'inside or 'outside as a last
\ resort.
---
\ Bootstrap: interpret current block
: <interpret>
  <parse-name>  ?dup 0= if drop exit then   ( <c-addr> +u )
  <find> if     ( <xt> flags )
    <STATE> <@> 0= or if <execute> else <compile,> then
  else \ not found  ( <c-addr> +u )
    str>host    ( c-addr +u )
    2dup >r >r  base @ >r  <BASE> <@> base !
    ['] number catch  r> base !
    if \ failed
      2drop r> r> <STATE> <@> if 'inside else 'outside then
      find-in if drop execute else ?? then
    else
      rdrop rdrop <STATE> <@> if <literal> then
    then
  then <interpret> ;
---
\ The word we call <quit> is less general than bsforth's, as
\ it only knows about blocks -- the target has no other input
\ sources. It processes blocks in the range specified in the
\ tgt-block (inclusive) and tgt-eblock (exclusive) variables.

\ Note that it maintains <BLK> so the target "knows" it is
\ interpreting from blocks. This is critical for getting the
\ target's version of line comments to work correctly.

\ This is where tgt-rescan gets invoked, btw.
---
\ Bootstrap: outer QUIT loop
: <quit>
  tgt-block @ tgt-eblock @ = if exit then

  tgt-block @  dup ." block " u. cr
               dup block>tgt  <BLK> <!>
  <blkbuf> <'SOURCE> <!> 1024 <'SOURCE> cell+ <!> 0 <>IN> <!>
  <interpret>
  1 tgt-block +!  0 <BLK> <!>
  tgt-rescan
  <quit> ;
---
\ Now we must provide the inside and outside definitions to get
\ the target Forth off the ground. Many of these will reuse
\ bracketed words we defined for the host above. All are
\ direct glosses from bsforth.
---
\ Bootstrap: common definitions for inside + out
: <\>
  <BLK> <@> if <>IN> <@> 63 + 63 invert and <>IN> <!>
  else <SOURCE> nip <>IN> <!> then ;
: <(> [: ')' <> ;] <scan> 2drop ;
---
\ Bootstrap: outside definitions.
outside: \    <\> ;     outside: ( <(> ;   outside: host.  . ;
outside: asm, <asm,> ;  outside: , <,> ;   outside: c, <c,> ;
outside: ]    <]> ;
: <:>
  <align> <here>  <CURRENT> <@>  dup <@> <,>  <!>
  <parse-name> <s,>
  0 <,> <]> ;
outside: : <:> ;
outside: constant <:> <[>
  tgt-docon-xt @  dup 0= 1 and throw  <compile,> <,> ;
---
\ Bootstrap: bootstrap control flow words
: <mark>> <freeze> swap <asm,> ;
: <>resolve> dup <@> <freeze> u2/ or swap <!> ;
---
\ Bootstrap: inside definitions.
inside: \ <\> ;   inside: ( <(> ;    inside: [ <[> ;
inside: exit <exit> ;                inside: ; <exit> <[> ;
inside: postpone
  <parse-name> dup if
    <find> if
      if <compile,>
      else <literal>
         tgt-compile-xt @  dup 0= 1 and throw  <compile,>
      then exit
    then
  then ?? ;
inside: if $2000 <mark>> ;           inside: then <>resolve> ;
inside: else 0 <mark>> swap <>resolve> ;
---
\ The expected workflow for the bootstrapper is:
\ 1. begin-bootstrap
\ 2. x y <thru>  -- repeat as needed
\ 3. end-bootstrap
\ 4a. reboot-into  -- to test immediately
\ 4b. <save>       -- from extras.fs, to write to SPI flash
---
\ Bootstrap: entry point
: begin-bootstrap ( -- )
  ." starting" cr
  0 begin cell- $DEAD over <!>  ?dup 0= until
  tgt-reset  tgt-init-vars ;
: <thru>
  1+ tgt-eblock !  tgt-block ! <quit> ;
: end-bootstrap
  ." used: " <DP> <@> u. cr
  ;
: reboot-into
  8 begin  1-  MMUMP io!d MMUM1 io!d  ?dup 0= until
  di  MMUCS io!d
  8 begin  1-  MMUMP io!d  8 over + MMUM0 io!  ?dup 0= until
  0 >r MMUCS io!d ;
---
