\ Bootstrap Forth, second volume. For larger environments.

\ -----------------------------------------------------------------------------
\ Luxury overrides

: \
  blk @ if
    >IN @ 63 + 63 invert and >IN !
  else
    SOURCE nip >IN !
  then ;  immediate

\ -----------------------------------------------------------------------------
\ Utilities.

: fill  ( c-addr u c -- )
  >r
  bounds begin
    over over xor
  while
    r@ over c!
    1+
  repeat
  rdrop 2drop ;

\ -----------------------------------------------------------------------------
\ Programming tools.

\ Creates a word that marks a point in the dictionary. When executed, the word
\ will restore the dictionary, search order, and vocabularies to the state they
\ had immediately after the word was defined (i.e. the word will become the
\ last defined word again). This is a variant on ANS MARKER, which destroys
\ itself and is slightly harder to implement.
: remarker  ( "name" -- )
  create
    \ Store as much as possible in the fixed-size region,
    \ to keep me sane while debugging.
    CURRENT @ ,         \ Identify CURRENT wordlist.
    CONTEXT @ ,         \ Identify CONTEXT wordlist.
    VOC-LINK @ dup ,    \ Identify most recent vocab.
    begin               \ For all defined vocabs,
      ?dup
    while
      dup cell+ @ ,     \ Record the current head.
      @
    repeat
    here cell+ ,        \ Dictionary pointer to restore.
  does>
    dup @ CURRENT !     \ Restore CURRENT
    cell+
    dup @ CONTEXT !     \ Restore CONTEXT
    cell+
    dup @ dup VOC-LINK !  \ Restore VOC-LINK keeping a copy
    swap cell+ swap
    begin                 \ For each known vocab,
      ?dup
    while ( dict-addr link-addr )
      over @ over cell+ !     \ restore the head.
      swap cell+ swap @ \ advance both marker address and
                        \ position in the vocab list.
    repeat
    @ DP ! ;            \ restore DP, finally consuming our addr

: words
  CURRENT @
  begin
    @ dup
  while
    2 over +  \ compute address of name field
    count     \ convert to counted string
    type space
  repeat
  drop ;

.( Volume 2 compiled, size:)
here host.
