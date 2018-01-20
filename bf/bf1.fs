\ vim: cc=64:syntax=forth
\ Bootstrap Forth, second volume. For larger environments.
---
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
---
\ REMARKER Creates a word that marks a point in the dictionary.
\ When executed, the word will restore the dictionary, search
\ order, and vocabularies to the state they had immediately
\ after the word was defined (i.e. the word will become the
\ last defined word again). This is a variant on ANS MARKER,
\ which destroys itself and is slightly harder to implement.
---
\ REMARKER
: remarker  ( "name" -- )
  create  CURRENT @ , CONTEXT @ , PATCHES @ dup ,
    begin ?dup while   dup cell+ @ ,  @ repeat
    here cell+ ,        \ Dictionary pointer to restore.
  does>
    dup @ CURRENT !  cell+
    dup @ CONTEXT !  cell+
    dup @ dup PATCHES !  \ Restore PATCHES keeping a copy
    swap cell+ swap
    begin ?dup while      \ For each patch location,
      ( dict-addr link-addr )
      over @ over cell+ !     \ restore its value,
      swap cell+ swap @
    repeat
    @ DP ! ;          \ restore DP, finally consuming our addr
---
\ WORDS
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
---
