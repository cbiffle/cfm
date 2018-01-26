\ vim: cc=64:syntax=forth
\ Bootstrap Forth, second volume. For larger environments.
---
\ Utilities.
: fill  ( c-addr u c -- )
  rot rot bounds do
    dup i c!
  loop drop ;
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
  create  CURRENT @ , CONTEXT @ , PATCHES @ ,
    PATCHES [: cell+ @ , false ;] traverse drop
    here cell+ ,        \ Dictionary pointer to restore.
  does>
    dup @ CURRENT !  cell+
    dup @ CONTEXT !  cell+
    dup @ PATCHES !  cell+
    PATCHES [: over @ swap cell+ ! cell+ false ;] traverse drop
    @ DP ! ;          \ restore DP, finally consuming our addr
---
\ WORDS
: words
  CURRENT @ [: name>string type space false ;] traverse drop ;
---
