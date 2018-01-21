\ vim: cc=64:syntax=forth
\ Extras package for BsForth.

\ I find this stuff useful, but I'm not sure it's ready for
\ prime time -- so it isn't in the main distribution.
---
\ A rudimentary editor.                                     1/1
\ n m rl   starts a line editor to replace line m of block n
: rl  ( blk line -- )
  6 lshift swap block + 64 ( c-addr u )
  2dup accept /string bl fill update ;
\ n rb  requests sixteen lines to completely replace block n
: rb ( blk -- )
  0 begin  cr dup .  2dup rl  1+  16 over = until 2drop ;
---
\ SPI flash access - fundamentals and reading.              1/2
: sf.sel  ( i*x xt -- j*x )
  $800 OUTCLR io!  catch  $800 OUTSET io!  throw ;
: sf.bit  ( data -- data' )
  $400 over 15 rshift if OUTSET else OUTCLR then io!
  1 lshift  $200 OUTSET io!  IN io@ 5 rshift 1 and or
  $200 OUTCLR io! ;
: >sf>  ( c -- c' )  8 lshift sf.bit sf.bit sf.bit sf.bit
                              sf.bit sf.bit sf.bit sf.bit ;
: >sf ( c -- ) >sf> drop ;  : sf> ( -- c ) 0 >sf> ;
: sf.rd  ( c-addr u alo ahi -- ) [:
  $03 >sf  >sf  dup 8 rshift >sf  >sf
  bounds begin over over xor while sf> over c! 1+ repeat 2drop
  ;] sf.sel ;
---
\ SPI flash access - program, erase, save dictionary.       2/2
: sf.wren [: $06 >sf ;] sf.sel ;
: sf.wait begin [: $05 >sf sf> ;] sf.sel 1 and while repeat ;
: sf.prog256  ( a-addr page# -- )  sf.wren [:
  $02 >sf  dup 8 rshift >sf  >sf  0 >sf
  256 bounds begin  over over xor  while
    dup c@ >sf  1+  repeat  2drop
  ;] sf.sel sf.wait ;
: sf.erase64k  ( page# -- )  dup $FF and if 1 throw then
  sf.wren [: $D8 >sf dup 8 rshift >sf >sf 0 >sf ;] sf.sel
  sf.wait ;
: save  $0400 sf.erase64k  here 255 + 8 rshift  0
  begin over over xor while
    dup 8 lshift  over $0400 +  sf.prog256  1+ repeat 2drop ;
---
\ SPI flash access - bootstrapper integration (optional)    1/1
: sf.<prog256>  sf.wren  [:
  $02 >sf  dup 8 rshift >sf  >sf  0 >sf
  256 bounds begin  over over xor  while
    dup <c@> >sf  1+  repeat 2drop
  ;] sf.sel sf.wait ;
: <save>
  $0400 sf.erase64k  <here> 255 + 8 rshift  0
  begin over over xor while
    dup 8 lshift over $0400 +  sf.<prog256>  1+ repeat 2drop ;
---
\ Reloader: copy an image from hi to lo memory and reboot.  1/1
here  $3B00 DP ! ]
$3B00 begin 2 - dup $4000 + @ swap !a dup 0 = until >r ; DP !
: reload $3B00 >r ;

( Note: $3B00 is toward the top of branch range )
---
\ XMODEM himem loader: reads an image into hi memory        1/1
\ Use with reload from previous screen.
\ TODO: share more code with other XMODEM loader.
: xhi  $15 tx
  begin rx!
    4 over = if drop $06 tx exit then
    1 <> if 1 throw then
    rx! rx! over 255 swap - <> if 2 throw then
    1- dup . 7 lshift $4000 + 128 bounds 0 >r begin
      over over xor
    while rx! 2dup swap c! r> + >r 1+ repeat 2drop
    rx! r> $FF and <> if '?' emit $15 tx else $06 tx then again
;
---
\ XMODEM block loader - first parts                         1/2
: xblk#  ( -- u )
  rx! rx!  over 255 swap - <>  2 and throw  1- $FF and ;
: >xraddr  ( blk i -- c-addr )
  swap over 3 rshift +  block   swap 7 and 7 lshift + ;
: xblk  ( blk -- blk' )
  xblk#  dup .   2dup >xraddr  ( blk i c-addr )
  0 >r 128 bounds begin ( blk i c-addrE c-addr ) ( R: cksum )
    over over xor
  while  rx!  2dup swap c!  r> + >r  1+
  repeat 2drop  ( blk i )  ( R: checksum )
  rx! r> $FF and <> if '?' emit  drop  $15
                    else update  $FF = 32 and + $06
                  then tx ;
---
\ XMODEM block loader - final parts                         2/2
\ Downloads a file to consecutive blocks.
\ Use like:   <blkno> xrecv
: xrecv   ( blk -- )
  $15 tx  \ start of transmission request
  begin
    rx! \ block type
    4 over = if  drop $06 tx drop exit  then  \ EOT
    1 <> if 1 throw then  \ otherwise we expect a block.
    xblk
  again ;
---
