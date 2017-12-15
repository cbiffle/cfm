{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Target.Stub where

import Clash.Sized.Vector (Vec(..))
import qualified Clash.Sized.Vector as V
import Data.List (foldl')

import CFM.Types
import Assembler (asmQQ)

debugStub :: Vec 8192 Cell
debugStub = foldl' (\v (a, i) -> V.replace a i v)
                   (V.repeat 0xDEAD) $
            zip [0 :: Int ..] $
            [asmQQ|
            ( Primitive ALU instruction definitions )
            0x6180 alu: swap          ( a b -- b a )
            0x6020 alu: 2dup/!        ( a b -- a b )
            0x6381 alu: 2dup/and      ( a b -- a b a&b )
            0x6103 alu: drop          ( x -- )
            0x6600 alu: invert        ( x -- ~x )
            0x6203 alu: +             ( a b -- a+b)
            0x6a03 alu: -             ( a b -- a-b)
            0x6903 alu: rshift        ( a b -- a>>b )
            0x6d03 alu: lshift        ( a b -- a<<b )
            0x6303 alu: and           ( a b -- a&b)
            0x6403 alu: or            ( a b -- a|b)
            0x6081 alu: dup           ( x -- x x )
            0x6c00 alu: @             ( x -- [x] )
            0x6703 alu: =             ( a b -- a=b )
            0x6f03 alu: u<            ( a b -- a<b )
            0x6803 alu: <             ( a b -- a<b )
            0x6181 alu: over          ( a b -- a b a )
            0x6e81 alu: depth         ( a b -- a b a )
            0x6147 alu: >r            ( a --  R: -- a )
            0x6b8d alu: r>            ( -- a  R: a -- )

            0x3E00 org  ( leave most of RAM available )

            variable last-error
            : err? last-error @ ;

            0x8000 constant in-ready?   ( reads non-zero when a word awaits )
            0x8002 constant in-value    ( reads as last word from host, clears in-ready)
            0x8004 constant out-ready?  ( reads non-zero when no outgoing word waits )
            0x8006 constant out-value   ( writes go to host )

            : execute  ( i*x xt -- j*x )  >r ;

            : >host  ( x -- )
              begin out-ready? @ until  ( spin until FIFO available )
              out-value ! ;             ( send word )

            : host>  ( -- x )
              begin in-ready? @ until   ( spin until word available )
              in-value @ ;              ( receive word )

            : debug-loop
              host>
              0 over = if drop  ( peek )
                host>
                err? dup >host
                if drop
                else @ >host
                then
                debug-loop exit
              then
              1 over = if drop  ( poke )
                host> host>
                err? if drop drop
                else !
                then
                debug-loop exit
              then
              2 over = if drop  ( push )
                host>
                err? if drop then
                debug-loop exit
              then
              3 over = if drop  ( pop )
                err? dup >host
                if ( nothing )
                else >host
                then
                debug-loop exit
              then
              4 over = if drop  ( execute )
                host>
                err?
                if drop
                else execute
                then
                debug-loop exit
              then

              drop
              0xFFFF last-error !
              debug-loop ;

            : debug
              debug-loop ;

            0 org
            : reset debug ;|]
