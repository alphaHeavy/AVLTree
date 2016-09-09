{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Internals.HJoin
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- Functions for joining AVL trees of known height.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Internals.HJoin
        ( spliceH,joinH,joinH',
        ) where

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.Push(pushL,pushR)
import Data.Tree.AVL.Internals.HPush(pushHL_,pushHR_)
import Data.Tree.AVL.Internals.DelUtils(popRN,popRZ,popRP,popLN,popLZ,popLP)

#if __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | Join two trees of known height, returning an AVL tree.
-- It's OK if heights are relative (I.E. if they share same fixed offset).
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
joinH'
       :: AVL e -> UINT -> AVL e -> UINT -> AVL e
joinH' l hl r hr
                 = if IS_TRUE(hl LEQ hr)
                                then let d = SUBINT(hr,hl) in joinHL d l r
                                else let d = SUBINT(hl,hr) in joinHR d l r

-- hr >= hl, join l to left subtree of r.
-- Int argument is absolute difference in tree height, hr-hl (>=0)
{-# INLINE joinHL #-}
joinHL :: UINT -> AVL e -> AVL e -> AVL e
joinHL _  E           r = r                                                  -- l was empty
joinHL d (N ll le lr) r = case popRN ll le lr of
                          UBT2(l_,e) -> case l_ of
                                        E       -> error "joinHL: Bug0"       -- impossible if BF=-1
                                        Z _ _ _ -> spliceL l_ e INCINT1(d) r  -- hl2=hl-1
                                        _       -> spliceL l_ e         d  r  -- hl2=hl
joinHL d (Z ll le lr) r = case popRZ ll le lr of
                          UBT2(l_,e) -> case l_ of
                                        E       -> e `pushL` r               -- l had only one element
                                        _       -> spliceL l_ e d  r         -- hl2=hl
joinHL d (P ll le lr) r = case popRP ll le lr of
                          UBT2(l_,e) -> case l_ of
                                        E       -> error "joinHL: Bug1"      -- impossible if BF=+1
                                        Z _ _ _ -> spliceL l_ e INCINT1(d) r -- hl2=hl-1
                                        _       -> spliceL l_ e         d  r -- hl2=hl


-- hl >= hr, join r to right subtree of l.
-- Int argument is absolute difference in tree height, hl-hr (>=0)
{-# INLINE joinHR #-}
joinHR :: UINT -> AVL e -> AVL e -> AVL e
joinHR _ l  E           = l                                    -- r was empty
joinHR d l (N rl re rr) = case popLN rl re rr of
                          UBT2(e,r_) -> case r_ of
                                        E       -> error "joinHR: Bug0"      -- impossible if BF=-1
                                        Z _ _ _ -> spliceR r_ e INCINT1(d) l -- hr2=hr-1
                                        _       -> spliceR r_ e         d  l -- hr2=hr
joinHR d l (Z rl re rr) = case popLZ rl re rr of
                          UBT2(e,r_) -> case r_ of
                                        E       -> l `pushR` e            -- r had only one element
                                        _       -> spliceR r_ e d l       -- hr2=hr
joinHR d l (P rl re rr) = case popLP rl re rr of
                          UBT2(e,r_) -> case r_ of
                                        E       -> error "joinHL: Bug1"      -- impossible if BF=+1
                                        Z _ _ _ -> spliceR r_ e INCINT1(d) l -- hr2=hr-1
                                        _       -> spliceR r_ e         d  l -- hr2=hr
-----------------------------------------------------------------------
--------------------------- joinH' Ends Here --------------------------
-----------------------------------------------------------------------

-- | Join two AVL trees of known height, returning an AVL tree of known height.
-- It's OK if heights are relative (I.E. if they share same fixed offset).
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
joinH :: AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
joinH l hl r hr =
 case COMPAREUINT hl hr of
 -- hr > hl
 LT -> case l of
       E          -> UBT2(r,hr)
       N ll le lr -> case popRN ll le lr of
                     UBT2(l_,e) -> case l_ of
                                   Z _ _ _ -> spliceHL l_ DECINT1(hl) e r hr -- dH=-1
                                   _       -> spliceHL l_         hl  e r hr -- dH= 0
       Z ll le lr -> case popRZ ll le lr of
                     UBT2(l_,e) -> case l_ of
                                   E       -> pushHL_ l r hr                  -- l had only 1 element
                                   _       -> spliceHL l_         hl  e r hr -- dH=0
       P ll le lr -> case popRP ll le lr of
                     UBT2(l_,e) -> case l_ of
                                   Z _ _ _ -> spliceHL l_ DECINT1(hl) e r hr -- dH=-1
                                   _       -> spliceHL l_         hl  e r hr -- dH= 0
 -- hr = hl
 EQ -> case l of
       E          -> UBT2(l,hl)              -- r must be empty too, don't use emptyAVL!
       N ll le lr -> case popRN ll le lr of
                     UBT2(l_,e) -> case l_ of
                                   Z _ _ _ -> spliceHL l_ DECINT1(hl) e r hr -- dH=-1
                                   _       -> UBT2(Z l_ e r, INCINT1(hr))    -- dH= 0
       Z ll le lr -> case popRZ ll le lr of
                     UBT2(l_,e) -> case l_ of
                                   E       -> pushHL_ l r hr                 -- l had only 1 element
                                   _       -> UBT2(Z l_ e r, INCINT1(hr))    -- dH= 0
       P ll le lr -> case popRP ll le lr of
                     UBT2(l_,e) -> case l_ of
                                   Z _ _ _ -> spliceHL l_ DECINT1(hl) e r hr -- dH=-1
                                   _       -> UBT2(Z l_ e r, INCINT1(hr))    -- dH= 0
 -- hl > hr
 GT -> case r of
       E          -> UBT2(l,hl)
       N rl re rr -> case popLN rl re rr of
                     UBT2(e,r_) -> case r_ of
                                   Z _ _ _ -> spliceHR l hl e r_ DECINT1(hr) -- dH=-1
                                   _       -> spliceHR l hl e r_         hr  -- dH= 0
       Z rl re rr -> case popLZ rl re rr of
                     UBT2(e,r_) -> case r_ of
                                   E       -> pushHR_ l hl r                 -- r had only 1 element
                                   _       -> spliceHR l hl e r_ hr          -- dH=0
       P rl re rr -> case popLP rl re rr of
                     UBT2(e,r_) -> case r_ of
                                   Z _ _ _ -> spliceHR l hl e r_ DECINT1(hr) -- dH=-1
                                   _       -> spliceHR l hl e r_         hr  -- dH= 0


-- | Splice two AVL trees of known height using the supplied bridging element.
-- That is, the bridging element appears \"in the middle\" of the resulting AVL tree.
-- The elements of the first tree argument are to the left of the bridging element and
-- the elements of the second tree are to the right of the bridging element.
--
-- This function does not require that the AVL heights are absolutely correct, only that
-- the difference in supplied heights is equal to the difference in actual heights. So it's
-- OK if the input heights both have the same unknown constant offset. (The output height
-- will also have the same constant offset in this case.)
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
spliceH :: AVL e -> UINT -> e -> AVL e -> UINT -> UBT2(AVL e,UINT)
-- You'd think inlining this function would make a significant difference to many functions
-- (such as set operations), but it doesn't. It makes them marginally slower!!
spliceH l hl b r hr =
 case COMPAREUINT hl hr of
 LT -> spliceHL l hl b r hr
 EQ -> UBT2(Z l b r, INCINT1(hl))
 GT -> spliceHR l hl b r hr

-- Splice two trees of known relative height where hr>hl, using the supplied bridging element,
-- returning another tree of known relative height.
spliceHL :: AVL e -> UINT -> e -> AVL e -> UINT -> UBT2(AVL e,UINT)
spliceHL l hl b r hr = let d = SUBINT(hr,hl)
                       in if IS_TRUE(d EQL L(1))
                                        then UBT2(N l b r, INCINT1(hr))
                                        else spliceHL_ hr d l b r

-- Splice two trees of known relative height where hl>hr, using the supplied bridging element,
-- returning another tree of known relative height.
spliceHR :: AVL e -> UINT -> e -> AVL e -> UINT -> UBT2(AVL e,UINT)
spliceHR l hl b r hr = let d = SUBINT(hl,hr)
                       in if IS_TRUE(d EQL L(1))
                                        then UBT2(P l b r, INCINT1(hl))
                                        else spliceHR_ hl d l b r

-- Splice two trees of known relative height where hr>hl+1, using the supplied bridging element,
-- returning another tree of known relative height. d >= 2
{-# INLINE spliceHL_ #-}
spliceHL_ :: UINT -> UINT -> AVL e -> e -> AVL e -> UBT2(AVL e,UINT)
spliceHL_ _  _ _ _  E           = error "spliceHL_: Bug0"          -- impossible if hr>hl
spliceHL_ hr d l b (N rl re rr) = let r_ = spliceLN l b DECINT2(d) rl re rr
                                  in  r_ `seq` UBT2(r_,hr)
spliceHL_ hr d l b (Z rl re rr) = let r_ = spliceLZ l b DECINT1(d) rl re rr
                                  in case r_ of
                                     E       -> error "spliceHL_: Bug1"
                                     Z _ _ _ -> UBT2(r_,        hr )
                                     _       -> UBT2(r_,INCINT1(hr))
spliceHL_ hr d l b (P rl re rr) = let r_ = spliceLP l b DECINT1(d) rl re rr
                                  in  r_ `seq` UBT2(r_,hr)

-- Splice two trees of known relative height where hl>hr+1, using the supplied bridging element,
-- returning another tree of known relative height. d >= 2 !!
{-# INLINE spliceHR_ #-}
spliceHR_ :: UINT -> UINT -> AVL e -> e -> AVL e -> UBT2(AVL e,UINT)
spliceHR_ _  _  E           _ _ = error "spliceHR_: Bug0"          -- impossible if hl>hr
spliceHR_ hl d (N ll le lr) b r = let l_ = spliceRN r b DECINT1(d) ll le lr
                                  in  l_ `seq` UBT2(l_,hl)
spliceHR_ hl d (Z ll le lr) b r = let l_ = spliceRZ r b DECINT1(d) ll le lr
                                  in case l_ of
                                     E       -> error "spliceHR_: Bug1"
                                     Z _ _ _ -> UBT2(l_,        hl )
                                     _       -> UBT2(l_,INCINT1(hl))
spliceHR_ hl d (P ll le lr) b r = let l_ = spliceRP r b DECINT2(d) ll le lr
                                  in  l_ `seq` UBT2(l_,hl)
-----------------------------------------------------------------------
-------------------------- spliceH Ends Here --------------------------
-----------------------------------------------------------------------

-- hr >= hl, splice s to left subtree of r, using b as the bridge
-- The Int argument is the absolute difference in tree height, hr-hl (>=0)
spliceL :: AVL e -> e -> UINT -> AVL e -> AVL e
spliceL s b L(0) r           = Z s b r
spliceL s b L(1) r           = N s b r
spliceL s b d   (N rl re rr) = spliceLN s b DECINT2(d) rl re rr   -- height diff of rl is two less
spliceL s b d   (Z rl re rr) = spliceLZ s b DECINT1(d) rl re rr   -- height diff of rl is one less
spliceL s b d   (P rl re rr) = spliceLP s b DECINT1(d) rl re rr   -- height diff of rl is one less
spliceL _ _ _    E           = error "spliceL: Bug0"              -- r can't be empty

-- Splice into left subtree of (N l e r), height cannot change as a result of this
spliceLN :: AVL e -> e -> UINT -> AVL e -> e -> AVL e -> AVL e
spliceLN s b L(0) l           e r = Z (Z s b l) e r                                             -- dH=0
spliceLN s b L(1) l           e r = Z (N s b l) e r                                             -- dH=0
spliceLN s b d   (N ll le lr) e r = let l_ = spliceLN s b DECINT2(d) ll le lr in l_ `seq` N l_ e r
spliceLN s b d   (Z ll le lr) e r = let l_ = spliceLZ s b DECINT1(d) ll le lr
                                    in case l_ of
                                       Z _ _ _ -> N l_ e r                                      -- dH=0
                                       P _ _ _ -> Z l_ e r                                      -- dH=0
                                       _       -> error "spliceLN: Bug0"                        -- impossible
spliceLN s b d   (P ll le lr) e r = let l_ = spliceLP s b DECINT1(d) ll le lr in l_ `seq` N l_ e r
spliceLN _ _ _    E           _ _ = error "spliceLN: Bug1"                                      -- impossible

-- Splice into left subtree of (Z l e r), Z->P if dH=1, Z->Z if dH=0
spliceLZ :: AVL e -> e -> UINT -> AVL e -> e -> AVL e -> AVL e
spliceLZ s b L(1) l           e r = P (N s b l) e r                                                -- Z->P, dH=1
spliceLZ s b d   (N ll le lr) e r = let l_ = spliceLN s b DECINT2(d) ll le lr in l_ `seq` Z l_ e r -- Z->Z, dH=0
spliceLZ s b d   (Z ll le lr) e r = let l_ = spliceLZ s b DECINT1(d) ll le lr
                                    in case l_ of
                                       Z _ _ _ -> Z l_ e r                                      -- Z->Z, dH=0
                                       P _ _ _ -> P l_ e r                                      -- Z->P, dH=1
                                       _       -> error "spliceLZ: Bug0"                        -- impossible
spliceLZ s b d   (P ll le lr) e r = let l_ = spliceLP s b DECINT1(d) ll le lr in l_ `seq` Z l_ e r -- Z->Z, dH=0
spliceLZ _ _ _    E           _ _ = error "spliceLZ: Bug1"                                      -- impossible

-- Splice into left subtree of (P l e r), height cannot change as a result of this
spliceLP :: AVL e -> e -> UINT -> AVL e -> e -> AVL e -> AVL e
spliceLP s b L(1) (N ll le lr) e r = Z (P s b ll) le (Z lr e r)                                     -- dH=0
spliceLP s b L(1) (Z ll le lr) e r = Z (Z s b ll) le (Z lr e r)                                     -- dH=0
spliceLP s b L(1) (P ll le lr) e r = Z (Z s b ll) le (N lr e r)                                     -- dH=0
spliceLP s b d    (N ll le lr) e r = let l_ = spliceLN s b DECINT2(d) ll le lr in l_ `seq` P l_ e r -- dH=0
spliceLP s b d    (Z ll le lr) e r = spliceLPZ s b DECINT1(d) ll le lr e r                          -- dH=0
spliceLP s b d    (P ll le lr) e r = let l_ = spliceLP s b DECINT1(d) ll le lr in l_ `seq` P l_ e r -- dH=0
spliceLP _ _ _     E           _ _ = error "spliceLP: Bug0"

-- Splice into left subtree of (P (Z ll le lr) e r)
{-# INLINE spliceLPZ #-}
spliceLPZ :: AVL e -> e -> UINT -> AVL e -> e -> AVL e -> e -> AVL e -> AVL e
spliceLPZ s b L(1) ll             le lr e r = Z (N s b ll) le (Z lr e r)                        -- dH=0
spliceLPZ s b d   (N lll lle llr) le lr e r = let ll_ = spliceLN s b DECINT2(d) lll lle llr     -- dH=0
                                              in  ll_ `seq` P (Z ll_ le lr) e r
spliceLPZ s b d   (Z lll lle llr) le lr e r = let ll_ = spliceLZ s b DECINT1(d) lll lle llr     -- dH=0
                                              in case ll_ of
                                                 Z _ _ _ -> P (Z ll_ le lr) e r                 -- dH=0
                                                 P _ _ _ -> Z ll_ le (Z lr e r)                 -- dH=0
                                                 _       -> error "spliceLPZ: Bug0"             -- impossible
spliceLPZ s b d   (P lll lle llr) le lr e r = let ll_ = spliceLP s b DECINT1(d) lll lle llr     -- dH=0
                                              in  ll_ `seq` P (Z ll_ le lr) e r
spliceLPZ _ _ _    E              _  _  _ _ = error "spliceLPZ: Bug1"
-----------------------------------------------------------------------
-------------------------- spliceL Ends Here --------------------------
-----------------------------------------------------------------------

-- hl >= hr, splice s to right subtree of l, using b as the bridge
-- The Int argument is the absolute difference in tree height, hl-hr (>=0)
spliceR :: AVL e -> e -> UINT -> AVL e -> AVL e
spliceR s b L(0) l           = Z l b s
spliceR s b L(1) l           = P l b s
spliceR s b d   (N ll le lr) = spliceRN s b DECINT1(d) ll le lr   -- height diff of lr is one less
spliceR s b d   (Z ll le lr) = spliceRZ s b DECINT1(d) ll le lr   -- height diff of lr is one less
spliceR s b d   (P ll le lr) = spliceRP s b DECINT2(d) ll le lr   -- height diff of lr is two less
spliceR _ _ _    E           = error "spliceR: Bug0"              -- l can't be empty

-- Splice into right subtree of (P l e r), height cannot change as a result of this
spliceRP :: AVL e -> e -> UINT -> AVL e -> e -> AVL e -> AVL e
spliceRP s b L(0) l e  r           = Z l e (Z r b s)                                             -- dH=0
spliceRP s b L(1) l e  r           = Z l e (P r b s)                                             -- dH=0
spliceRP s b d    l e (N rl re rr) = let r_ = spliceRN s b DECINT1(d) rl re rr in r_ `seq` P l e r_
spliceRP s b d    l e (Z rl re rr) = let r_ = spliceRZ s b DECINT1(d) rl re rr
                                     in case r_ of
                                        Z _ _ _ -> P l e r_                                      -- dH=0
                                        N _ _ _ -> Z l e r_                                      -- dH=0
                                        _       -> error "spliceRP: Bug0"                        -- impossible
spliceRP s b d    l e (P rl re rr) = let r_ = spliceRP s b DECINT2(d) rl re rr in r_ `seq` P l e r_
spliceRP _ _ _    _ _  E           = error "spliceRP: Bug1"                                      -- impossible

-- Splice into right subtree of (Z l e r), Z->N if dH=1, Z->Z if dH=0
spliceRZ :: AVL e -> e -> UINT -> AVL e -> e -> AVL e -> AVL e
spliceRZ s b L(1) l e  r           = N l e (P r b s)                                                -- Z->N, dH=1
spliceRZ s b d    l e (N rl re rr) = let r_ = spliceRN s b DECINT1(d) rl re rr in r_ `seq` Z l e r_ -- Z->Z, dH=0
spliceRZ s b d    l e (Z rl re rr) = let r_ = spliceRZ s b DECINT1(d) rl re rr
                                     in case r_ of
                                        Z _ _ _ -> Z l e r_                                         -- Z->Z, dH=0
                                        N _ _ _ -> N l e r_                                         -- Z->N, dH=1
                                        _       -> error "spliceRZ: Bug0"                           -- impossible
spliceRZ s b d    l e (P rl re rr) = let r_ = spliceRP s b DECINT2(d) rl re rr in r_ `seq` Z l e r_ -- Z->Z, dH=0
spliceRZ _ _ _    _ _  E           = error "spliceRZ: Bug1"                                         -- impossible

-- Splice into right subtree of (N l e r), height cannot change as a result of this
spliceRN :: AVL e -> e -> UINT -> AVL e -> e -> AVL e -> AVL e
spliceRN s b L(1) l e (N rl re rr) = Z (P l e rl) re (Z rr b s)                                     -- dH=0
spliceRN s b L(1) l e (Z rl re rr) = Z (Z l e rl) re (Z rr b s)                                     -- dH=0
spliceRN s b L(1) l e (P rl re rr) = Z (Z l e rl) re (N rr b s)                                     -- dH=0
spliceRN s b d    l e (N rl re rr) = let r_ = spliceRN s b DECINT1(d) rl re rr in r_ `seq` N l e r_ -- dH=0
spliceRN s b d    l e (Z rl re rr) = spliceRNZ s b DECINT1(d) l e rl re rr                          -- dH=0
spliceRN s b d    l e (P rl re rr) = let r_ = spliceRP s b DECINT2(d) rl re rr in r_ `seq` N l e r_ -- dH=0
spliceRN _ _ _    _ _  E           = error "spliceRN: Bug0"

-- Splice into right subtree of (N l e (Z rl re rr))
{-# INLINE spliceRNZ #-}
spliceRNZ :: AVL e -> e -> UINT -> AVL e -> e -> AVL e -> e -> AVL e -> AVL e
spliceRNZ s b L(1) l e rl re rr              = Z (Z l e rl) re (P rr b s)                        -- dH=0
spliceRNZ s b d    l e rl re (N rrl rre rrr) = let rr_ = spliceRN s b DECINT1(d) rrl rre rrr
                                               in  rr_ `seq` N l e (Z rl re rr_)                 -- dH=0
spliceRNZ s b d    l e rl re (Z rrl rre rrr) = let rr_ = spliceRZ s b DECINT1(d) rrl rre rrr     -- dH=0
                                               in case rr_ of
                                                  Z _ _ _ -> N l e (Z rl re rr_)                 -- dH=0
                                                  N _ _ _ -> Z (Z l e rl) re rr_                 -- dH=0
                                                  _       -> error "spliceRNZ: Bug0"             -- impossible
spliceRNZ s b d    l e rl re (P rrl rre rrr) = let rr_ = spliceRP s b DECINT2(d) rrl rre rrr     -- dH=0
                                               in rr_ `seq` N l e (Z rl re rr_)
spliceRNZ _ _ _    _ _ _  _   E              = error "spliceRNZ: Bug1"
-----------------------------------------------------------------------
-------------------------- spliceR Ends Here --------------------------
-----------------------------------------------------------------------
