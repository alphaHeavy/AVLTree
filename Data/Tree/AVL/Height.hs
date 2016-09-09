{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Height
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- AVL tree height related utilities.
--
-- The functions defined here are not exported by the main Data.Tree.AVL module
-- because they violate the policy for AVL tree equality used elsewhere in this library.
-- You need to import this module explicitly if you want to use any of these functions.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Height
        (-- * AVL tree height utilities.
         height,addHeight,compareHeight, -- heightInt,
        ) where

import Data.Tree.AVL.Types(AVL(..))

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- {-# INLINE heightInt #-} -- Don't want this
-- heightInt :: AVL e -> Int
-- heightInt t = ASINT(addHeight L(0) t)

-- | Determine the height of an AVL tree.
--
-- Complexity: O(log n)
{-# INLINE height #-}
height :: AVL e -> UINT
height t = addHeight L(0) t

-- | Adds the height of a tree to the first argument.
--
-- Complexity: O(log n)
addHeight :: UINT -> AVL e -> UINT
addHeight h  E        = h
addHeight h (N l _ _) = addHeight INCINT2(h) l
addHeight h (Z l _ _) = addHeight INCINT1(h) l
addHeight h (P _ _ r) = addHeight INCINT2(h) r

-- | A fast algorithm for comparing the heights of two trees. This algorithm avoids the need
-- to compute the heights of both trees and should offer better performance if the trees differ
-- significantly in height. But if you need the heights anyway it will be quicker to just evaluate
-- them both and compare the results.
--
-- Complexity: O(log n), where n is the size of the smaller of the two trees.
compareHeight :: AVL a -> AVL b -> Ordering
compareHeight = ch L(0) where                       -- d = hA-hB
 ch :: UINT -> AVL a -> AVL b -> Ordering
 ch d  E           E          = COMPAREUINT d L(0)
 ch d  E          (N l1 _ _ ) = chA DECINT2(d) l1
 ch d  E          (Z l1 _ _ ) = chA DECINT1(d) l1
 ch d  E          (P _  _ r1) = chA DECINT2(d) r1
 ch d (N l0 _ _ )  E          = chB INCINT2(d) l0
 ch d (N l0 _ _ ) (N l1 _ _ ) = ch          d  l0 l1
 ch d (N l0 _ _ ) (Z l1 _ _ ) = ch  INCINT1(d) l0 l1
 ch d (N l0 _ _ ) (P _  _ r1) = ch          d  l0 r1
 ch d (Z l0 _ _ )  E          = chB INCINT1(d) l0
 ch d (Z l0 _ _ ) (N l1 _ _ ) = ch  DECINT1(d) l0 l1
 ch d (Z l0 _ _ ) (Z l1 _ _ ) = ch          d  l0 l1
 ch d (Z l0 _ _ ) (P _  _ r1) = ch  DECINT1(d) l0 r1
 ch d (P _  _ r0)  E          = chB INCINT2(d) r0
 ch d (P _  _ r0) (N l1 _ _ ) = ch          d  r0 l1
 ch d (P _  _ r0) (Z l1 _ _ ) = ch  INCINT1(d) r0 l1
 ch d (P _  _ r0) (P _  _ r1) = ch          d  r0 r1
 -- Tree A ended first, continue with Tree B until hA-hB<0, or Tree B ends
 chA d tB = case COMPAREUINT d L(0) of
            LT ->             LT
            EQ -> case tB of
                  E        -> EQ
                  _        -> LT
            GT -> case tB of
                  E        -> GT
                  N l _ _  -> chA DECINT2(d) l
                  Z l _ _  -> chA DECINT1(d) l
                  P _ _ r  -> chA DECINT2(d) r
 -- Tree B ended first, continue with Tree A until hA-hB>0, or Tree A ends
 chB d tA = case COMPAREUINT d L(0) of
            GT ->             GT
            EQ -> case tA of
                  E        -> EQ
                  _        -> GT
            LT -> case tA of
                  E        -> LT
                  N l _ _  -> chB INCINT2(d) l
                  Z l _ _  -> chB INCINT1(d) l
                  P _ _ r  -> chB INCINT2(d) r

