{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Join
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.Join
(-- * Joining AVL trees
 join,concatAVL,flatConcat,
) where

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.Size(addSize)
import Data.Tree.AVL.List(asTreeLenL,toListL)
import Data.Tree.AVL.Internals.DelUtils(popHLN,popHLZ,popHLP)
import Data.Tree.AVL.Height(height,addHeight)
import Data.Tree.AVL.Internals.HJoin(joinH',spliceH)

import Data.List(foldl')

#ifdef __GLASGOW_HASKELL__
import GHC.Base hiding (join)
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

import Prelude hiding (foldr)

-- | Join two AVL trees. This is the AVL equivalent of (++).
--
-- > asListL (l `join` r) = asListL l ++ asListL r
--
-- Complexity: O(log n), where n is the size of the larger of the two trees.
join :: AVL e -> AVL e -> AVL e
join l r = joinH' l (height l) r (height r)

-- Specialised list of AVL trees of known height, with leftmost element popped.
-- (used by concatAVL).
data HAVLS e = HE | H e (AVL e) UINT (HAVLS e)

-- | Concatenate a /finite/ list of AVL trees. During construction of the resulting tree the
-- input list is consumed lazily, but it will be consumed entirely before the result is returned.
--
-- > asListL (concatAVL avls) = concatMap asListL avls
--
-- Complexity: Umm..Dunno. Uses a divide and conquer approach to splice adjacent pairs of
-- trees in the list recursively, until only one tree remains. The complexity of each splice
-- is proportional to the difference in tree heights.
concatAVL :: [AVL e] -> AVL e
concatAVL []               = E
concatAVL (   E       :ts) = concatAVL ts
concatAVL (t@(N l _ _):ts) = concatHAVLS t (addHeight L(2) l) (mkHAVLS ts)
concatAVL (t@(Z l _ _):ts) = concatHAVLS t (addHeight L(1) l) (mkHAVLS ts)
concatAVL (t@(P _ _ r):ts) = concatHAVLS t (addHeight L(2) r) (mkHAVLS ts)

-- Recursively call mergePairs until only one tree remains.
-- The head of the current list has to be treated specially becuase it has no associated
-- bridging element.
concatHAVLS :: AVL e -> UINT -> HAVLS e -> AVL e
concatHAVLS l _   HE               = l
concatHAVLS l hl (H e r hr hs) = case mergePairs l hl e r hr hs of
                                 UBT3(t,ht,hs_) -> concatHAVLS t ht hs_


-- Merge adjacent pairs in the current list.
-- The head of the current list has to be treated specially becuase it has no associated
-- bridging element.
-- This function is strict in both elements of the result pair.
{-# INLINE mergePairs #-}
mergePairs :: AVL e -> UINT -> e -> AVL e -> UINT -> HAVLS e -> UBT3(AVL e,UINT,HAVLS e)
mergePairs l hl e r hr hs = case spliceH l hl e r hr of
                            UBT2(t,ht) -> case hs of
                               HE              -> UBT3(t,ht,HE)
                               H e_ t_ ht_ hs_ -> let hs__ = mergePairs_ e_ t_ ht_ hs_
                                                  in  hs__ `seq` UBT3(t,ht,hs__)

-- Deals with the rest of mergePairs after the head of the current list has been dealt with.
-- This function is strict in the resulting list head and lazy in the tail.
mergePairs_ :: e -> AVL e -> UINT -> HAVLS e -> HAVLS e
mergePairs_ e l hl  HE            = H e l hl HE
mergePairs_ e l hl (H e_ r hr hs) = case spliceH l hl e_ r hr of
                                    UBT2(t,ht) -> case hs of
                                       HE               -> H e t ht HE
                                       H e__ r_ hr_ hs_ -> H e t ht (mergePairs_ e__ r_ hr_ hs_)

-- Uses popHL to get the leftmost element from each tree and calculate the (popped) tree height.
-- The popped element is used as a bridging element for splicing purposes.
-- Empty and singleton trees get special treatment.
-- This function is strict in the resulting list head and lazy in the tail.
mkHAVLS :: [AVL e] -> HAVLS e
mkHAVLS []             = HE
mkHAVLS ( E       :ts) = mkHAVLS ts                -- Discard empty trees
mkHAVLS ((N l e r):ts) = case popHLN l e r of      -- Never a singlton with N
                         UBT3(e_,t,ht) -> H e_ t ht (mkHAVLS ts)
mkHAVLS ((Z l e r):ts) = case popHLZ l e r of
                         UBT3(e_,t,ht) -> if IS_TRUE(ht EQL L(0))
                                          then mkHAVLS_ e_ ts                -- Deal with singleton
                                          else H e_ t ht (mkHAVLS ts)        -- Otherwise treat as normal
mkHAVLS ((P l e r):ts) = case popHLP l e r of      -- Never a singlton with P
                         UBT3(e_,t,ht) -> H e_ t ht (mkHAVLS ts)
-- Deals with singletons (avoids unnecessary popHL in next in list)
mkHAVLS_ :: e -> [AVL e] -> HAVLS e
mkHAVLS_ e []               = H e E L(0) HE    -- End of list reached anyway
mkHAVLS_ e (   E       :ts) = mkHAVLS_ e ts    -- Discard empty trees
mkHAVLS_ e (t@(N l _ _):ts) = H e t (addHeight L(2) l) (mkHAVLS ts)
mkHAVLS_ e (t@(Z l _ _):ts) = H e t (addHeight L(1) l) (mkHAVLS ts)
mkHAVLS_ e (t@(P _ _ r):ts) = H e t (addHeight L(2) r) (mkHAVLS ts)
-----------------------------------------------------------------------
---------------------- concatAVL Ends Here ----------------------------
-----------------------------------------------------------------------

-- | Similar to 'concatAVL', except the resulting tree is flat.
-- This function evaluates the entire list of trees before constructing the result.
--
-- Complexity: O(n), where n is the total number of elements in the resulting tree.
flatConcat :: [AVL e] -> AVL e
flatConcat avls = asTreeLenL (foldl' addSize 0 avls) (foldr toListL [] avls)
