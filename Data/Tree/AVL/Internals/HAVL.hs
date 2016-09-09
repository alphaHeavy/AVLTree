{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Internals.HAVL
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- HAVL data type and related utilities
-----------------------------------------------------------------------------
module Data.Tree.AVL.Internals.HAVL
        (
         HAVL(HAVL),emptyHAVL,toHAVL,isEmptyHAVL,isNonEmptyHAVL,
         spliceHAVL,joinHAVL,
         pushLHAVL,pushRHAVL
        ) where

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.Height(addHeight)
import Data.Tree.AVL.Internals.HJoin(spliceH,joinH)
import Data.Tree.AVL.Internals.HPush(pushHL,pushHR)

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | An HAVL represents an AVL tree of known height.
data HAVL e = HAVL (AVL e) {-# UNPACK #-} !UINT

-- | Empty HAVL (height is 0).
emptyHAVL :: HAVL e
emptyHAVL = HAVL E L(0)

-- | Returns 'True' if the AVL component of an HAVL tree is empty. Note that height component
-- is ignored, so it's OK to use this function in cases where the height is relative.
--
-- Complexity: O(1)
{-# INLINE isEmptyHAVL #-}
isEmptyHAVL :: HAVL e -> Bool
isEmptyHAVL (HAVL E _) = True
isEmptyHAVL (HAVL _ _) = False

-- | Returns 'True' if the AVL component of an HAVL tree is non-empty. Note that height component
-- is ignored, so it's OK to use this function in cases where the height is relative.
--
-- Complexity: O(1)
{-# INLINE isNonEmptyHAVL #-}
isNonEmptyHAVL :: HAVL e -> Bool
isNonEmptyHAVL (HAVL E _) = False
isNonEmptyHAVL (HAVL _ _) = True

-- | Converts an AVL to HAVL
toHAVL :: AVL e -> HAVL e
toHAVL t = HAVL t (addHeight L(0) t)

-- | Splice two HAVL trees using the supplied bridging element.
-- That is, the bridging element appears "in the middle" of the resulting HAVL tree.
-- The elements of the first tree argument are to the left of the bridging element and
-- the elements of the second tree are to the right of the bridging element.
--
-- This function does not require that the AVL heights are absolutely correct, only that
-- the difference in supplied heights is equal to the difference in actual heights. So it's
-- OK if the input heights both have the same unknown constant offset. (The output height
-- will also have the same constant offset in this case.)
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
{-# INLINE spliceHAVL #-}
spliceHAVL :: HAVL e -> e -> HAVL e -> HAVL e
spliceHAVL (HAVL l hl) e (HAVL r hr) = case spliceH l hl e r hr of UBT2(t,ht) -> HAVL t ht

-- | Join two HAVL trees.
-- It's OK if heights are relative (I.E. if they share same fixed offset).
--
-- Complexity: O(d), where d is the absolute difference in tree heights.
{-# INLINE joinHAVL #-}
joinHAVL :: HAVL e -> HAVL e -> HAVL e
joinHAVL (HAVL l hl) (HAVL r hr) = case joinH l hl r hr of UBT2(t,ht) -> HAVL t ht

-- | A version of 'pushL' for HAVL trees.
-- It's OK if height is relative, with fixed offset. In this case the height of the result
-- will have the same fixed offset.
{-# INLINE pushLHAVL #-}
pushLHAVL :: e -> HAVL e -> HAVL e
pushLHAVL e (HAVL t ht) = case pushHL e t ht of UBT2(t_,ht_) -> HAVL t_ ht_

-- | A version of 'pushR' for HAVL trees.
-- It's OK if height is relative, with fixed offset. In this case the height of the result
-- will have the same fixed offset.
{-# INLINE pushRHAVL #-}
pushRHAVL :: HAVL e -> e -> HAVL e
pushRHAVL (HAVL t ht) e = case pushHR t ht e of UBT2(t_,ht_) -> HAVL t_ ht_

