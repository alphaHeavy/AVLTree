{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.List
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.List
(-- * List related utilities for AVL trees

 -- ** Converting AVL trees to Lists (fixed element order).
 -- | These functions are lazy and allow normal lazy list processing
 -- style to be used (without necessarily converting the entire tree
 -- to a list in one gulp).
 asListL,toListL,asListR,toListR,

 -- ** Converting Lists to AVL trees (fixed element order)
 asTreeLenL,asTreeL,
 asTreeLenR,asTreeR,

 -- ** Converting unsorted Lists to sorted AVL trees
 asTree,

 -- ** \"Pushing\" unsorted Lists in sorted AVL trees
 pushList,

 -- * Some analogues of common List functions
 reverse,map,map',
 mapAccumL  ,mapAccumR  ,
 mapAccumL' ,mapAccumR' ,
 replicate,
 filter,mapMaybe,
 filterViaList,mapMaybeViaList,
 partition,
#if __GLASGOW_HASKELL__ > 604
 traverseAVL,
#endif

 -- ** Folds
 -- | Note that unlike folds over lists ('foldr' and 'foldl'), there is no
 -- significant difference between left and right folds in AVL trees, other
 -- than which side of the tree each starts with.
 -- Therefore this library provides strict and lazy versions of both.
 foldr,foldr',foldr1,foldr1',foldr2,foldr2',
 foldl,foldl',foldl1,foldl1',foldl2,foldl2',

#ifdef __GLASGOW_HASKELL__
         -- ** (GHC Only)
         mapAccumL'',mapAccumR'', foldrInt#,
#endif

 -- * Some clones of common List functions
 -- | These are a cure for the horrible @O(n^2)@ complexity the noddy Data.List definitions.
 nub,nubBy,

 -- * \"Flattening\" AVL trees
 -- | These functions can be improve search times by reducing a tree of given size to
 -- the minimum possible height.
 flatten,
 flatReverse,flatMap,flatMap',
) where

import Prelude hiding (reverse,map,replicate,filter,foldr,foldr1,foldl,foldl1) -- so haddock finds the symbols there

import Data.COrdering
import Data.Tree.AVL.Types(AVL(..),empty)
import Data.Tree.AVL.Size(size)
import Data.Tree.AVL.Push(push)
import Data.Tree.AVL.BinPath(findEmptyPath,insertPath)
import Data.Tree.AVL.Internals.HJoin(spliceH,joinH)

import Data.Bits(shiftR,(.&.))
import qualified Data.List as List (foldl',map)
#if __GLASGOW_HASKELL__ > 604
import Control.Applicative hiding (empty)
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base(Int#,(-#))
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | List AVL tree contents in left to right order.
-- The resulting list in ascending order if the tree is sorted.
--
-- Complexity: O(n)
asListL  :: AVL e -> [e]
asListL avl = toListL avl []

-- | Join the AVL tree contents to an existing list in left to right order.
-- This is a ++ free function which behaves as if defined thusly..
--
-- > avl `toListL` as = (asListL avl) ++ as
--
-- Complexity: O(n)
toListL :: AVL e -> [e] -> [e]
toListL  E        es = es
toListL (N l e r) es = toListL' l e r es
toListL (Z l e r) es = toListL' l e r es
toListL (P l e r) es = toListL' l e r es
toListL' :: AVL e -> e -> AVL e -> [e] -> [e]
toListL'   l e r  es = toListL l (e:(toListL r es))

-- | List AVL tree contents in right to left order.
-- The resulting list in descending order if the tree is sorted.
--
-- Complexity: O(n)
asListR  :: AVL e -> [e]
asListR avl = toListR avl []

-- | Join the AVL tree contents to an existing list in right to left order.
-- This is a ++ free function which behaves as if defined thusly..
--
-- > avl `toListR` as = (asListR avl) ++ as
--
-- Complexity: O(n)
toListR :: AVL e -> [e] -> [e]
toListR  E        es = es
toListR (N l e r) es = toListR' l e r es
toListR (Z l e r) es = toListR' l e r es
toListR (P l e r) es = toListR' l e r es
toListR' :: AVL e -> e -> AVL e -> [e] -> [e]
toListR'   l e r  es = toListR r (e:(toListR l es))

-- | The AVL equivalent of 'foldr' on lists. This is a the lazy version (as lazy as the folding function
-- anyway). Using this version with a function that is strict in it's second argument will result in O(n)
-- stack use. See 'foldr'' for a strict version.
--
-- It behaves as if defined..
--
-- > foldr f a avl = foldr f a (asListL avl)
--
-- For example, the 'asListL' function could be defined..
--
-- > asListL = foldr (:) []
--
-- Complexity: O(n)
foldr :: (e -> a -> a) -> a -> AVL e -> a
foldr f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = foldU (f e (foldU a r)) l

-- | The strict version of 'foldr', which is useful for functions which are strict in their second
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldr' :: (e -> a -> a) -> a -> AVL e -> a
foldr' f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = let a'  = foldU a r
                         a'' = f e a'
                     in a' `seq` a'' `seq` foldU a'' l

-- | The AVL equivalent of 'foldr1' on lists. This is a the lazy version (as lazy as the folding function
-- anyway). Using this version with a function that is strict in it's second argument will result in O(n)
-- stack use. See 'foldr1'' for a strict version.
--
-- > foldr1 f avl = foldr1 f (asListL avl)
--
-- This function raises an error if the tree is empty.
--
-- Complexity: O(n)
foldr1 :: (e -> e -> e) -> AVL e -> e
foldr1 f = foldU where
 foldU  E        = error "foldr1: Empty Tree"
 foldU (N l e r) = foldV l e r  -- r can't be E
 foldU (Z l e r) = foldW l e r  -- r might be E
 foldU (P l e r) = foldW l e r  -- r might be E
 -- Use this when r can't be E
 foldV l e r     = foldr f (f e (foldU r)) l
 -- Use this when r might be E
 foldW l e  E           = foldr f e l
 foldW l e (N rl re rr) = foldr f (f e (foldV rl re rr)) l -- rr can't be E
 foldW l e (Z rl re rr) = foldX l e rl re rr                  -- rr might be E
 foldW l e (P rl re rr) = foldX l e rl re rr                  -- rr might be E
 -- Common code for foldW (Z and P cases)
 foldX l e rl re rr = foldr f (f e (foldW rl re rr)) l

-- | The strict version of 'foldr1', which is useful for functions which are strict in their second
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldr1' :: (e -> e -> e) -> AVL e -> e
foldr1' f = foldU where
 foldU  E        = error "foldr1': Empty Tree"
 foldU (N l e r) = foldV l e r  -- r can't be E
 foldU (Z l e r) = foldW l e r  -- r might be E
 foldU (P l e r) = foldW l e r  -- r might be E
 -- Use this when r can't be E
 foldV l e r     = let a  = foldU r
                       a' = f e a
                   in a `seq` a' `seq` foldr' f a' l
 -- Use this when r might be E
 foldW l e  E           = foldr' f e l
 foldW l e (N rl re rr) = let a  = foldV rl re rr       -- rr can't be E
                              a' = f e a
                          in a `seq` a' `seq` foldr' f a' l
 foldW l e (Z rl re rr) = foldX l e rl re rr            -- rr might be E
 foldW l e (P rl re rr) = foldX l e rl re rr            -- rr might be E
 -- Common code for foldW (Z and P cases)
 foldX l e rl re rr = let a  = foldW rl re rr
                          a' = f e a
                      in a `seq` a' `seq` foldr' f a' l

-- | This fold is a hybrid between 'foldr' and 'foldr1'. As with 'foldr1', it requires
-- a non-empty tree, but instead of treating the rightmost element as an initial value, it applies
-- a function to it (second function argument) and uses the result instead. This allows
-- a more flexible type for the main folding function (same type as that used by 'foldr').
-- As with 'foldr' and 'foldr1', this function is lazy, so it's best not to use it with functions
-- that are strict in their second argument. See 'foldr2'' for a strict version.
--
-- Complexity: O(n)
foldr2 :: (e -> a -> a) -> (e -> a) -> AVL e -> a
foldr2 f g = foldU where
 foldU  E        = error "foldr2: Empty Tree"
 foldU (N l e r) = foldV l e r  -- r can't be E
 foldU (Z l e r) = foldW l e r  -- r might be E
 foldU (P l e r) = foldW l e r  -- r might be E
 -- Use this when r can't be E
 foldV l e r     = foldr f (f e (foldU r)) l
 -- Use this when r might be E
 foldW l e  E           = foldr f (g e) l
 foldW l e (N rl re rr) = foldr f (f e (foldV rl re rr)) l -- rr can't be E
 foldW l e (Z rl re rr) = foldX l e rl re rr                  -- rr might be E
 foldW l e (P rl re rr) = foldX l e rl re rr                  -- rr might be E
 -- Common code for foldW (Z and P cases)
 foldX l e rl re rr = foldr f (f e (foldW rl re rr)) l

-- | The strict version of 'foldr2', which is useful for functions which are strict in their second
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldr2' :: (e -> a -> a) -> (e -> a) -> AVL e -> a
foldr2' f g = foldU where
 foldU  E        = error "foldr2': Empty Tree"
 foldU (N l e r) = foldV l e r  -- r can't be E
 foldU (Z l e r) = foldW l e r  -- r might be E
 foldU (P l e r) = foldW l e r  -- r might be E
 -- Use this when r can't be E
 foldV l e r     = let a  = foldU r
                       a' = f e a
                   in a `seq` a' `seq` foldr' f a' l
 -- Use this when r might be E
 foldW l e  E           = let a = g e in a `seq` foldr' f a l
 foldW l e (N rl re rr) = let a  = foldV rl re rr              -- rr can't be E
                              a' = f e a
                          in a `seq` a' `seq` foldr' f a' l
 foldW l e (Z rl re rr) = foldX l e rl re rr                   -- rr might be E
 foldW l e (P rl re rr) = foldX l e rl re rr                   -- rr might be E
 -- Common code for foldW (Z and P cases)
 foldX l e rl re rr = let a  = foldW rl re rr
                          a' = f e a
                      in a `seq` a' `seq` foldr' f a' l


-- | The AVL equivalent of 'foldl' on lists. This is a the lazy version (as lazy as the folding function
-- anyway). Using this version with a function that is strict in it's first argument will result in O(n)
-- stack use. See 'foldl'' for a strict version.
--
-- > foldl f a avl = foldl f a (asListL avl)
--
-- For example, the 'asListR' function could be defined..
--
-- > asListR = foldl (flip (:)) []
--
-- Complexity: O(n)
foldl :: (a -> e -> a) -> a -> AVL e -> a
foldl f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = foldU (f (foldU a l) e) r

-- | The strict version of 'foldl', which is useful for functions which are strict in their first
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldl' :: (a -> e -> a) -> a -> AVL e -> a
foldl' f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = let a'  = foldU a l
                         a'' = f a' e
                     in a' `seq` a'' `seq` foldU a'' r

-- | The AVL equivalent of 'foldl1' on lists. This is a the lazy version (as lazy as the folding function
-- anyway). Using this version with a function that is strict in it's first argument will result in O(n)
-- stack use. See 'foldl1'' for a strict version.
--
-- > foldl1 f avl = foldl1 f (asListL avl)
--
-- This function raises an error if the tree is empty.
--
-- Complexity: O(n)
foldl1 :: (e -> e -> e) -> AVL e -> e
foldl1 f = foldU where
 foldU  E        = error "foldl1: Empty Tree"
 foldU (N l e r) = foldW l e r  -- l might be E
 foldU (Z l e r) = foldW l e r  -- l might be E
 foldU (P l e r) = foldV l e r  -- l can't be E
 -- Use this when l can't be E
 foldV l e r     = foldl f (f (foldU l) e) r
 -- Use this when l might be E
 foldW  E           e r = foldl f e r
 foldW (N ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (Z ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (P ll le lr) e r = foldl f (f (foldV ll le lr) e) r -- ll can't be E
 -- Common code for foldW (Z and P cases)
 foldX ll le lr e r = foldl f (f (foldW ll le lr) e) r

-- | The strict version of 'foldl1', which is useful for functions which are strict in their first
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldl1' :: (e -> e -> e) -> AVL e -> e
foldl1' f = foldU where
 foldU  E        = error "foldl1': Empty Tree"
 foldU (N l e r) = foldW l e r  -- l might be E
 foldU (Z l e r) = foldW l e r  -- l might be E
 foldU (P l e r) = foldV l e r  -- l can't be E
 -- Use this when l can't be E
 foldV l e r     = let a  = foldU l
                       a' = f a e
                   in a `seq` a' `seq` foldl' f a' r
 -- Use this when l might be E
 foldW  E           e r = foldl' f e r
 foldW (N ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (Z ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (P ll le lr) e r = let a  = foldV ll le lr             -- ll can't be E
                              a' = f a e
                          in a `seq` a' `seq` foldl' f a' r
 -- Common code for foldW (Z and P cases)
 foldX ll le lr e r = let a  = foldW ll le lr
                          a' = f a e
                      in a `seq` a' `seq` foldl' f a' r

-- | This fold is a hybrid between 'foldl' and 'foldl1'. As with 'foldl1', it requires
-- a non-empty tree, but instead of treating the leftmost element as an initial value, it applies
-- a function to it (second function argument) and uses the result instead. This allows
-- a more flexible type for the main folding function (same type as that used by 'foldl').
-- As with 'foldl' and 'foldl1', this function is lazy, so it's best not to use it with functions
-- that are strict in their first argument. See 'foldl2'' for a strict version.
--
-- Complexity: O(n)
foldl2 :: (a -> e -> a) -> (e -> a) -> AVL e -> a
foldl2 f g = foldU where
 foldU  E        = error "foldl2: Empty Tree"
 foldU (N l e r) = foldW l e r  -- l might be E
 foldU (Z l e r) = foldW l e r  -- l might be E
 foldU (P l e r) = foldV l e r  -- l can't be E
 -- Use this when l can't be E
 foldV l e r     = foldl f (f (foldU l) e) r
 -- Use this when l might be E
 foldW  E           e r = foldl f (g e) r
 foldW (N ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (Z ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (P ll le lr) e r = foldl f (f (foldV ll le lr) e) r -- ll can't be E
 -- Common code for foldW (Z and P cases)
 foldX ll le lr e r = foldl f (f (foldW ll le lr) e) r

-- | The strict version of 'foldl2', which is useful for functions which are strict in their first
-- argument. The advantage of this version is that it reduces the stack use from the O(n) that the lazy
-- version gives (when used with strict functions) to O(log n).
--
-- Complexity: O(n)
foldl2' :: (a -> e -> a) -> (e -> a) -> AVL e -> a
foldl2' f g = foldU where
 foldU  E        = error "foldl2': Empty Tree"
 foldU (N l e r) = foldW l e r  -- l might be E
 foldU (Z l e r) = foldW l e r  -- l might be E
 foldU (P l e r) = foldV l e r  -- l can't be E
 -- Use this when l can't be E
 foldV l e r     = let a  = foldU l
                       a' = f a e
                   in a `seq` a' `seq` foldl' f a' r
 -- Use this when l might be E
 foldW  E           e r = let a = g e in a `seq` foldl' f a r
 foldW (N ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (Z ll le lr) e r = foldX ll le lr e r                  -- ll might be E
 foldW (P ll le lr) e r = let a  = foldV ll le lr             -- ll can't be E
                              a' = f a e
                          in a `seq` a' `seq` foldl' f a' r
 -- Common code for foldW (Z and P cases)
 foldX ll le lr e r = let a  = foldW ll le lr
                          a' = f a e
                      in a `seq` a' `seq` foldl' f a' r

#ifdef __GLASGOW_HASKELL__
-- | This is a specialised version of 'foldr'' for use with an
-- /unboxed/ Int accumulator.
--
-- Complexity: O(n)
foldrInt# :: (e -> UINT -> UINT) -> UINT -> AVL e -> UINT
foldrInt# f = foldU where
 foldU a  E        = a
 foldU a (N l e r) = foldV a l e r
 foldU a (Z l e r) = foldV a l e r
 foldU a (P l e r) = foldV a l e r
 foldV a    l e r  = foldU (f e (foldU a r)) l
#endif

-- | The AVL equivalent of 'Data.List.mapAccumL' on lists.
-- It behaves like a combination of 'map' and 'foldl'.
-- It applies a function to each element of a tree, passing an accumulating parameter from
-- left to right, and returning a final value of this accumulator together with the new tree.
--
-- Using this version with a function that is strict in it's first argument will result in
-- O(n) stack use. See 'mapAccumL'' for a strict version.
--
-- Complexity: O(n)
mapAccumL :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumL f z ta = case mapAL z ta of
                      UBT2(zt,tb) -> (zt,tb)
 where mapAL z_  E          = UBT2(z_,E)
       mapAL z_ (N la a ra) = mapAL' z_ N la a ra
       mapAL z_ (Z la a ra) = mapAL' z_ Z la a ra
       mapAL z_ (P la a ra) = mapAL' z_ P la a ra
       {-# INLINE mapAL' #-}
       mapAL' z' c la a ra = case mapAL z' la of
                             UBT2(zl,lb) -> let (za,b) = f zl a
                                            in case mapAL za ra of
                                               UBT2(zr,rb) -> UBT2(zr, c lb b rb)

-- | This is a strict version of 'mapAccumL', which is useful for functions which
-- are strict in their first argument. The advantage of this version is that it reduces
-- the stack use from the O(n) that the lazy version gives (when used with strict functions)
-- to O(log n).
--
-- Complexity: O(n)
mapAccumL' :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumL' f z ta = case mapAL z ta of
                       UBT2(zt,tb) -> (zt,tb)
 where mapAL z_  E          = UBT2(z_,E)
       mapAL z_ (N la a ra) = mapAL' z_ N la a ra
       mapAL z_ (Z la a ra) = mapAL' z_ Z la a ra
       mapAL z_ (P la a ra) = mapAL' z_ P la a ra
       {-# INLINE mapAL' #-}
       mapAL' z' c la a ra = case mapAL z' la of
                             UBT2(zl,lb) -> case f zl a of
                                            (za,b) -> case mapAL za ra of
                                                      UBT2(zr,rb) -> UBT2(zr, c lb b rb)


-- | The AVL equivalent of 'Data.List.mapAccumR' on lists.
-- It behaves like a combination of 'map' and 'foldr'.
-- It applies a function to each element of a tree, passing an accumulating parameter from
-- right to left, and returning a final value of this accumulator together with the new tree.
--
-- Using this version with a function that is strict in it's first argument will result in
-- O(n) stack use. See 'mapAccumR'' for a strict version.
--
-- Complexity: O(n)
mapAccumR :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumR f z ta = case mapAR z ta of
                      UBT2(zt,tb) -> (zt,tb)
 where mapAR z_  E          = UBT2(z_,E)
       mapAR z_ (N la a ra) = mapAR' z_ N la a ra
       mapAR z_ (Z la a ra) = mapAR' z_ Z la a ra
       mapAR z_ (P la a ra) = mapAR' z_ P la a ra
       {-# INLINE mapAR' #-}
       mapAR' z' c la a ra = case mapAR z' ra of
                             UBT2(zr,rb) -> let (za,b) = f zr a
                                            in case mapAR za la of
                                               UBT2(zl,lb) -> UBT2(zl, c lb b rb)

-- | This is a strict version of 'mapAccumR', which is useful for functions which
-- are strict in their first argument. The advantage of this version is that it reduces
-- the stack use from the O(n) that the lazy version gives (when used with strict functions)
-- to O(log n).
--
-- Complexity: O(n)
mapAccumR' :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumR' f z ta = case mapAR z ta of
                       UBT2(zt,tb) -> (zt,tb)
 where mapAR z_  E          = UBT2(z_,E)
       mapAR z_ (N la a ra) = mapAR' z_ N la a ra
       mapAR z_ (Z la a ra) = mapAR' z_ Z la a ra
       mapAR z_ (P la a ra) = mapAR' z_ P la a ra
       {-# INLINE mapAR' #-}
       mapAR' z' c la a ra = case mapAR z' ra of
                             UBT2(zr,rb) -> case f zr a of
                                            (za,b) -> case mapAR za la of
                                                      UBT2(zl,lb) -> UBT2(zl, c lb b rb)

------------------------------------------------------------------------------------------------
-- These two functions attempt to make the strict mapAccums more efficient and reduce heap
-- burn rate with ghc by using an accumulating function that returns an unboxed pair.
------------------------------------------------------------------------------------------------
#ifdef __GLASGOW_HASKELL__
-- | Glasgow Haskell only. Similar to 'mapAccumL'' but uses an unboxed pair in the
-- accumulating function.
--
-- Complexity: O(n)
mapAccumL''
               :: (z -> a -> UBT2(z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumL'' f z ta = case mapAL z ta of
                        UBT2(zt,tb) -> (zt,tb)
 where mapAL z_  E          = UBT2(z_,E)
       mapAL z_ (N la a ra) = mapAL' z_ N la a ra
       mapAL z_ (Z la a ra) = mapAL' z_ Z la a ra
       mapAL z_ (P la a ra) = mapAL' z_ P la a ra
       {-# INLINE mapAL' #-}
       mapAL' z' c la a ra = case mapAL z' la of
                             UBT2(zl,lb) -> case f zl a of
                                            UBT2(za,b) -> case mapAL za ra of
                                                          UBT2(zr,rb) -> UBT2(zr, c lb b rb)

-- | Glasgow Haskell only. Similar to 'mapAccumR'' but uses an unboxed pair in the
-- accumulating function.
--
-- Complexity: O(n)
mapAccumR''
               :: (z -> a -> UBT2(z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumR'' f z ta = case mapAR z ta of
                        UBT2(zt,tb) -> (zt,tb)
 where mapAR z_  E          = UBT2(z_,E)
       mapAR z_ (N la a ra) = mapAR' z_ N la a ra
       mapAR z_ (Z la a ra) = mapAR' z_ Z la a ra
       mapAR z_ (P la a ra) = mapAR' z_ P la a ra
       {-# INLINE mapAR' #-}
       mapAR' z' c la a ra = case mapAR z' ra of
                             UBT2(zr,rb) -> case f zr a of
                                            UBT2(za,b) -> case mapAR za la of
                                                          UBT2(zl,lb) -> UBT2(zl, c lb b rb)

#endif
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- | Convert a list of known length into an AVL tree, such that the head of the list becomes
-- the leftmost tree element. The resulting tree is flat (and also sorted if the supplied list
-- is sorted in ascending order).
--
-- If the actual length of the list is not the same as the supplied length then
-- an error will be raised.
--
-- Complexity: O(n)
asTreeLenL :: Int -> [e] -> AVL e
asTreeLenL n es = case subst (replicate n ()) es of
                  UBT2(tree,es_) -> case es_ of
                                    [] -> tree
                                    _  -> error "asTreeLenL: List too long."
 where
 -- Substitute template values for real values taken from the list
 subst  E        as = UBT2(E,as)
 subst (N l _ r) as = subst' N l r as
 subst (Z l _ r) as = subst' Z l r as
 subst (P l _ r) as = subst' P l r as
 {-# INLINE subst' #-}
 subst' f l r as = case subst l as of
                   UBT2(l_,xs) -> case xs of
                                  a:as' -> case subst r as' of
                                           UBT2(r_,as__) -> let t_ = f l_ a r_
                                                            in t_ `seq` UBT2(t_,as__)
                                  []    -> error "asTreeLenL: List too short."


-- | As 'asTreeLenL', except the length of the list is calculated internally, not supplied
-- as an argument.
--
-- Complexity: O(n)
asTreeL :: [e] -> AVL e
asTreeL es = asTreeLenL (length es) es

-- | Convert a list of known length into an AVL tree, such that the head of the list becomes
-- the rightmost tree element. The resulting tree is flat (and also sorted if the supplied list
-- is sorted in descending order).
--
-- If the actual length of the list is not the same as the supplied length then
-- an error will be raised.
--
-- Complexity: O(n)
asTreeLenR :: Int -> [e] -> AVL e
asTreeLenR n es = case subst (replicate n ()) es of
                  UBT2(tree,es_) -> case es_ of
                                    [] -> tree
                                    _  -> error "asTreeLenR: List too long."
 where
 -- Substitute template values for real values taken from the list
 subst  E        as = UBT2(E,as)
 subst (N l _ r) as = subst' N l r as
 subst (Z l _ r) as = subst' Z l r as
 subst (P l _ r) as = subst' P l r as
 {-# INLINE subst' #-}
 subst' f l r as = case subst r as of
                   UBT2(r_,xs) -> case xs of
                                  a:as' -> case subst l as' of
                                           UBT2(l_,as__) -> let t_ = f l_ a r_
                                                            in t_ `seq` UBT2(t_,as__)
                                  []    -> error "asTreeLenR: List too short."

-- | As 'asTreeLenR', except the length of the list is calculated internally, not supplied
-- as an argument.
--
-- Complexity: O(n)
asTreeR :: [e] -> AVL e
asTreeR es = asTreeLenR (length es) es

-- | Reverse an AVL tree (swaps and reverses left and right sub-trees).
-- The resulting tree is the mirror image of the original.
--
-- Complexity: O(n)
reverse :: AVL e -> AVL e
reverse  E        = E
reverse (N l e r) = let l' = reverse l
                        r' = reverse r
                    in  l' `seq` r' `seq` P r' e l'
reverse (Z l e r) = let l' = reverse l
                        r' = reverse r
                    in  l' `seq` r' `seq` Z r' e l'
reverse (P l e r) = let l' = reverse l
                        r' = reverse r
                    in  l' `seq` r' `seq` N r' e l'

-- | Apply a function to every element in an AVL tree. This function preserves the tree shape.
-- There is also a strict version of this function ('map'').
--
-- N.B. If the tree is sorted the result of this operation will only be sorted if
-- the applied function preserves ordering (for some suitable ordering definition).
--
-- Complexity: O(n)
map :: (a -> b) -> AVL a -> AVL b
map f = mp where
 mp  E        = E
 mp (N l a r) = let l' = mp l
                    r' = mp r
                in  l' `seq` r' `seq` N l' (f a) r'
 mp (Z l a r) = let l' = mp l
                    r' = mp r
                in  l' `seq` r' `seq` Z l' (f a) r'
 mp (P l a r) = let l' = mp l
                    r' = mp r
                in  l' `seq` r' `seq` P l' (f a) r'

-- | Similar to 'map', but the supplied function is applied strictly.
--
-- Complexity: O(n)
map' :: (a -> b) -> AVL a -> AVL b
map' f = mp' where
 mp'  E        = E
 mp' (N l a r) = let l' = mp' l
                     r' = mp' r
                     b  = f a
                 in  b `seq` l' `seq` r' `seq` N l' b r'
 mp' (Z l a r) = let l' = mp' l
                     r' = mp' r
                     b  = f a
                 in  b `seq` l' `seq` r' `seq` Z l' b r'
 mp' (P l a r) = let l' = mp' l
                     r' = mp' r
                     b  = f a
                 in  b `seq` l' `seq` r' `seq` P l' b r'


-- | Construct a flat AVL tree of size n (n>=0), where all elements are identical.
--
-- Complexity: O(log n)
replicate :: Int -> e -> AVL e
replicate m e = rep m where -- Functional spaghetti follows :-)
 rep n | odd n = repOdd n -- n is odd , >=1
 rep n         = repEvn n -- n is even, >=0
 -- n is known to be odd (>=1), so left and right sub-trees are identical
 repOdd n      = let sub = rep (n `shiftR` 1) in sub `seq` Z sub e sub
 -- n is known to be even (>=0)
 repEvn n | n .&. (n-1) == 0 = repP2 n -- treat exact powers of 2 specially, traps n=0 too
 repEvn n      = let nl = n `shiftR` 1 -- size of left subtree  (odd or even)
                     nr = nl - 1       -- size of right subtree (even or odd)
                 in if odd nr
                    then let l = repEvn nl           -- right sub-tree is odd , so left is even (>=2)
                             r = repOdd nr
                         in l `seq` r `seq` Z l e r
                    else let l = repOdd nl           -- right sub-tree is even, so left is odd (>=2)
                             r = repEvn nr
                         in l `seq` r `seq` Z l e r
 -- n is an exact power of 2 (or 0), I.E. 0,1,2,4,8,16..
 repP2 0       = E
 repP2 1       = Z E e E
 repP2 n       = let nl = n `shiftR` 1 -- nl is also an exact power of 2
                     nr = nl - 1       -- nr is one less that an exact power of 2
                     l  = repP2 nl
                     r  = repP2M1 nr
                 in  l `seq` r `seq` P l e r -- BF=+1
 -- n is one less than an exact power of 2, I.E. 0,1,3,7,15..
 repP2M1 0     = E
 repP2M1 n     = let sub = repP2M1 (n `shiftR` 1) in sub `seq` Z sub e sub

-- | Flatten an AVL tree, preserving the ordering of the tree elements.
--
-- Complexity: O(n)
flatten :: AVL e -> AVL e
flatten t = asTreeLenL (size t) (asListL t)

-- | Similar to 'flatten', but the tree elements are reversed. This function has higher constant
-- factor overhead than 'reverse'.
--
-- Complexity: O(n)
flatReverse :: AVL e -> AVL e
flatReverse t = asTreeLenL (size t) (asListR t)

-- | Similar to 'map', but the resulting tree is flat.
-- This function has higher constant factor overhead than 'map'.
--
-- Complexity: O(n)
flatMap :: (a -> b) -> AVL a -> AVL b
flatMap f t = asTreeLenL (size t) (List.map f (asListL t))

-- | Same as 'flatMap', but the supplied function is applied strictly.
--
-- Complexity: O(n)
flatMap' :: (a -> b) -> AVL a -> AVL b
flatMap' f t = asTreeLenL (size t) (mp' f (asListL t)) where
 mp' _ []     = []
 mp' g (a:as) = let b = g a in b `seq` (b : mp' f as)

-- | Remove all AVL tree elements which do not satisfy the supplied predicate.
-- Element ordering is preserved. The resulting tree is flat.
-- See 'filter' for an alternative implementation which is probably more efficient.
--
-- Complexity: O(n)
filterViaList :: (e -> Bool) -> AVL e -> AVL e
filterViaList p t = filter' [] 0 (asListR t) where
 filter' se n []     = asTreeLenL n se
 filter' se n (e:es) = if p e then  let n'=n+1  in  n' `seq` filter' (e:se) n' es
                              else  filter' se n es

-- | Remove all AVL tree elements which do not satisfy the supplied predicate.
-- Element ordering is preserved.
--
-- Complexity: O(n)
filter :: (e -> Bool) -> AVL e -> AVL e
filter p t0 = case filter_ L(0) t0 of UBT3(_,t_,_) -> t_  -- Work with relative heights!!
 where filter_ h t = case t of
                     E       -> UBT3(False,E,h)
                     N l e r -> f l DECINT2(h) e r DECINT1(h)
                     Z l e r -> f l DECINT1(h) e r DECINT1(h)
                     P l e r -> f l DECINT1(h) e r DECINT2(h)
        where f l hl e r hr =                     case filter_ hl l of
                              UBT3(bl,l_,hl_)  -> case filter_ hr r of
                               UBT3(br,r_,hr_) -> if p e
                                                  then if bl || br
                                                       then case spliceH l_ hl_ e r_ hr_ of
                                                            UBT2(t_,h_) -> UBT3(True,t_,h_)
                                                       else UBT3(False,t,h)
                                                  else case joinH l_ hl_ r_ hr_ of
                                                       UBT2(t_,h_) -> UBT3(True,t_,h_)

-- | Partition an AVL tree using the supplied predicate. The first AVL tree in the
-- resulting pair contains all elements for which the predicate is True, the second
-- contains all those for which the predicate is False. Element ordering is preserved.
-- Both of the resulting trees are flat.
--
-- Complexity: O(n)
partition :: (e -> Bool) -> AVL e -> (AVL e, AVL e)
partition p t = part 0 [] 0 [] (asListR t) where
 part nT lstT nF lstF []     = let avlT = asTreeLenL nT lstT
                                   avlF = asTreeLenL nF lstF
                               in (avlT,avlF) -- Non strict in avlT, avlF !!
 part nT lstT nF lstF (e:es) = if p e then let nT'=nT+1 in nT' `seq` part nT' (e:lstT) nF     lstF  es
                                      else let nF'=nF+1 in nF' `seq` part nT     lstT  nF' (e:lstF) es

-- | Remove all AVL tree elements for which the supplied function returns 'Nothing'.
-- Element ordering is preserved. The resulting tree is flat.
-- See 'mapMaybe' for an alternative implementation which is probably more efficient.
--
-- Complexity: O(n)
mapMaybeViaList :: (a -> Maybe b) -> AVL a -> AVL b
mapMaybeViaList f t = mp' [] 0 (asListR t) where
 mp' sb n []     = asTreeLenL n sb
 mp' sb n (a:as) = case f a of
                   Just b  -> let n'=n+1  in  n' `seq` mp' (b:sb) n' as
                   Nothing -> mp' sb n as

-- | Remove all AVL tree elements for which the supplied function returns 'Nothing'.
-- Element ordering is preserved.
--
-- Complexity: O(n)
mapMaybe :: (a -> Maybe b) -> AVL a -> AVL b
mapMaybe f t0 = case mapMaybe_ L(0) t0 of UBT2(t_,_) -> t_  -- Work with relative heights!!
 where mapMaybe_ h t = case t of
                       E       -> UBT2(E,h)
                       N l a r -> m l DECINT2(h) a r DECINT1(h)
                       Z l a r -> m l DECINT1(h) a r DECINT1(h)
                       P l a r -> m l DECINT1(h) a r DECINT2(h)
        where m l hl a r hr =                  case mapMaybe_ hl l of
                              UBT2(l_,hl_)  -> case mapMaybe_ hr r of
                               UBT2(r_,hr_) -> case f a of
                                               Just b  -> spliceH l_ hl_ b r_ hr_
                                               Nothing ->   joinH l_ hl_   r_ hr_

-- | Invokes 'pushList' on the empty AVL tree.
--
-- Complexity: O(n.(log n))
asTree :: (e -> e -> COrdering e) -> [e] -> AVL e
asTree c = pushList c empty
{-# INLINE asTree #-}

-- | Push the elements of an unsorted List in a sorted AVL tree using the supplied combining comparison.
--
-- Complexity: O(n.(log (m+n))) where n is the list length, m is the tree size.
pushList :: (e -> e -> COrdering e) -> AVL e -> [e] -> AVL e
pushList c avl = List.foldl' addElem avl
 where addElem t e = push (c e) e t

-- | A fast alternative implementation for 'Data.List.nub'.
-- Deletes all but the first occurrence of an element from the input list.
--
-- Complexity: O(n.(log n))
nub :: Ord a => [a] -> [a]
nub = nubBy compare
{-# INLINE nub #-}

-- | A fast alternative implementation for 'Data.List.nubBy'.
-- Deletes all but the first occurrence of an element from the input list.
--
-- Complexity: O(n.(log n))
nubBy :: (a -> a -> Ordering) -> [a] -> [a]
nubBy c = nubbit E where
 nubbit _   []     = []
 nubbit avl (a:as) = case findEmptyPath (c a) avl of
                     L(-1) -> nubbit avl as                  -- Already encountered
                     p     -> let avl' = insertPath p a avl  -- First encounter
                              in avl' `seq` (a : nubbit avl' as)

#if __GLASGOW_HASKELL__ > 604
-- | This is the non-overloaded version of the 'Data.Traversable.traverse' method for AVL trees.
traverseAVL :: Applicative f => (a -> f b) -> AVL a -> f (AVL b)
traverseAVL _f E = pure E
traverseAVL f (N l v r) = N <$> traverseAVL f l <*> f v <*> traverseAVL f r
traverseAVL f (Z l v r) = Z <$> traverseAVL f l <*> f v <*> traverseAVL f r
traverseAVL f (P l v r) = P <$> traverseAVL f l <*> f v <*> traverseAVL f r
#endif

