{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Set
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.Set
(-- * Set operations
 -- | Functions for manipulating AVL trees which represent ordered sets (I.E. /sorted/ trees).
 -- Note that although many of these functions work with a variety of different element
 -- types they all require that elements are sorted according to the same criterion (such
 -- as a field value in a record).

 -- ** Union
 union,unionMaybe,disjointUnion,unions,

 -- ** Difference
 difference,differenceMaybe,symDifference,

 -- ** Intersection
 intersection,intersectionMaybe,

 -- *** Intersection with the result as a list
 -- | Sometimes you don\'t want intersection to give a tree, particularly if the
 -- resulting elements are not orderered or sorted according to whatever criterion was
 -- used to sort the elements of the input sets.
 --
 -- The reason these variants are provided for intersection only (and not the other
 -- set functions) is that the (tree returning) intersections always construct an entirely
 -- new tree, whereas with the others the resulting tree will typically share sub-trees
 -- with one or both of the originals. (Of course the results of the others can easily be
 -- converted to a list too if required.)
 intersectionToList,intersectionAsList,
 intersectionMaybeToList,intersectionMaybeAsList,

 -- ** \'Venn diagram\' operations
 -- | Given two sets A and B represented as sorted AVL trees, the venn operations evaluate
 -- components @A-B@, @A.B@ and @B-A@. The intersection part may be obtained as a List
 -- rather than AVL tree if required.
 --
 -- Note that in all cases the three resulting sets are /disjoint/ and can safely be re-combined
 -- after most \"munging\" operations using 'disjointUnion'.
 venn,vennMaybe,

 -- *** \'Venn diagram\' operations with the intersection component as a List.
 -- | These variants are provided for the same reasons as the Intersection as List variants.
 vennToList,vennAsList,
 vennMaybeToList,vennMaybeAsList,

 -- ** Subset
 isSubsetOf,isSubsetOfBy,
) where

import Prelude -- so haddock finds the symbols there

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.Height(addHeight)
import Data.Tree.AVL.List(asTreeLenL)
import Data.Tree.AVL.Internals.HJoin(spliceH)
import Data.Tree.AVL.Internals.HSet(unionH,unionMaybeH,disjointUnionH,
                                    intersectionH,intersectionMaybeH,
                                    vennH,vennMaybeH,
                                    differenceH,differenceMaybeH,symDifferenceH)

import Data.COrdering

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | Uses the supplied combining comparison to evaluate the union of two sets represented as
-- sorted AVL trees. Whenever the combining comparison is applied, the first comparison argument is
-- an element of the first tree and the second comparison argument is an element of the second tree.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
union :: (e -> e -> COrdering e) -> AVL e -> AVL e -> AVL e
union c = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = t1
 gu t0                 E          = t0
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(1) l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(1) l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(1) l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) r1)
 gu_ t0 h0 t1 h1 = case unionH c t0 h0 t1 h1 of UBT2(t,_) -> t

-- | Similar to 'union', but the resulting tree does not include elements in cases where
-- the supplied combining comparison returns @(Eq Nothing)@.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
unionMaybe :: (e -> e -> COrdering (Maybe e)) -> AVL e -> AVL e -> AVL e
unionMaybe c = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = t1
 gu t0                 E          = t0
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(1) l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(1) l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(1) l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) r1)
 gu_ t0 h0 t1 h1 = case unionMaybeH c t0 h0 t1 h1 of UBT2(t,_) -> t

-- | Uses the supplied comparison to evaluate the union of two /disjoint/ sets represented as
-- sorted AVL trees. It will be slightly faster than 'union' but will raise an error if the
-- two sets intersect. Typically this would be used to re-combine the \"post-munge\" results
-- from one of the \"venn\" operations.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
-- (Faster than Hedge union from Data.Set at any rate).
disjointUnion :: (e -> e -> Ordering) -> AVL e -> AVL e -> AVL e
disjointUnion c = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = t1
 gu t0                 E          = t0
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(1) l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(1) l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(1) l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) r1)
 gu_ t0 h0 t1 h1 = case disjointUnionH c t0 h0 t1 h1 of UBT2(t,_) -> t

-- | Uses the supplied combining comparison to evaluate the union of all sets in a list
-- of sets represented as sorted AVL trees. Behaves as if defined..
--
-- @unions ccmp avls = foldl' ('union' ccmp) empty avls@
unions :: (e -> e -> COrdering e) -> [AVL e] -> AVL e
unions c = gus E L(0) where
 gus a _  []                 = a
 gus a ha (   E       :avls) = gus a ha avls
 gus a ha (t@(N l _ _):avls) = case unionH c a ha t (addHeight L(2) l) of UBT2(a_,ha_) -> gus a_ ha_ avls
 gus a ha (t@(Z l _ _):avls) = case unionH c a ha t (addHeight L(1) l) of UBT2(a_,ha_) -> gus a_ ha_ avls
 gus a ha (t@(P _ _ r):avls) = case unionH c a ha t (addHeight L(2) r) of UBT2(a_,ha_) -> gus a_ ha_ avls

-- | Uses the supplied combining comparison to evaluate the intersection of two sets represented as
-- sorted AVL trees.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
intersection :: (a -> b -> COrdering c) -> AVL a -> AVL b -> AVL c
intersection c t0 t1 = case intersectionH c t0 t1 of UBT2(t,_) -> t

-- | Similar to 'intersection', but the resulting tree does not include elements in cases where
-- the supplied combining comparison returns @(Eq Nothing)@.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
intersectionMaybe :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> AVL c
intersectionMaybe c t0 t1 = case intersectionMaybeH c t0 t1 of UBT2(t,_) -> t

-- | Similar to 'intersection', but prepends the result to the supplied list in
-- ascending order. This is a (++) free function which behaves as if defined:
--
-- @intersectionToList c setA setB cs = asListL (intersection c setA setB) ++ cs@
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
intersectionToList :: (a -> b -> COrdering c) -> AVL a -> AVL b -> [c] -> [c]
intersectionToList comp = i where
 -- i :: AVL a -> AVL b -> [c] -> [c]
 i  E            _           cs = cs
 i  _            E           cs = cs
 i (N l0 e0 r0) (N l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (N l0 e0 r0) (Z l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (N l0 e0 r0) (P l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (Z l0 e0 r0) (N l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (Z l0 e0 r0) (Z l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (Z l0 e0 r0) (P l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (P l0 e0 r0) (N l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (P l0 e0 r0) (Z l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (P l0 e0 r0) (P l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i' l0 e0 r0 l1 e1 r1 cs =
  case comp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt   ->                            case forkR r0 e1 of
          UBT5(rl0,_,mbc1,rr0,_)  -> case forkL e0 l1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
           UBT5(ll1,_,mbc0,lr1,_) ->                     -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
            -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
            let cs'  = i rr0 r1 cs
                cs'' = cs'  `seq` case mbc1 of
                                  Nothing -> i rl0 lr1 cs'
                                  Just c1 -> i rl0 lr1 (c1:cs')
            in         cs'' `seq` case mbc0 of
                                  Nothing -> i l0 ll1 cs''
                                  Just c0 -> i l0 ll1 (c0:cs'')
  -- e0 = e1
  Eq c -> let cs' = i r0 r1 cs in cs' `seq` i l0 l1 (c:cs')
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt   ->                            case forkL e0 r1 of
          UBT5(rl1,_,mbc0,rr1,_)  -> case forkR l0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
           UBT5(ll0,_,mbc1,lr0,_) ->                     -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0 + rl1) < e0 < (r0 + rr1)
            let cs'  = i r0 rr1 cs
                cs'' = cs'  `seq` case mbc0 of
                                  Nothing -> i lr0 rl1 cs'
                                  Just c0 -> i lr0 rl1 (c0:cs')
            in         cs'' `seq` case mbc1 of
                                  Nothing -> i ll0 l1 cs''
                                  Just c1 -> i ll0 l1 (c1:cs'')
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1)
 -- forkL :: a -> AVL b -> UBT5(AVL b,UINT,Maybe c,AVL b,UINT)
 forkL e0 t1 = forkL_ t1 L(0) where
  forkL_  E        h = UBT5(E,h,Nothing,E,h) -- Relative heights!!
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case comp e0 e of
                        Lt    ->                             case forkL_ l hl of
                                 UBT5(l0,hl0,mbc0,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                  UBT2(l1_,hl1_)          -> UBT5(l0,hl0,mbc0,l1_,hl1_)
                        Eq c0 -> UBT5(l,hl,Just c0,r,hr)
                        Gt    ->                             case forkL_ r hr of
                                 UBT5(l0,hl0,mbc0,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                  UBT2(l0_,hl0_)          -> UBT5(l0_,hl0_,mbc0,l1,hl1)
 -- forkR :: AVL a -> b -> UBT5(AVL a,UINT,Maybe c,AVL a,UINT)
 forkR t0 e1 = forkR_ t0 L(0) where
  forkR_  E        h = UBT5(E,h,Nothing,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case comp e e1 of
                        Lt    ->                             case forkR_ r hr of
                                 UBT5(l0,hl0,mbc1,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                  UBT2(l0_,hl0_)          -> UBT5(l0_,hl0_,mbc1,l1,hl1)
                        Eq c1 -> UBT5(l,hl,Just c1,r,hr)
                        Gt    ->                             case forkR_ l hl of
                                 UBT5(l0,hl0,mbc1,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                  UBT2(l1_,hl1_)          -> UBT5(l0,hl0,mbc1,l1_,hl1_)
-----------------------------------------------------------------------
------------------ intersectionToList Ends Here -------------------
-----------------------------------------------------------------------

-- | Applies 'intersectionToList' to the empty list.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
intersectionAsList :: (a -> b -> COrdering c) -> AVL a -> AVL b -> [c]
intersectionAsList c setA setB = intersectionToList c setA setB []

-- | Similar to 'intersectionToList', but the result does not include elements in cases where
-- the supplied combining comparison returns @(Eq Nothing)@.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
intersectionMaybeToList :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> [c] -> [c]
intersectionMaybeToList comp = i where
 -- i :: AVL a -> AVL b -> [c] -> [c]
 i  E            _           cs = cs
 i  _            E           cs = cs
 i (N l0 e0 r0) (N l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (N l0 e0 r0) (Z l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (N l0 e0 r0) (P l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (Z l0 e0 r0) (N l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (Z l0 e0 r0) (Z l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (Z l0 e0 r0) (P l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (P l0 e0 r0) (N l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (P l0 e0 r0) (Z l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i (P l0 e0 r0) (P l1 e1 r1) cs = i' l0 e0 r0 l1 e1 r1 cs
 i' l0 e0 r0 l1 e1 r1 cs =
  case comp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt   ->                            case forkR r0 e1 of
          UBT5(rl0,_,mbc1,rr0,_)  -> case forkL e0 l1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
           UBT5(ll1,_,mbc0,lr1,_) ->                     -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
            -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
            let cs'  = i rr0 r1 cs
                cs'' = cs'  `seq` case mbc1 of
                                  Nothing -> i rl0 lr1 cs'
                                  Just c1 -> i rl0 lr1 (c1:cs')
            in         cs'' `seq` case mbc0 of
                                  Nothing -> i l0 ll1 cs''
                                  Just c0 -> i l0 ll1 (c0:cs'')
  -- e0 = e1
  Eq mbc  -> let cs' = i r0 r1 cs in cs' `seq` case mbc of
                                               Nothing -> i l0 l1 cs'
                                               Just c  -> i l0 l1 (c:cs')
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt   ->                            case forkL e0 r1 of
          UBT5(rl1,_,mbc0,rr1,_)  -> case forkR l0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
           UBT5(ll0,_,mbc1,lr0,_) ->                     -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0 + rl1) < e0 < (r0 + rr1)
            let cs'  = i r0 rr1 cs
                cs'' = cs'  `seq` case mbc0 of
                                  Nothing -> i lr0 rl1 cs'
                                  Just c0 -> i lr0 rl1 (c0:cs')
            in         cs'' `seq` case mbc1 of
                                  Nothing -> i ll0 l1 cs''
                                  Just c1 -> i ll0 l1 (c1:cs'')
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1)
 -- forkL :: a -> AVL b -> UBT5(AVL b,UINT,Maybe c,AVL b,UINT)
 forkL e0 t1 = forkL_ t1 L(0) where
  forkL_  E        h = UBT5(E,h,Nothing,E,h) -- Relative heights!!
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case comp e0 e of
                        Lt      ->                             case forkL_ l hl of
                                   UBT5(l0,hl0,mbc0,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                    UBT2(l1_,hl1_)          -> UBT5(l0,hl0,mbc0,l1_,hl1_)
                        Eq mbc0 -> UBT5(l,hl,mbc0,r,hr)
                        Gt      ->                             case forkL_ r hr of
                                   UBT5(l0,hl0,mbc0,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                    UBT2(l0_,hl0_)          -> UBT5(l0_,hl0_,mbc0,l1,hl1)
 -- forkR :: AVL a -> b -> UBT5(AVL a,UINT,Maybe c,AVL a,UINT)
 forkR t0 e1 = forkR_ t0 L(0) where
  forkR_  E        h = UBT5(E,h,Nothing,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case comp e e1 of
                        Lt      ->                             case forkR_ r hr of
                                   UBT5(l0,hl0,mbc1,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                    UBT2(l0_,hl0_)          -> UBT5(l0_,hl0_,mbc1,l1,hl1)
                        Eq mbc1 -> UBT5(l,hl,mbc1,r,hr)
                        Gt      ->                             case forkR_ l hl of
                                   UBT5(l0,hl0,mbc1,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                    UBT2(l1_,hl1_)          -> UBT5(l0,hl0,mbc1,l1_,hl1_)
-----------------------------------------------------------------------
---------------- intersectionMaybeToList Ends Here ----------------
-----------------------------------------------------------------------

-- | Applies 'intersectionMaybeToList' to the empty list.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
intersectionMaybeAsList :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> [c]
intersectionMaybeAsList c setA setB = intersectionMaybeToList c setA setB []

-- | Uses the supplied comparison to evaluate the difference between two sets represented as
-- sorted AVL trees. The expression..
--
-- > difference cmp setA setB
--
-- .. is a set containing all those elements of @setA@ which do not appear in @setB@.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
difference :: (a -> b -> Ordering) -> AVL a -> AVL b -> AVL a
-- N.B. differenceH works with relative heights on first tree, and needs no height for the second.
difference c t0 t1 = case differenceH c t0 L(0) t1 of UBT2(t,_) -> t

-- | Similar to 'difference', but the resulting tree also includes those elements a\' for which the
-- combining comparison returns @(Eq (Just a\'))@.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
differenceMaybe :: (a -> b -> COrdering (Maybe a)) -> AVL a -> AVL b -> AVL a
-- N.B. differenceMaybeH works with relative heights on first tree, and needs no height for the second.
differenceMaybe c t0 t1 = case differenceMaybeH c t0 L(0) t1 of UBT2(t,_) -> t

-- | Uses the supplied comparison to test whether the first set is a subset of the second,
-- both sets being represented as sorted AVL trees.  This function returns True if any of
-- the following conditions hold..
--
-- * The first set is empty (the empty set is a subset of any set).
--
-- * The two sets are equal.
--
-- * The first set is a proper subset of the second set.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
isSubsetOf :: (a -> b -> Ordering) -> AVL a -> AVL b -> Bool
isSubsetOf comp = s where
 -- s :: AVL a -> AVL b -> Bool
 s  E            _           = True
 s  _            E           = False
 s (N l0 e0 r0) (N l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (N l0 e0 r0) (Z l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (N l0 e0 r0) (P l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (Z l0 e0 r0) (N l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (Z l0 e0 r0) (Z l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (Z l0 e0 r0) (P l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (P l0 e0 r0) (N l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (P l0 e0 r0) (Z l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (P l0 e0 r0) (P l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s' l0 e0 r0 l1 e1 r1 =
  case comp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  LT -> case forkL e0 l1 of
        UBT5(False,_  ,_,_  ,_) -> False
        UBT5(True ,ll1,_,lr1,_) -> (s l0 ll1) && case forkR r0 e1 of  -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
              UBT4(rl0,_,rr0,_) -> (s rl0 lr1) && (s rr0 r1)          -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
  -- e0 = e1
  EQ -> (s l0 l1) && (s r0 r1)
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  GT -> case forkL e0 r1 of
        UBT5(False,_  ,_,_  ,_) -> False
        UBT5(True ,rl1,_,rr1,_) -> (s r0 rr1) && case forkR l0 e1 of  -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
              UBT4(ll0,_,lr0,_) -> (s lr0 rl1) && (s ll0 l1)          -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
 -- forkL returns False if t1 does not contain e0 (which implies set 0 cannot be a subset of set 1)
 -- forkL :: a -> AVL b -> UBT5(Bool,AVL b,UINT,AVL b,UINT) -- Vals 1..4 only valid if Bool is True!
 forkL e0 t = forkL_ t L(0) where
  forkL_  E        h = UBT5(False,E,h,E,h)
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case comp e0 e of
                        LT -> case forkL_ l hl of
                              UBT5(False,t0,ht0,t1,ht1) -> UBT5(False,t0,ht0,t1,ht1)
                              UBT5(True ,t0,ht0,t1,ht1) -> case spliceH t1 ht1 e r hr of
                                                           UBT2(t1_,ht1_) -> UBT5(True,t0,ht0,t1_,ht1_)
                        EQ -> UBT5(True,l,hl,r,hr)
                        GT -> case forkL_ r hr of
                              UBT5(False,t0,ht0,t1,ht1) -> UBT5(False,t0,ht0,t1,ht1)
                              UBT5(True ,t0,ht0,t1,ht1) -> case spliceH l hl e t0 ht0 of
                                                           UBT2(t0_,ht0_) -> UBT5(True,t0_,ht0_,t1,ht1)
 -- forkR discards an element from set 0 if it is equal to the element from set 1
 -- forkR :: AVL a -> b -> UBT4(AVL a,UINT,AVL a,UINT)
 forkR t e1 = forkR_ t L(0) where
  forkR_  E        h = UBT4(E,h,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case comp e e1 of
                        LT -> case forkR_ r hr of
                              UBT4(t0,ht0,t1,ht1) -> case spliceH l hl e t0 ht0 of
                               UBT2(t0_,ht0_)     -> UBT4(t0_,ht0_,t1,ht1)
                        EQ -> UBT4(l,hl,r,hr)     -- e is discarded from set 0
                        GT -> case forkR_ l hl of
                              UBT4(t0,ht0,t1,ht1) -> case spliceH t1 ht1 e r hr of
                               UBT2(t1_,ht1_)     -> UBT4(t0,ht0,t1_,ht1_)
-----------------------------------------------------------------------
------------------------ isSubsetOf Ends Here ----------------------
-----------------------------------------------------------------------

-- | Similar to 'isSubsetOf', but also requires that the supplied combining
-- comparison returns @('Eq' True)@ for matching elements.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
isSubsetOfBy :: (a -> b -> COrdering Bool) -> AVL a -> AVL b -> Bool
isSubsetOfBy comp = s where
 -- s :: AVL a -> AVL b -> Bool
 s  E            _           = True
 s  _            E           = False
 s (N l0 e0 r0) (N l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (N l0 e0 r0) (Z l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (N l0 e0 r0) (P l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (Z l0 e0 r0) (N l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (Z l0 e0 r0) (Z l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (Z l0 e0 r0) (P l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (P l0 e0 r0) (N l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (P l0 e0 r0) (Z l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s (P l0 e0 r0) (P l1 e1 r1) = s' l0 e0 r0 l1 e1 r1
 s' l0 e0 r0 l1 e1 r1 =
  case comp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt       -> case forkL e0 l1 of
              UBT5(False,_  ,_,_  ,_)  -> False
              UBT5(True ,ll1,_,lr1,_)  -> (s l0 ll1) && case forkR r0 e1 of -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
               UBT5(False,_  ,_,_  ,_) -> False
               UBT5(True ,rl0,_,rr0,_) -> (s rl0 lr1) && (s rr0 r1)         -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
  -- e0 = e1
  Eq True  -> (s l0 l1) && (s r0 r1)
  Eq False -> False
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt       -> case forkL e0 r1 of
              UBT5(False,_  ,_,_  ,_)  -> False
              UBT5(True ,rl1,_,rr1,_)  -> (s r0 rr1) && case forkR l0 e1 of  -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
               UBT5(False,_  ,_,_  ,_) -> False
               UBT5(True ,ll0,_,lr0,_) -> (s lr0 rl1) && (s ll0 l1)          -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
 -- forkL returns False if t1 does not contain e0 (which implies set 0 cannot be a subset of set 1)
 -- forkL :: a -> AVL b -> UBT5(Bool,AVL b,UINT,AVL b,UINT) -- Vals 1..4 only valid if Bool is True!
 forkL e0 t = forkL_ t L(0) where
  forkL_  E        h = UBT5(False,E,h,E,h)
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case comp e0 e of
                        Lt   -> case forkL_ l hl of
                                UBT5(False,t0,ht0,t1,ht1) -> UBT5(False,t0,ht0,t1,ht1)
                                UBT5(True ,t0,ht0,t1,ht1) -> case spliceH t1 ht1 e r hr of
                                                             UBT2(t1_,ht1_) -> UBT5(True,t0,ht0,t1_,ht1_)
                        Eq b -> UBT5(b,l,hl,r,hr)
                        Gt   -> case forkL_ r hr of
                                UBT5(False,t0,ht0,t1,ht1) -> UBT5(False,t0,ht0,t1,ht1)
                                UBT5(True ,t0,ht0,t1,ht1) -> case spliceH l hl e t0 ht0 of
                                                             UBT2(t0_,ht0_) -> UBT5(True,t0_,ht0_,t1,ht1)
 -- forkR discards an element from set 0 if it is equal to the element from set 1
 -- forkR :: AVL a -> b -> UBT5(Bool,AVL a,UINT,AVL a,UINT)  -- Vals 1..4 only valid if Bool is True!
 forkR t e1 = forkR_ t L(0) where
  forkR_  E        h = UBT5(True,E,h,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case comp e e1 of
                        Lt   -> case forkR_ r hr of
                                UBT5(False,t0,ht0,t1,ht1) -> UBT5(False,t0,ht0,t1,ht1)
                                UBT5(True ,t0,ht0,t1,ht1) -> case spliceH l hl e t0 ht0 of
                                                             UBT2(t0_,ht0_) -> UBT5(True,t0_,ht0_,t1,ht1)
                        Eq b -> UBT5(b,l,hl,r,hr)     -- e is discarded from set 0
                        Gt   -> case forkR_ l hl of
                                UBT5(False,t0,ht0,t1,ht1) -> UBT5(False,t0,ht0,t1,ht1)
                                UBT5(True ,t0,ht0,t1,ht1) -> case spliceH t1 ht1 e r hr of
                                                             UBT2(t1_,ht1_) -> UBT5(True,t0,ht0,t1_,ht1_)
-----------------------------------------------------------------------
----------------------- isSubsetOfBy Ends Here ---------------------
-----------------------------------------------------------------------

-- | The symmetric difference is the set of elements which occur in one set or the other but /not both/.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
symDifference :: (e -> e -> Ordering) -> AVL e -> AVL e -> AVL e
symDifference c = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = t1
 gu t0                 E          = t0
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(1) l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(1) l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(1) l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) r1)
 gu_ t0 h0 t1 h1 = case symDifferenceH c t0 h0 t1 h1 of UBT2(t,_) -> t

-- | Given two Sets @A@ and @B@ represented as sorted AVL trees, this function
-- extracts the \'Venn diagram\' components @A-B@, @A.B@ and @B-A@.
-- See also 'vennMaybe'.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
venn :: (a -> b -> COrdering c) -> AVL a -> AVL b -> (AVL a, AVL c, AVL b)
venn c = gu where  -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = (E ,E,t1)
 gu t0                 E          = (t0,E,E )
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(1) l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(1) l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(1) l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) r1)
 gu_ t0 h0 t1 h1 = case vennH c [] L(0) t0 h0 t1 h1 of
                   UBT6(tab,_,cs,cl,tba,_) -> let tc = asTreeLenL ASINT(cl) cs
                                              in tc `seq` (tab,tc,tba)

-- | Similar to 'venn', but intersection elements for which the combining comparison
-- returns @('Eq' 'Nothing')@ are deleted from the intersection result.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
vennMaybe :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> (AVL a, AVL c, AVL b)
vennMaybe c = gu where -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = (E ,E,t1)
 gu t0                 E          = (t0,E,E )
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(1) l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(1) l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(1) l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) r1)
 gu_ t0 h0 t1 h1 = case vennMaybeH c [] L(0) t0 h0 t1 h1 of
                   UBT6(tab,_,cs,cl,tba,_) -> let tc = asTreeLenL ASINT(cl) cs
                                              in tc `seq` (tab,tc,tba)

-- | Same as 'venn', but prepends the intersection component to the supplied list
-- in ascending order.
vennToList :: (a -> b -> COrdering c) -> [c] -> AVL a -> AVL b -> (AVL a, [c], AVL b)
vennToList cmp cs = gu where  -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = (E ,cs,t1)
 gu t0                 E          = (t0,cs,E )
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(1) l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(1) l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(1) l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) r1)
 gu_ t0 h0 t1 h1 = case vennH cmp cs L(0) t0 h0 t1 h1 of
                   UBT6(tab,_,cs_,_,tba,_) -> (tab,cs_,tba)

-- | Same as 'vennMaybe', but prepends the intersection component to the supplied list
-- in ascending order.
vennMaybeToList  :: (a -> b -> COrdering (Maybe c)) -> [c] -> AVL a -> AVL b -> (AVL a, [c], AVL b)
vennMaybeToList cmp cs = gu where  -- This is to avoid O(log n) height calculation for empty sets
 gu     E          t1             = (E ,cs,t1)
 gu t0                 E          = (t0,cs,E )
 gu t0@(N l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) l1)
 gu t0@(N l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(1) l1)
 gu t0@(N l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) l0) t1 (addHeight L(2) r1)
 gu t0@(Z l0 _ _ ) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) l1)
 gu t0@(Z l0 _ _ ) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(1) l1)
 gu t0@(Z l0 _ _ ) t1@(P _  _ r1) = gu_ t0 (addHeight L(1) l0) t1 (addHeight L(2) r1)
 gu t0@(P _  _ r0) t1@(N l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) l1)
 gu t0@(P _  _ r0) t1@(Z l1 _ _ ) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(1) l1)
 gu t0@(P _  _ r0) t1@(P _  _ r1) = gu_ t0 (addHeight L(2) r0) t1 (addHeight L(2) r1)
 gu_ t0 h0 t1 h1 = case vennMaybeH cmp cs L(0) t0 h0 t1 h1 of
                   UBT6(tab,_,cs_,_,tba,_) -> (tab,cs_,tba)

-- | Same as 'venn', but returns the intersection component as a list in ascending order.
-- This is just 'vennToList' applied to an empty initial intersection list.
vennAsList :: (a -> b -> COrdering c) -> AVL a -> AVL b -> (AVL a, [c], AVL b)
vennAsList cmp = vennToList cmp []
{-# INLINE vennAsList #-}

-- | Same as 'vennMaybe', but returns the intersection component as a list in ascending order.
-- This is just 'vennMaybeToList' applied to an empty initial intersection list.
vennMaybeAsList  :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> (AVL a, [c], AVL b)
vennMaybeAsList cmp = vennMaybeToList cmp []
{-# INLINE vennMaybeAsList #-}

