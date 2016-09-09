{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Split
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.Split
(-- * Splitting AVL trees

 -- ** Taking fixed size lumps of tree
 -- | Bear in mind that the tree size (s) is not stored in the AVL data structure, but if it is
 -- already known for other reasons then for (n > s\/2) using the appropriate complementary
 -- function with argument (s-n) will be faster.
 -- But it's probably not worth invoking 'Data.Tree.AVL.Types.size' for no reason other than to
 -- exploit this optimisation (because this is O(s) anyway).
 splitAtL,splitAtR,takeL,takeR,dropL,dropR,

 -- ** Rotations
 -- | Bear in mind that the tree size (s) is not stored in the AVL data structure, but if it is
 -- already known for other reasons then for (n > s\/2) using the appropriate complementary
 -- function with argument (s-n) will be faster.
 -- But it's probably not worth invoking 'Data.Tree.AVL.Types.size' for no reason other than to exploit this optimisation
 -- (because this is O(s) anyway).
 rotateL,rotateR,popRotateL,popRotateR,rotateByL,rotateByR,

 -- ** Taking lumps of tree according to a supplied predicate
 spanL,spanR,takeWhileL,dropWhileL,takeWhileR,dropWhileR,

 -- ** Taking lumps of /sorted/ trees
 -- | Prepare to get confused. All these functions adhere to the same Ordering convention as
 -- is used for searches. That is, if the supplied selector returns LT that means the search
 -- key is less than the current tree element. Or put another way, the current tree element
 -- is greater than the search key.
 --
 -- So (for example) the result of the 'takeLT' function is a tree containing all those elements
 -- which are less than the notional search key. That is, all those elements for which the
 -- supplied selector returns GT (not LT as you might expect). I know that seems backwards, but
 -- it's consistent if you think about it.
 forkL,forkR,fork,
 takeLE,dropGT,
 takeLT,dropGE,
 takeGT,dropLE,
 takeGE,dropLT,
) where

import Prelude -- so haddock finds the symbols there


import Data.COrdering(COrdering(..))
import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.Push(pushL,pushR)
import Data.Tree.AVL.Internals.DelUtils(popRN,popRZ,popRP,popLN,popLZ,popLP)
import Data.Tree.AVL.Internals.HAVL(HAVL(HAVL),spliceHAVL,pushLHAVL,pushRHAVL)
import Data.Tree.AVL.Internals.HJoin(joinH')

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- Local Datatype for results of split operations.
data SplitResult e = All  (HAVL e) (HAVL e)     -- Two tree/height pairs. Non Strict!!
                   | More {-# UNPACK #-} !UINT  -- No of tree elements still required (>=0!!)

-- | Split an AVL tree from the Left. The 'Int' argument n (n >= 0) specifies the split point.
-- This function raises an error if n is negative.
--
-- If the tree size is greater than n the result is (Right (l,r)) where l contains
-- the leftmost n elements and r contains the remaining rightmost elements (r will be non-empty).
--
-- If the tree size is less than or equal to n then the result is (Left s), where s is tree size.
--
-- An empty tree will always yield a result of (Left 0).
--
-- Complexity: O(n)
splitAtL :: Int -> AVL e -> Either Int (AVL e, AVL e)
splitAtL n _ | n < 0  = error "splitAtL: Negative argument."
splitAtL 0        E = Left 0       -- Treat this case specially
splitAtL 0        t = Right (E,t)
splitAtL ASINT(n) t = case splitL n t L(0) of -- Tree Heights are relative!!
                      More n_                   -> Left ASINT(SUBINT(n,n_))
                      All (HAVL l _) (HAVL r _) -> Right (l,r)

-- n > 0 !!
-- N.B Never returns a result of form (ALL lhavl rhavl) where rhavl is empty
splitL :: UINT -> AVL e -> UINT -> SplitResult e
splitL n  E        _ = More n
splitL n (N l e r) h = splitL_ n l DECINT2(h) e r DECINT1(h)
splitL n (Z l e r) h = splitL_ n l DECINT1(h) e r DECINT1(h)
splitL n (P l e r) h = splitL_ n l DECINT1(h) e r DECINT2(h)

-- n > 0 !!
-- N.B Never returns a result of form (ALL lhavl rhavl) where rhavl is empty
splitL_ :: UINT -> AVL e -> UINT -> e -> AVL e -> UINT -> SplitResult e
splitL_ n l hl e r hr =
 case splitL n l hl of
 More L(0)         -> let rhavl = pushLHAVL e (HAVL r hr); lhavl = HAVL l hl
                      in  lhavl `seq` rhavl `seq` All lhavl rhavl
 More L(1)         -> case r of
                      E       -> More L(0)
                      _       -> let lhavl = pushRHAVL (HAVL l hl) e
                                     rhavl = HAVL r hr
                                 in  lhavl `seq` rhavl `seq` All lhavl rhavl
 More n_           -> let sr = splitL DECINT1(n_) r hr
                      in case sr of
                         More _          -> sr
                         All havl0 havl1 -> let havl0' = spliceHAVL (HAVL l hl) e havl0
                                            in  havl0' `seq` All havl0' havl1
 All havl0 havl1   -> let havl1' = spliceHAVL havl1 e (HAVL r hr)
                      in  havl1' `seq` All havl0 havl1'
-----------------------------------------------------------------------
------------------------- splitAtL Ends Here --------------------------
-----------------------------------------------------------------------

-- | Split an AVL tree from the Right. The 'Int' argument n (n >= 0) specifies the split point.
-- This function raises an error if n is negative.
--
-- If the tree size is greater than n the result is (Right (l,r)) where r contains
-- the rightmost n elements and l contains the remaining leftmost elements (l will be non-empty).
--
-- If the tree size is less than or equal to n then the result is (Left s), where s is tree size.
--
-- An empty tree will always yield a result of (Left 0).
--
-- Complexity: O(n)
splitAtR :: Int -> AVL e -> Either Int (AVL e, AVL e)
splitAtR n        _ | n < 0  = error "splitAtR: Negative argument."
splitAtR 0        E = Left 0       -- Treat this case specially
splitAtR 0        t = Right (t,E)
splitAtR ASINT(n) t = case splitR n t L(0) of -- Tree Heights are relative!!
                      More n_                   -> Left ASINT(SUBINT(n,n_))
                      All (HAVL l _) (HAVL r _) -> Right (l,r)

-- n > 0 !!
-- N.B Never returns a result of form (ALL lhavl rhavl) where lhavl is empty
splitR :: UINT -> AVL e -> UINT -> SplitResult e
splitR n  E        _ = More n
splitR n (N l e r) h = splitR_ n l DECINT2(h) e r DECINT1(h)
splitR n (Z l e r) h = splitR_ n l DECINT1(h) e r DECINT1(h)
splitR n (P l e r) h = splitR_ n l DECINT1(h) e r DECINT2(h)

-- n > 0 !!
-- N.B Never returns a result of form (ALL lhavl rhavl) where lhavl is empty
splitR_ :: UINT -> AVL e -> UINT -> e -> AVL e -> UINT -> SplitResult e
splitR_ n l hl e r hr =
 case splitR n r hr of
 More L(0)         -> let lhavl = pushRHAVL (HAVL l hl) e; rhavl = HAVL r hr
                      in  lhavl `seq` rhavl `seq` All lhavl rhavl
 More L(1)         -> case l of
                      E       -> More L(0)
                      _       -> let rhavl = pushLHAVL e (HAVL r hr)
                                     lhavl = HAVL l hl
                                 in  lhavl `seq` rhavl `seq` All lhavl rhavl
 More n_           -> let sr = splitR DECINT1(n_) l hl
                      in case sr of
                         More _          -> sr
                         All havl0 havl1 -> let havl1' = spliceHAVL havl1 e (HAVL r hr)
                                            in  havl1' `seq` All havl0 havl1'
 All havl0 havl1   -> let havlO' = spliceHAVL (HAVL l hl) e havl0
                      in  havlO' `seq` All havlO' havl1
-----------------------------------------------------------------------
------------------------- splitAtR Ends Here --------------------------
-----------------------------------------------------------------------

-- Local Datatype for results of take/drop operations.
data TakeResult e = AllTR (HAVL e)               -- The resulting Tree
                  | MoreTR {-# UNPACK #-} !UINT  -- No of tree elements still required (>=0!!)

-- | This is a simplified version of 'splitAtL' which does not return the remaining tree.
-- The 'Int' argument n (n >= 0) specifies the number of elements to take (from the left).
-- This function raises an error if n is negative.
--
-- If the tree size is greater than n the result is (Right l) where l contains
-- the leftmost n elements.
--
-- If the tree size is less than or equal to n then the result is (Left s), where s is tree size.
--
-- An empty tree will always yield a result of (Left 0).
--
-- Complexity: O(n)
takeL :: Int -> AVL e -> Either Int (AVL e)
takeL n _ | n < 0  = error "takeL: Negative argument."
takeL 0        E = Left 0       -- Treat this case specially
takeL 0        _ = Right E
takeL ASINT(n) t = case takeL_ n t L(0) of -- Tree Heights are relative!!
                   MoreTR n_         -> Left ASINT(SUBINT(n,n_))
                   AllTR (HAVL t' _) -> Right t'

-- n > 0 !!
takeL_ :: UINT -> AVL e -> UINT -> TakeResult e
takeL_ n  E        _ = MoreTR n
takeL_ n (N l e r) h = takeL__ n l DECINT2(h) e r DECINT1(h)
takeL_ n (Z l e r) h = takeL__ n l DECINT1(h) e r DECINT1(h)
takeL_ n (P l e r) h = takeL__ n l DECINT1(h) e r DECINT2(h)

-- n > 0 !!
takeL__ :: UINT -> AVL e -> UINT -> e -> AVL e -> UINT -> TakeResult e
takeL__ n l hl e r hr =
 let takel = takeL_ n l hl
 in case takel of
    MoreTR L(0) -> let lhavl = HAVL l hl
                   in  lhavl `seq` AllTR lhavl
    MoreTR L(1) -> case r of
                   E       -> MoreTR L(0)
                   _       -> let lhavl = pushRHAVL (HAVL l hl) e
                              in  lhavl `seq` AllTR lhavl
    MoreTR n_   -> let taker = takeL_ DECINT1(n_) r hr
                   in case taker of
                      AllTR havl0 -> let havl0' = spliceHAVL (HAVL l hl) e havl0
                                     in  havl0' `seq` AllTR havl0'
                      _           -> taker
    _           -> takel
-----------------------------------------------------------------------
-------------------------- takeL Ends Here ----------------------------
-----------------------------------------------------------------------

-- | This is a simplified version of 'splitAtR' which does not return the remaining tree.
-- The 'Int' argument n (n >= 0) specifies the number of elements to take (from the right).
-- This function raises an error if n is negative.
--
-- If the tree size is greater than n the result is (Right r) where r contains
-- the rightmost n elements.
--
-- If the tree size is less than or equal to n then the result is (Left s), where s is tree size.
--
-- An empty tree will always yield a result of (Left 0).
--
-- Complexity: O(n)
takeR :: Int -> AVL e -> Either Int (AVL e)
takeR n _ | n < 0  = error "takeR: Negative argument."
takeR 0        E = Left 0       -- Treat this case specially
takeR 0        _ = Right E
takeR ASINT(n) t = case takeR_ n t L(0) of -- Tree Heights are relative!!
                   MoreTR n_         -> Left ASINT(SUBINT(n,n_))
                   AllTR (HAVL t' _) -> Right t'

-- n > 0 !!
takeR_ :: UINT -> AVL e -> UINT -> TakeResult e
takeR_ n  E        _ = MoreTR n
takeR_ n (N l e r) h = takeR__ n l DECINT2(h) e r DECINT1(h)
takeR_ n (Z l e r) h = takeR__ n l DECINT1(h) e r DECINT1(h)
takeR_ n (P l e r) h = takeR__ n l DECINT1(h) e r DECINT2(h)

-- n > 0 !!
takeR__ :: UINT -> AVL e -> UINT -> e -> AVL e -> UINT -> TakeResult e
takeR__ n l hl e r hr =
 let taker = takeR_ n r hr
 in case taker of
    MoreTR L(0) -> let rhavl = HAVL r hr
                   in  rhavl `seq` AllTR rhavl
    MoreTR L(1) -> case l of
                   E       -> MoreTR L(0)
                   _       -> let rhavl = pushLHAVL e (HAVL r hr)
                              in  rhavl `seq` AllTR rhavl
    MoreTR n_   -> let takel = takeR_ DECINT1(n_) l hl
                   in case takel of
                      AllTR havl0 -> let havl0' = spliceHAVL havl0 e (HAVL r hr)
                                     in  havl0' `seq` AllTR havl0'
                      _           -> takel
    _           -> taker
-----------------------------------------------------------------------
-------------------------- takeR Ends Here ----------------------------
-----------------------------------------------------------------------

-- | This is a simplified version of 'splitAtL' which returns the remaining tree only (rightmost elements).
-- This function raises an error if n is negative.
--
-- If the tree size is greater than n the result is (Right r) where r contains
-- the remaining elements (r will be non-empty).
--
-- If the tree size is less than or equal to n then the result is (Left s), where s is tree size.
--
-- An empty tree will always yield a result of (Left 0).
--
-- Complexity: O(n)
dropL :: Int -> AVL e -> Either Int (AVL e)
dropL n _ | n < 0  = error "dropL: Negative argument."
dropL 0        E = Left 0       -- Treat this case specially
dropL 0        t = Right t
dropL ASINT(n) t = case dropL_ n t L(0) of -- Tree Heights are relative!!
                   MoreTR n_        -> Left ASINT(SUBINT(n,n_))
                   AllTR (HAVL r _) -> Right r

-- n > 0 !!
-- N.B Never returns a result of form (AllTR rhavl) where rhavl is empty
dropL_ :: UINT -> AVL e -> UINT -> TakeResult e
dropL_ n  E        _ = MoreTR n
dropL_ n (N l e r) h = dropL__ n l DECINT2(h) e r DECINT1(h)
dropL_ n (Z l e r) h = dropL__ n l DECINT1(h) e r DECINT1(h)
dropL_ n (P l e r) h = dropL__ n l DECINT1(h) e r DECINT2(h)

-- n > 0 !!
-- N.B Never returns a result of form (AllTR rhavl) where rhavl is empty
dropL__ :: UINT -> AVL e -> UINT -> e -> AVL e -> UINT -> TakeResult e
dropL__ n l hl e r hr =
 case dropL_ n l hl of
 MoreTR L(0) -> let rhavl = pushLHAVL e (HAVL r hr)
                in  rhavl `seq` AllTR rhavl
 MoreTR L(1) -> case r of
                E  -> MoreTR L(0)
                _  -> let rhavl = HAVL r hr in rhavl `seq` AllTR rhavl
 MoreTR n_   -> dropL_ DECINT1(n_) r hr
 AllTR havl1 -> let havl1' = spliceHAVL havl1 e (HAVL r hr)
                in  havl1' `seq` AllTR havl1'
-----------------------------------------------------------------------
--------------------------- dropL Ends Here ---------------------------
-----------------------------------------------------------------------

-- | This is a simplified version of 'splitAtR' which returns the remaining tree only (leftmost elements).
-- This function raises an error if n is negative.
--
-- If the tree size is greater than n the result is (Right l) where l contains
-- the remaining elements (l will be non-empty).
--
-- If the tree size is less than or equal to n then the result is (Left s), where s is tree size.
--
-- An empty tree will always yield a result of (Left 0).
--
-- Complexity: O(n)
dropR :: Int -> AVL e -> Either Int (AVL e)
dropR n _ | n < 0  = error "dropL: Negative argument."
dropR 0        E = Left 0       -- Treat this case specially
dropR 0        t = Right t
dropR ASINT(n) t = case dropR_ n t L(0) of -- Tree Heights are relative!!
                   MoreTR n_        -> Left ASINT(SUBINT(n,n_))
                   AllTR (HAVL l _) -> Right l

-- n > 0 !!
-- N.B Never returns a result of form (AllTR lhavl) where lhavl is empty
dropR_ :: UINT -> AVL e -> UINT -> TakeResult e
dropR_ n  E        _ = MoreTR n
dropR_ n (N l e r) h = dropR__ n l DECINT2(h) e r DECINT1(h)
dropR_ n (Z l e r) h = dropR__ n l DECINT1(h) e r DECINT1(h)
dropR_ n (P l e r) h = dropR__ n l DECINT1(h) e r DECINT2(h)

-- n > 0 !!
-- N.B Never returns a result of form (AllTR lhavl) where lhavl is empty
dropR__ :: UINT -> AVL e -> UINT -> e -> AVL e -> UINT -> TakeResult e
dropR__ n l hl e r hr =
 case dropR_ n r hr of
 MoreTR L(0) -> let lhavl = pushRHAVL (HAVL l hl) e
                in  lhavl `seq` AllTR lhavl
 MoreTR L(1) -> case l of
                E  -> MoreTR L(0)
                _  -> let lhavl = HAVL l hl in lhavl `seq` AllTR lhavl
 MoreTR n_   -> dropR_ DECINT1(n_) l hl
 AllTR havl0 -> let havl0' = spliceHAVL (HAVL l hl) e havl0
                in  havl0' `seq` AllTR havl0'
-----------------------------------------------------------------------
--------------------------- dropR Ends Here ---------------------------
-----------------------------------------------------------------------


-- Local Datatype for results of span operations.
data SpanResult e = Some  (HAVL e) (HAVL e)     -- Two tree/height pairs. Non Strict!!
                  | TheLot                      -- The Lot satisfied

-- | Span an AVL tree from the left, using the supplied predicate. This function returns
-- a pair of trees (l,r), where l contains the leftmost consecutive elements which
-- satisfy the predicate. The leftmost element of r (if any) is the first to fail
-- the predicate. Either of the resulting trees may be empty. Element ordering is preserved.
--
-- Complexity: O(n), where n is the size of l.
spanL :: (e -> Bool) -> AVL e -> (AVL e, AVL e)
spanL p t = case spanIt t L(0) of -- Tree heights are relative
            TheLot                     -> (t, E)                  -- All satisfied
            Some (HAVL l _) (HAVL r _) -> (l, r)                  -- Some satisfied
 where
 spanIt   E        _ = TheLot
 spanIt  (N l e r) h = spanIt_ l DECINT2(h) e r DECINT1(h)
 spanIt  (Z l e r) h = spanIt_ l DECINT1(h) e r DECINT1(h)
 spanIt  (P l e r) h = spanIt_ l DECINT1(h) e r DECINT2(h)
 -- N.B: Never Returns (Some _ (HAVL E _)) (== TheLot)
 spanIt_ l hl e r hr =
  case spanIt l hl of
  Some havl0 havl1 -> let havl1_ = spliceHAVL havl1 e (HAVL r hr)
                      in  havl1_ `seq` Some havl0 havl1_
  TheLot           -> if p e
                      then let spanItr = spanIt r hr
                           in case spanItr of
                              Some havl0 havl1 -> let havl0_ = spliceHAVL (HAVL l hl) e havl0
                                                  in  havl0_ `seq` Some havl0_ havl1
                              _                -> spanItr
                      else let rhavl = pushLHAVL e (HAVL r hr)
                               lhavl = HAVL l hl
                           in lhavl `seq` rhavl `seq` Some lhavl rhavl
-----------------------------------------------------------------------
--------------------------- spanL Ends Here ---------------------------
-----------------------------------------------------------------------

-- | Span an AVL tree from the right, using the supplied predicate. This function returns
-- a pair of trees (l,r), where r contains the rightmost consecutive elements which
-- satisfy the predicate. The rightmost element of l (if any) is the first to fail
-- the predicate. Either of the resulting trees may be empty. Element ordering is preserved.
--
-- Complexity: O(n), where n is the size of r.
spanR :: (e -> Bool) -> AVL e -> (AVL e, AVL e)
spanR p t = case spanIt t L(0) of -- Tree heights are relative
            TheLot                     -> (E, t)                  -- All satisfied
            Some (HAVL l _) (HAVL r _) -> (l, r)                  -- Some satisfied
 where
 spanIt   E        _ = TheLot
 spanIt  (N l e r) h = spanIt_ l DECINT2(h) e r DECINT1(h)
 spanIt  (Z l e r) h = spanIt_ l DECINT1(h) e r DECINT1(h)
 spanIt  (P l e r) h = spanIt_ l DECINT1(h) e r DECINT2(h)
 -- N.B: Never Returns (Some (HAVL E _) _) (== TheLot)
 spanIt_ l hl e r hr =
  case spanIt r hr of
  Some havl0 havl1 -> let havl0_ = spliceHAVL (HAVL l hl) e havl0
                      in  havl0_ `seq` Some havl0_ havl1
  TheLot           -> if p e
                      then let spanItl = spanIt l hl
                           in case spanItl of
                              Some havl0 havl1 -> let havl1_ = spliceHAVL  havl1 e (HAVL r hr)
                                                  in  havl1_ `seq` Some havl0 havl1_
                              _                -> spanItl
                      else let lhavl = pushRHAVL (HAVL l hl) e
                               rhavl = HAVL r hr
                           in lhavl `seq` rhavl `seq` Some lhavl rhavl
-----------------------------------------------------------------------
--------------------------- spanR Ends Here ---------------------------
-----------------------------------------------------------------------

-- Local Datatype for results of takeWhile/DropWhile operations.
data TakeWhileResult e = SomeTW (HAVL e)
                       | TheLotTW

-- | This is a simplified version of 'spanL' which does not return the remaining tree
-- The result is the leftmost consecutive sequence of elements which satisfy the
-- supplied predicate (which may be empty).
--
-- Complexity: O(n), where n is the size of the result.
takeWhileL :: (e -> Bool) -> AVL e -> AVL e
takeWhileL p t = case spanIt t L(0) of    -- Tree heights are relative
                 TheLotTW          -> t   -- All satisfied
                 SomeTW (HAVL l _) -> l   -- Some satisfied
 where
 spanIt   E        _ = TheLotTW
 spanIt  (N l e r) h = spanIt_ l DECINT2(h) e r DECINT1(h)
 spanIt  (Z l e r) h = spanIt_ l DECINT1(h) e r DECINT1(h)
 spanIt  (P l e r) h = spanIt_ l DECINT1(h) e r DECINT2(h)
 spanIt_ l hl e r hr =
  let twl = spanIt l hl
  in case twl of
     TheLotTW -> if p e
                 then let twr = spanIt r hr
                      in case twr of
                      SomeTW havl0 -> let havl0_ = spliceHAVL (HAVL l hl) e havl0
                                      in  havl0_ `seq` SomeTW havl0_
                      _            -> twr
                 else let lhavl = HAVL l hl in lhavl `seq` SomeTW lhavl
     _        -> twl
-----------------------------------------------------------------------
------------------------- takeWhileL Ends Here ------------------------
-----------------------------------------------------------------------

-- | This is a simplified version of 'spanR' which does not return the remaining tree
-- The result is the rightmost consecutive sequence of elements which satisfy the
-- supplied predicate (which may be empty).
--
-- Complexity: O(n), where n is the size of the result.
takeWhileR :: (e -> Bool) -> AVL e -> AVL e
takeWhileR p t = case spanIt t L(0) of    -- Tree heights are relative
                 TheLotTW          -> t   -- All satisfied
                 SomeTW (HAVL r _) -> r   -- Some satisfied
 where
 spanIt   E        _ = TheLotTW
 spanIt  (N l e r) h = spanIt_ l DECINT2(h) e r DECINT1(h)
 spanIt  (Z l e r) h = spanIt_ l DECINT1(h) e r DECINT1(h)
 spanIt  (P l e r) h = spanIt_ l DECINT1(h) e r DECINT2(h)
 spanIt_ l hl e r hr =
  let twr = spanIt r hr
  in case twr of
     TheLotTW -> if p e
                 then let twl = spanIt l hl
                      in case twl of
                      SomeTW havl1 -> let havl1_ = spliceHAVL havl1 e (HAVL r hr)
                                      in  havl1_ `seq` SomeTW havl1_
                      _            -> twl
                 else let rhavl = HAVL r hr in rhavl `seq` SomeTW rhavl
     _        -> twr
-----------------------------------------------------------------------
------------------------- takeWhileR Ends Here ------------------------
-----------------------------------------------------------------------

-- | This is a simplified version of 'spanL' which does not return the tree containing
-- the elements which satisfy the supplied predicate.
-- The result is a tree whose leftmost element is the first to fail the predicate, starting from
-- the left (which may be empty).
--
-- Complexity: O(n), where n is the number of elements dropped.
dropWhileL :: (e -> Bool) -> AVL e -> AVL e
dropWhileL p t = case spanIt t L(0) of   -- Tree heights are relative
                 TheLotTW          -> E  -- All satisfied
                 SomeTW (HAVL r _) -> r  -- Some satisfied
 where
 spanIt   E        _ = TheLotTW
 spanIt  (N l e r) h = spanIt_ l DECINT2(h) e r DECINT1(h)
 spanIt  (Z l e r) h = spanIt_ l DECINT1(h) e r DECINT1(h)
 spanIt  (P l e r) h = spanIt_ l DECINT1(h) e r DECINT2(h)
 spanIt_ l hl e r hr =
  case spanIt l hl of
  SomeTW havl1 -> let havl1_ = spliceHAVL havl1 e (HAVL r hr)
                  in  havl1_ `seq` SomeTW havl1_
  TheLotTW     -> if p e
                  then spanIt r hr
                  else let rhavl = pushLHAVL e (HAVL r hr)
                       in rhavl `seq` SomeTW rhavl
-----------------------------------------------------------------------
---------------------- dropWhileL Ends Here ---------------------------
-----------------------------------------------------------------------

-- | This is a simplified version of 'spanR' which does not return the tree containing
-- the elements which satisfy the supplied predicate.
-- The result is a tree whose rightmost element is the first to fail the predicate, starting from
-- the right (which may be empty).
--
-- Complexity: O(n), where n is the number of elements dropped.
dropWhileR :: (e -> Bool) -> AVL e -> AVL e
dropWhileR p t = case spanIt t L(0) of   -- Tree heights are relative
                 TheLotTW          -> E  -- All satisfied
                 SomeTW (HAVL l _) -> l  -- Some satisfied
 where
 spanIt   E        _ = TheLotTW
 spanIt  (N l e r) h = spanIt_ l DECINT2(h) e r DECINT1(h)
 spanIt  (Z l e r) h = spanIt_ l DECINT1(h) e r DECINT1(h)
 spanIt  (P l e r) h = spanIt_ l DECINT1(h) e r DECINT2(h)
 spanIt_ l hl e r hr =
  case spanIt r hr of
  SomeTW havl0 -> let havl0_ = spliceHAVL (HAVL l hl) e havl0
                  in  havl0_ `seq` SomeTW havl0_
  TheLotTW     -> if p e
                  then spanIt l hl
                  else let lhavl = pushRHAVL (HAVL l hl) e
                       in lhavl `seq` SomeTW lhavl
-----------------------------------------------------------------------
---------------------- dropWhileR Ends Here ---------------------------
-----------------------------------------------------------------------


-- | Rotate an AVL tree one place left. This function pops the leftmost element and pushes into
-- the rightmost position. An empty tree yields an empty tree.
--
-- Complexity: O(log n)
rotateL :: AVL e -> AVL e
rotateL  E        = E
rotateL (N l e r) = case popLN l e r of UBT2(e_,t) -> pushR t e_
rotateL (Z l e r) = case popLZ l e r of UBT2(e_,t) -> pushR t e_
rotateL (P l e r) = case popLP l e r of UBT2(e_,t) -> pushR t e_

-- | Rotate an AVL tree one place right. This function pops the rightmost element and pushes into
-- the leftmost position. An empty tree yields an empty tree.
--
-- Complexity: O(log n)
rotateR :: AVL e -> AVL e
rotateR  E        = E
rotateR (N l e r) = case popRN l e r of UBT2(t,e_) -> pushL e_ t
rotateR (Z l e r) = case popRZ l e r of UBT2(t,e_) -> pushL e_ t
rotateR (P l e r) = case popRP l e r of UBT2(t,e_) -> pushL e_ t

-- | Similar to 'rotateL', but returns the rotated element. This function raises an error if
-- applied to an empty tree.
--
-- Complexity: O(log n)
popRotateL :: AVL e -> (e, AVL e)
popRotateL  E        = error "popRotateL: Empty tree."
popRotateL (N l e r) = case popLN l e r of UBT2(e_,t) -> popRotateL' e_ t
popRotateL (Z l e r) = case popLZ l e r of UBT2(e_,t) -> popRotateL' e_ t
popRotateL (P l e r) = case popLP l e r of UBT2(e_,t) -> popRotateL' e_ t
popRotateL' :: e -> AVL e -> (e, AVL e)
popRotateL' e t = let t' = pushR t e in t' `seq` (e,t')

-- | Similar to 'rotateR', but returns the rotated element. This function raises an error if
-- applied to an empty tree.
--
-- Complexity: O(log n)
popRotateR :: AVL e -> (AVL e, e)
popRotateR  E        = error "popRotateR: Empty tree."
popRotateR (N l e r) = case popRN l e r of UBT2(t,e_) -> popRotateR' t e_
popRotateR (Z l e r) = case popRZ l e r of UBT2(t,e_) -> popRotateR' t e_
popRotateR (P l e r) = case popRP l e r of UBT2(t,e_) -> popRotateR' t e_
popRotateR' :: AVL e -> e -> (AVL e, e)
popRotateR' t e = let t' = pushL e t in t' `seq` (t',e)


-- | Rotate an AVL tree left by n places. If s is the size of the tree then ordinarily n
-- should be in the range [0..s-1]. However, this function will deliver a correct result
-- for any n (n\<0 or n\>=s), the actual rotation being given by (n \`mod\` s) in such cases.
-- The result of rotating an empty tree is an empty tree.
--
-- Complexity: O(n)
rotateByL :: AVL e -> Int -> AVL e
rotateByL t ASINT(n) = case COMPAREUINT n L(0) of
                       LT -> rotateByR__ t NEGATE(n)
                       EQ -> t
                       GT -> rotateByL__ t n
-- n>=0!!
{-# INLINE rotateByL_ #-}
rotateByL_ :: AVL e -> UINT -> AVL e
rotateByL_ t L(0) = t
rotateByL_ t n    = rotateByL__ t n
-- n>0!!
rotateByL__ :: AVL e -> UINT -> AVL e
rotateByL__ E _ = E
rotateByL__ t n = case splitL n t L(0) of -- Tree Heights are relative!!
                  More L(0)       -> t
                  More m          -> let s  = SUBINT(n,m)      -- Actual size of tree, > 0!!
                                         n_ = _MODULO_(n,s)    -- Actual shift required, 0..s-1
                                     in if IS_TRUE(ADDINT(n_,n_) LEQ s)
                                        then rotateByL_  t n_            -- n_ may be 0 !!
                                        else rotateByR__ t SUBINT(s,n_)  -- (s-n_) can't be 0
                  All (HAVL l hl) (HAVL r hr) -> joinH' r hr l hl


-- | Rotate an AVL tree right by n places. If s is the size of the tree then ordinarily n
-- should be in the range [0..s-1]. However, this function will deliver a correct result
-- for any n (n\<0 or n\>=s), the actual rotation being given by (n \`mod\` s) in such cases.
-- The result of rotating an empty tree is an empty tree.
--
-- Complexity: O(n)
rotateByR :: AVL e -> Int -> AVL e
rotateByR t ASINT(n) = case COMPAREUINT n L(0) of
                       LT -> rotateByL__ t NEGATE(n)
                       EQ -> t
                       GT -> rotateByR__ t n
-- n>=0!!
{-# INLINE rotateByR_ #-}
rotateByR_ :: AVL e -> UINT -> AVL e
rotateByR_ t L(0) = t
rotateByR_ t n    = rotateByR__ t n
-- n>0!!
rotateByR__ :: AVL e -> UINT -> AVL e
rotateByR__ E _ = E
rotateByR__ t n = case splitR n t L(0) of -- Tree Heights are relative!!
                  More L(0)       -> t
                  More m          -> let s  = SUBINT(n,m)    -- Actual size of tree, > 0!!
                                         n_ = _MODULO_(n,s)    -- Actual shift required, 0..s-1
                                     in if IS_TRUE(ADDINT(n_,n_) LEQ s)
                                        then rotateByR_  t n_         -- n_ may be 0 !!
                                        else rotateByL__ t SUBINT(s,n_)  -- (s-n_) can_t be 0
                  All (HAVL l hl) (HAVL r hr) -> joinH' r hr l hl


-- | Divide a sorted AVL tree into left and right sorted trees (l,r), such that l contains all the
-- elements less than or equal to according to the supplied selector and r contains all the elements greater than
-- according to the supplied selector.
--
-- Complexity: O(log n)
forkL :: (e -> Ordering) -> AVL e -> (AVL e, AVL e)
forkL c avl = let (HAVL l _,HAVL r _) = forkL_ L(0) avl -- Tree heights are relative
                 in (l,r)
 where
 forkL_ h  E        = (HAVL E h, HAVL E h)
 forkL_ h (N l e r) = forkL__ l DECINT2(h) e r DECINT1(h)
 forkL_ h (Z l e r) = forkL__ l DECINT1(h) e r DECINT1(h)
 forkL_ h (P l e r) = forkL__ l DECINT1(h) e r DECINT2(h)
 forkL__ l hl e r hr = case c e of
                          -- Current element > pivot, so goes in right half
                          LT -> let (havl0,havl1) = forkL_ hl l
                                    havl1_ = spliceHAVL havl1 e (HAVL r hr)
                                in  havl1_ `seq` (havl0, havl1_)
                          -- Current element = pivot, so goes in left half and stop here
                          EQ -> let lhavl = pushRHAVL (HAVL l hl) e
                                    rhavl = HAVL r hr
                                in  lhavl `seq` rhavl `seq` (lhavl,rhavl)
                          -- Current element < pivot, so goes in left half
                          GT -> let (havl0,havl1) = forkL_ hr r
                                    havl0_ = spliceHAVL (HAVL l hl) e havl0
                                in  havl0_ `seq` (havl0_, havl1)

-- | Divide a sorted AVL tree into left and right sorted trees (l,r), such that l contains all the
-- elements less than supplied selector and r contains all the elements greater than or equal to the
-- supplied selector.
--
-- Complexity: O(log n)
forkR :: (e -> Ordering) -> AVL e -> (AVL e, AVL e)
forkR c avl = let (HAVL l _,HAVL r _) = forkR_ L(0) avl  -- Tree heights are relative
                 in (l,r)
 where
 forkR_ h  E        = (HAVL E h, HAVL E h)
 forkR_ h (N l e r) = forkR__ l DECINT2(h) e r DECINT1(h)
 forkR_ h (Z l e r) = forkR__ l DECINT1(h) e r DECINT1(h)
 forkR_ h (P l e r) = forkR__ l DECINT1(h) e r DECINT2(h)
 forkR__ l hl e r hr = case c e of
                          -- Current element > pivot, so goes in right half
                          LT -> let (havl0,havl1) = forkR_ hl l
                                    havl1_ = spliceHAVL havl1 e (HAVL r hr)
                                in  havl1_ `seq` (havl0, havl1_)
                          -- Current element = pivot, so goes in right half and stop here
                          EQ -> let rhavl = pushLHAVL e (HAVL r hr)
                                    lhavl = HAVL l hl
                                in  lhavl `seq` rhavl `seq` (lhavl, rhavl)
                          -- Current element < pivot, so goes in left half
                          GT -> let (havl0,havl1) = forkR_ hr r
                                    havl0_ = spliceHAVL (HAVL l hl) e havl0
                                in  havl0_ `seq` (havl0_, havl1)


-- | Similar to 'forkL' and 'forkR', but returns any equal element found (instead of
-- incorporating it into the left or right tree results respectively).
--
-- Complexity: O(log n)
fork :: (e -> COrdering a) -> AVL e -> (AVL e, Maybe a, AVL e)
fork c avl = let (HAVL l _, mba, HAVL r _) = fork_ L(0) avl -- Tree heights are relative
                in (l,mba,r)
 where
 fork_ h  E        = (HAVL E h, Nothing, HAVL E h)
 fork_ h (N l e r) = fork__ l DECINT2(h) e r DECINT1(h)
 fork_ h (Z l e r) = fork__ l DECINT1(h) e r DECINT1(h)
 fork_ h (P l e r) = fork__ l DECINT1(h) e r DECINT2(h)
 fork__ l hl e r hr = case c e of
                          -- Current element > pivot
                          Lt   -> let (havl0,mba,havl1) = fork_ hl l
                                      havl1_ = spliceHAVL havl1 e (HAVL r hr)
                                  in  havl1_ `seq` (havl0, mba, havl1_)
                          -- Current element = pivot
                          Eq a -> let lhavl = HAVL l hl
                                      rhavl = HAVL r hr
                                  in  lhavl `seq` rhavl `seq` (lhavl, Just a, rhavl)
                          -- Current element < pivot
                          Gt   -> let (havl0,mba,havl1) = fork_ hr r
                                      havl0_ = spliceHAVL (HAVL l hl) e havl0
                                  in  havl0_ `seq` (havl0_, mba, havl1)

-- | This is a simplified version of 'forkL' which returns a sorted tree containing
-- only those elements which are less than or equal to according to the supplied selector.
-- This function also has the synonym 'dropGT'.
--
-- Complexity: O(log n)
takeLE :: (e -> Ordering) -> AVL e -> AVL e
takeLE c avl = let HAVL l _ = forkL_ L(0) avl -- Tree heights are relative
                  in l
 where
 forkL_ h  E        = HAVL E h
 forkL_ h (N l e r) = forkL__ l DECINT2(h) e r DECINT1(h)
 forkL_ h (Z l e r) = forkL__ l DECINT1(h) e r DECINT1(h)
 forkL_ h (P l e r) = forkL__ l DECINT1(h) e r DECINT2(h)
 forkL__ l hl e r hr = case c e of
                          LT -> forkL_ hl l
                          EQ -> pushRHAVL (HAVL l hl) e
                          GT -> let havl0 = forkL_ hr r
                                in  spliceHAVL (HAVL l hl) e havl0


-- | A synonym for 'takeLE'.
--
-- Complexity: O(log n)
dropGT :: (e -> Ordering) -> AVL e -> AVL e
dropGT = takeLE
{-# INLINE dropGT #-}

-- | This is a simplified version of 'forkL' which returns a sorted tree containing
-- only those elements which are greater according to the supplied selector.
-- This function also has the synonym 'dropLE'.
--
-- Complexity: O(log n)
takeGT :: (e -> Ordering) -> AVL e -> AVL e
takeGT c avl = let HAVL r _ = forkL_ L(0) avl -- Tree heights are relative
                  in r
 where
 forkL_ h  E        = HAVL E h
 forkL_ h (N l e r) = forkL__ l DECINT2(h) e r DECINT1(h)
 forkL_ h (Z l e r) = forkL__ l DECINT1(h) e r DECINT1(h)
 forkL_ h (P l e r) = forkL__ l DECINT1(h) e r DECINT2(h)
 forkL__ l hl e r hr = case c e of
                          LT -> let havl1  = forkL_ hl l
                                in  spliceHAVL havl1 e (HAVL r hr)
                          EQ -> HAVL r hr
                          GT -> forkL_ hr r

-- | A synonym for 'takeGT'.
--
-- Complexity: O(log n)
dropLE :: (e -> Ordering) -> AVL e -> AVL e
dropLE = takeGT
{-# INLINE dropLE #-}

-- | This is a simplified version of 'forkR' which returns a sorted tree containing
-- only those elements which are less than according to the supplied selector.
-- This function also has the synonym 'dropGE'.
--
-- Complexity: O(log n)
takeLT :: (e -> Ordering) -> AVL e -> AVL e
takeLT c avl = let HAVL l _ = forkL_ L(0) avl -- Tree heights are relative
                  in l
 where
 forkL_ h  E        = HAVL E h
 forkL_ h (N l e r) = forkL__ l DECINT2(h) e r DECINT1(h)
 forkL_ h (Z l e r) = forkL__ l DECINT1(h) e r DECINT1(h)
 forkL_ h (P l e r) = forkL__ l DECINT1(h) e r DECINT2(h)
 forkL__ l hl e r hr = case c e of
                          LT -> forkL_ hl l
                          EQ -> HAVL l hl
                          GT -> let havl0 = forkL_ hr r
                                in  spliceHAVL (HAVL l hl) e havl0


-- | A synonym for 'takeLT'.
--
-- Complexity: O(log n)
dropGE :: (e -> Ordering) -> AVL e -> AVL e
dropGE = takeLT
{-# INLINE dropGE #-}

-- | This is a simplified version of 'forkR' which returns a sorted tree containing
-- only those elements which are greater or equal to according to the supplied selector.
-- This function also has the synonym 'dropLT'.
--
-- Complexity: O(log n)
takeGE :: (e -> Ordering) -> AVL e -> AVL e
takeGE c avl = let HAVL r _ = forkL_ L(0) avl -- Tree heights are relative
               in r
 where
 forkL_ h  E        = HAVL E h
 forkL_ h (N l e r) = forkL__ l DECINT2(h) e r DECINT1(h)
 forkL_ h (Z l e r) = forkL__ l DECINT1(h) e r DECINT1(h)
 forkL_ h (P l e r) = forkL__ l DECINT1(h) e r DECINT2(h)
 forkL__ l hl e r hr = case c e of
                          LT -> let havl1  = forkL_ hl l
                                in  spliceHAVL havl1 e (HAVL r hr)
                          EQ -> pushLHAVL e (HAVL r hr)
                          GT -> forkL_ hr r

-- | A synonym for 'takeGE'.
--
-- Complexity: O(log n)
dropLT :: (e -> Ordering) -> AVL e -> AVL e
dropLT = takeGE
{-# INLINE dropLT #-}

