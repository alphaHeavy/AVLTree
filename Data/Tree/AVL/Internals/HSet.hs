{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Internals.HSet
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- Set primitives on AVL trees with (height information supplied where needed).
-- All the functions in this module use essentially the same symetric \"Divide and Conquer\" algorithm.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Internals.HSet
        (-- * Union primitives.
         unionH,unionMaybeH,disjointUnionH,

         -- * Intersection primitives.
         intersectionH,intersectionMaybeH,

         -- * Difference primitives.
         differenceH,differenceMaybeH,symDifferenceH,

         -- * Venn primitives
         vennH,vennMaybeH,
        ) where

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.Internals.HJoin(spliceH,joinH)

import Data.COrdering

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | Uses the supplied combining comparison to evaluate the union of two sets represented as
-- sorted AVL trees of known height. Whenever the combining comparison is applied, the first
-- comparison argument is an element of the first tree and the second comparison argument is
-- an element of the second tree.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
-- (Faster than Hedge union from Data.Set at any rate).
unionH :: (e -> e -> COrdering e) -> AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
unionH c = u where
 -- u :: AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
 u  E           _   t1          h1 = UBT2(t1,h1)
 u  t0          h0  E           _  = UBT2(t0,h0)
 u (N l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (N l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (N l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u (Z l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (Z l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (Z l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u (P l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (P l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (P l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u_ l0 hl0 e0 r0 hr0 l1 hl1 e1 r1 hr1 =
  case c e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt   ->                                 case forkR r0 hr0 e1 of
          UBT5(rl0,hrl0,e1_,rr0,hrr0)  -> case forkL e0 l1 hl1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
           UBT5(ll1,hll1,e0_,lr1,hlr1) ->                         -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
            -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                          case u  l0  hl0 ll1 hll1 of
            UBT2(l,hl)                 -> case u rl0 hrl0 lr1 hlr1 of
             UBT2(m,hm)                -> case u rr0 hrr0  r1  hr1 of
              UBT2(r,hr)               -> case spliceH m hm e1_ r hr of
               UBT2(t,ht)              -> spliceH l hl e0_ t ht
  -- e0 = e1
  Eq e ->                case u l0 hl0 l1 hl1 of
          UBT2(l,hl)  -> case u r0 hr0 r1 hr1 of
           UBT2(r,hr) -> spliceH l hl e r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt   ->                                 case forkL e0 r1 hr1 of
          UBT5(rl1,hrl1,e0_,rr1,hrr1)  -> case forkR l0 hl0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
           UBT5(ll0,hll0,e1_,lr0,hlr0) ->                         -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                          case u ll0 hll0  l1  hl1 of
            UBT2(l,hl)                 -> case u lr0 hlr0 rl1 hrl1 of
             UBT2(m,hm)                -> case u  r0  hr0 rr1 hrr1 of
              UBT2(r,hr)               -> case spliceH l hl e1_ m hm of
               UBT2(t,ht)              -> spliceH t ht e0_ r hr
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1)
 -- forkL :: e -> AVL e -> UINT -> UBT5(AVL e,UINT,e,AVL e,UINT)
 forkL e0 t1 ht1 = forkL_ t1 ht1 where
  forkL_  E        _ = UBT5(E, L(0), e0, E, L(0))
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case c e0 e of
                        Lt     ->                            case forkL_ l hl of
                                  UBT5(l0,hl0,e0_,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                   UBT2(l1_,hl1_)         -> UBT5(l0,hl0,e0_,l1_,hl1_)
                        Eq e0_ -> UBT5(l,hl,e0_,r,hr)
                        Gt     ->                            case forkL_ r hr of
                                  UBT5(l0,hl0,e0_,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                   UBT2(l0_,hl0_)         -> UBT5(l0_,hl0_,e0_,l1,hl1)
 -- forkR :: AVL e -> UINT -> e -> UBT5(AVL e,UINT,e,AVL e,UINT)
 forkR t0 ht0 e1 = forkR_ t0 ht0 where
  forkR_  E        _ = UBT5(E, L(0), e1, E, L(0))
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case c e e1 of
                        Lt     ->                            case forkR_ r hr of
                                  UBT5(l0,hl0,e1_,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                   UBT2(l0_,hl0_)         -> UBT5(l0_,hl0_,e1_,l1,hl1)
                        Eq e1_ -> UBT5(l,hl,e1_,r,hr)
                        Gt     ->                            case forkR_ l hl of
                                  UBT5(l0,hl0,e1_,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                   UBT2(l1_,hl1_)         -> UBT5(l0,hl0,e1_,l1_,hl1_)
-----------------------------------------------------------------------
-------------------------- unionH Ends Here ---------------------------
-----------------------------------------------------------------------

-- | Similar to _unionH_, but the resulting tree does not include elements in cases where
-- the supplied combining comparison returns @(Eq Nothing)@.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
unionMaybeH :: (e -> e -> COrdering (Maybe e)) -> AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
unionMaybeH c = u where
 -- u :: AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
 u  E           _   t1          h1 = UBT2(t1,h1)
 u  t0          h0  E           _  = UBT2(t0,h0)
 u (N l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (N l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (N l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u (Z l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (Z l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (Z l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u (P l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (P l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (P l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u_ l0 hl0 e0 r0 hr0 l1 hl1 e1 r1 hr1 =
  case c e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt   ->                                   case forkR r0 hr0 e1 of
          UBT5(rl0,hrl0,mbe1_,rr0,hrr0)  -> case forkL e0 l1 hl1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
           UBT5(ll1,hll1,mbe0_,lr1,hlr1) ->                         -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
            -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                            case u  l0  hl0 ll1 hll1 of
            UBT2(l,hl)                   -> case u rl0 hrl0 lr1 hlr1 of
             UBT2(m,hm)                  -> case u rr0 hrr0  r1  hr1 of
              UBT2(r,hr)                 -> case (case mbe1_ of
                                                  Just e1_ -> spliceH m hm e1_ r hr
                                                  Nothing  -> joinH   m hm     r hr
                                                 ) of
               UBT2(t,ht)                -> case mbe0_ of
                                            Just e0_ -> spliceH l hl e0_ t ht
                                            Nothing  -> joinH   l hl     t ht
  -- e0 = e1
  Eq mbe ->                case u l0 hl0 l1 hl1 of
            UBT2(l,hl)  -> case u r0 hr0 r1 hr1 of
             UBT2(r,hr) -> case mbe of
                           Just e  -> spliceH l hl e r hr
                           Nothing -> joinH   l hl   r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt   ->                                   case forkL e0 r1 hr1 of
          UBT5(rl1,hrl1,mbe0_,rr1,hrr1)  -> case forkR l0 hl0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
           UBT5(ll0,hll0,mbe1_,lr0,hlr0) ->                         -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                            case u ll0 hll0  l1  hl1 of
            UBT2(l,hl)                   -> case u lr0 hlr0 rl1 hrl1 of
             UBT2(m,hm)                  -> case u  r0  hr0 rr1 hrr1 of
              UBT2(r,hr)                 -> case (case mbe1_ of
                                                  Just e1_ -> spliceH l hl e1_ m hm
                                                  Nothing  -> joinH   l hl     m hm
                                                 ) of
               UBT2(t,ht)                -> case mbe0_ of
                                            Just e0_ -> spliceH t ht e0_ r hr
                                            Nothing  -> joinH   t ht     r hr
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1)
 -- forkL :: e -> AVL e -> UINT -> UBT5(AVL e,UINT,Maybe e,AVL e,UINT)
 forkL e0 t1 ht1 = forkL_ t1 ht1 where
  forkL_  E        _ = UBT5(E, L(0), Just e0, E, L(0))
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case c e0 e of
                        Lt       ->                              case forkL_ l hl of
                                    UBT5(l0,hl0,mbe0_,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                     UBT2(l1_,hl1_)           -> UBT5(l0,hl0,mbe0_,l1_,hl1_)
                        Eq mbe0_ -> UBT5(l,hl,mbe0_,r,hr)
                        Gt       ->                              case forkL_ r hr of
                                    UBT5(l0,hl0,mbe0_,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                     UBT2(l0_,hl0_)           -> UBT5(l0_,hl0_,mbe0_,l1,hl1)
 -- forkR :: AVL e -> UINT -> e -> UBT5(AVL e,UINT,Maybe e,AVL e,UINT)
 forkR t0 ht0 e1 = forkR_ t0 ht0 where
  forkR_  E        _ = UBT5(E, L(0), Just e1, E, L(0))
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case c e e1 of
                        Lt       ->                              case forkR_ r hr of
                                    UBT5(l0,hl0,mbe1_,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                     UBT2(l0_,hl0_)           -> UBT5(l0_,hl0_,mbe1_,l1,hl1)
                        Eq mbe1_ -> UBT5(l,hl,mbe1_,r,hr)
                        Gt       ->                              case forkR_ l hl of
                                    UBT5(l0,hl0,mbe1_,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                     UBT2(l1_,hl1_)           -> UBT5(l0,hl0,mbe1_,l1_,hl1_)
-----------------------------------------------------------------------
----------------------- unionMaybeH Ends Here -------------------------
-----------------------------------------------------------------------


-- | Uses the supplied comparison to evaluate the union of two /disjoint/ sets represented as
-- sorted AVL trees of known height. This function raises an error if the two sets intersect.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
-- (Faster than Hedge union from Data.Set at any rate).
disjointUnionH :: (e -> e -> Ordering) -> AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
disjointUnionH c = u where
 -- u :: AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
 u  E           _   t1          h1 = UBT2(t1,h1)
 u  t0          h0  E           _  = UBT2(t0,h0)
 u (N l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (N l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (N l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u (Z l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (Z l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (Z l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u (P l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (P l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (P l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u_ l0 hl0 e0 r0 hr0 l1 hl1 e1 r1 hr1 =
  case c e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  LT ->                             case fork e1 r0 hr0 of
        UBT4(rl0,hrl0,rr0,hrr0)  -> case fork e0 l1 hl1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
         UBT4(ll1,hll1,lr1,hlr1) ->                        -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
          -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                    case u  l0  hl0 ll1 hll1 of
          UBT2(l,hl)             -> case u rl0 hrl0 lr1 hlr1 of
           UBT2(m,hm)            -> case u rr0 hrr0  r1  hr1 of
            UBT2(r,hr)           -> case spliceH m hm e1 r hr of
             UBT2(t,ht)          -> spliceH l hl e0 t ht
  -- e0 = e1
  EQ -> error "disjointUnionH: Trees intersect" `seq` UBT2(E,L(0))
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  GT ->                             case fork e0 r1 hr1 of
        UBT4(rl1,hrl1,rr1,hrr1)  -> case fork e1 l0 hl0 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
         UBT4(ll0,hll0,lr0,hlr0) ->                        -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
          -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                    case u ll0 hll0  l1  hl1 of
          UBT2(l,hl)             -> case u lr0 hlr0 rl1 hrl1 of
           UBT2(m,hm)            -> case u  r0  hr0 rr1 hrr1 of
            UBT2(r,hr)           -> case spliceH l hl e1 m hm of
             UBT2(t,ht)          -> spliceH t ht e0 r hr
 -- fork :: e -> AVL e -> UINT -> UBT4(AVL e,UINT,AVL e,UINT)
 fork e0 t1 ht1 = fork_ t1 ht1 where
  fork_  E        _ = UBT4(E, L(0), E, L(0))
  fork_ (N l e r) h = fork__ l DECINT2(h) e r DECINT1(h)
  fork_ (Z l e r) h = fork__ l DECINT1(h) e r DECINT1(h)
  fork_ (P l e r) h = fork__ l DECINT1(h) e r DECINT2(h)
  fork__ l hl e r hr = case c e0 e of
                        LT ->                        case fork_ l hl of
                              UBT4(l0,hl0,l1,hl1) -> case spliceH l1 hl1 e r hr of
                               UBT2(l1_,hl1_)     -> UBT4(l0,hl0,l1_,hl1_)
                        EQ -> error "disjointUnionH: Trees intersect" `seq` UBT4(E, L(0), E, L(0))
                        GT ->                        case fork_ r hr of
                              UBT4(l0,hl0,l1,hl1) -> case spliceH l hl e l0 hl0 of
                               UBT2(l0_,hl0_)     -> UBT4(l0_,hl0_,l1,hl1)
-----------------------------------------------------------------------
---------------------- disjointUnionH Ends Here -----------------------
-----------------------------------------------------------------------

-- | Uses the supplied combining comparison to evaluate the intersection of two sets represented as
-- sorted AVL trees. This function requires no height information at all for
-- the two tree inputs. The absolute height of the resulting tree is returned also.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
intersectionH :: (a -> b -> COrdering c) -> AVL a -> AVL b -> UBT2(AVL c,UINT)
intersectionH cmp = i where
 -- i :: AVL a -> AVL b -> UBT2(AVL c,UINT)
 i  E            _           = UBT2(E,L(0))
 i  _            E           = UBT2(E,L(0))
 i (N l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (N l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (N l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i_ l0 e0 r0 l1 e1 r1 =
  case cmp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt   ->                            case forkR r0 e1 of
          UBT5(rl0,_,mbc1,rr0,_)  -> case forkL e0 l1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
           UBT5(ll1,_,mbc0,lr1,_) ->                     -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
            -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                     case i rr0  r1 of
                    UBT2(r,hr)    -> case i rl0 lr1 of
                     UBT2(m,hm)   -> case i  l0 ll1 of
                      UBT2(l,hl)  -> case (case mbc1 of
                                           Just c1 -> spliceH m hm c1 r hr
                                           Nothing -> joinH   m hm    r hr
                                          ) of
                       UBT2(t,ht) -> case mbc0 of
                                     Just c0 -> spliceH l hl c0 t ht
                                     Nothing -> joinH   l hl    t ht
  -- e0 = e1
  Eq c ->                case i l0 l1 of
          UBT2(l,hl)  -> case i r0 r1 of
           UBT2(r,hr) -> spliceH l hl c r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt   ->                            case forkL e0 r1 of
          UBT5(rl1,_,mbc0,rr1,_)  -> case forkR l0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
           UBT5(ll0,_,mbc1,lr0,_) ->                     -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0 + rl1) < e0 < (r0 + rr1)
                                     case i  r0 rr1 of
                    UBT2(r,hr)    -> case i lr0 rl1 of
                     UBT2(m,hm)   -> case i ll0  l1 of
                      UBT2(l,hl)  -> case (case mbc0 of
                                           Just c0 -> spliceH m hm c0 r hr
                                           Nothing -> joinH   m hm    r hr
                                          ) of
                       UBT2(t,ht) -> case mbc1 of
                                     Just c1 -> spliceH l hl c1 t ht
                                     Nothing -> joinH   l hl    t ht
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1)
 -- forkL :: a -> AVL b -> UBT5(AVL b,UINT,Maybe c,AVL b,UINT)
 forkL e0 t1 = forkL_ t1 L(0) where
  forkL_  E        h = UBT5(E,h,Nothing,E,h) -- Relative heights!!
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case cmp e0 e of
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
  forkR__ l hl e r hr = case cmp e e1 of
                        Lt    ->                             case forkR_ r hr of
                                 UBT5(l0,hl0,mbc1,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                  UBT2(l0_,hl0_)          -> UBT5(l0_,hl0_,mbc1,l1,hl1)
                        Eq c1 -> UBT5(l,hl,Just c1,r,hr)
                        Gt    ->                             case forkR_ l hl of
                                 UBT5(l0,hl0,mbc1,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                  UBT2(l1_,hl1_)          -> UBT5(l0,hl0,mbc1,l1_,hl1_)
-----------------------------------------------------------------------
---------------------- intersectionH Ends Here ------------------------
-----------------------------------------------------------------------

-- | Similar to _intersectionH_, but the resulting tree does not include elements in cases where
-- the supplied combining comparison returns @(Eq Nothing)@.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
intersectionMaybeH :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> UBT2(AVL c,UINT)
intersectionMaybeH comp = i where
 -- i :: AVL a -> AVL b -> UBT2(AVL c,UINT)
 i  E            _           = UBT2(E,L(0))
 i  _            E           = UBT2(E,L(0))
 i (N l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (N l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (N l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (Z l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (N l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (Z l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i (P l0 e0 r0) (P l1 e1 r1) = i_ l0 e0 r0 l1 e1 r1
 i_ l0 e0 r0 l1 e1 r1 =
  case comp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt   ->                            case forkR r0 e1 of
          UBT5(rl0,_,mbc1,rr0,_)  -> case forkL e0 l1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
           UBT5(ll1,_,mbc0,lr1,_) ->                     -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
            -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                     case i rr0  r1 of
                    UBT2(r,hr)    -> case i rl0 lr1 of
                     UBT2(m,hm)   -> case i  l0 ll1 of
                      UBT2(l,hl)  -> case (case mbc1 of
                                           Just c1 -> spliceH m hm c1 r hr
                                           Nothing -> joinH   m hm    r hr
                                          ) of
                       UBT2(t,ht) -> case mbc0 of
                                     Just c0 -> spliceH l hl c0 t ht
                                     Nothing -> joinH   l hl    t ht
  -- e0 = e1
  Eq mbc ->                case i l0 l1 of
            UBT2(l,hl)  -> case i r0 r1 of
             UBT2(r,hr) -> case mbc of
                           Just c  -> spliceH l hl c r hr
                           Nothing -> joinH   l hl   r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt   ->                            case forkL e0 r1 of
          UBT5(rl1,_,mbc0,rr1,_)  -> case forkR l0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
           UBT5(ll0,_,mbc1,lr0,_) ->                     -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0 + rl1) < e0 < (r0 + rr1)
                                     case i  r0 rr1 of
                    UBT2(r,hr)    -> case i lr0 rl1 of
                     UBT2(m,hm)   -> case i ll0  l1 of
                      UBT2(l,hl)  -> case (case mbc0 of
                                           Just c0 -> spliceH m hm c0 r hr
                                           Nothing -> joinH   m hm    r hr
                                          ) of
                       UBT2(t,ht) -> case mbc1 of
                                     Just c1 -> spliceH l hl c1 t ht
                                     Nothing -> joinH   l hl    t ht
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1)
 -- forkL :: a -> AVL b -> UBT5(AVL b,UINT,Maybe c,AVL b,UINT)
 forkL e0 t1 = forkL_ t1 L(0) where
  forkL_  E        h = UBT5(E,h,Nothing,E,h) -- Relative heights!!
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case comp e0 e of
                        Lt       ->                             case forkL_ l hl of
                                    UBT5(l0,hl0,mbc0,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                     UBT2(l1_,hl1_)          -> UBT5(l0,hl0,mbc0,l1_,hl1_)
                        Eq mbc0_ -> UBT5(l,hl,mbc0_,r,hr)
                        Gt       ->                             case forkL_ r hr of
                                    UBT5(l0,hl0,mbc0,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                     UBT2(l0_,hl0_)          -> UBT5(l0_,hl0_,mbc0,l1,hl1)
 -- forkR :: AVL a -> b -> UBT5(AVL a,UINT,Maybe c,AVL a,UINT)
 forkR t0 e1 = forkR_ t0 L(0) where
  forkR_  E        h = UBT5(E,h,Nothing,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case comp e e1 of
                        Lt       ->                             case forkR_ r hr of
                                    UBT5(l0,hl0,mbc1,l1,hl1) -> case spliceH l hl e l0 hl0 of
                                     UBT2(l0_,hl0_)          -> UBT5(l0_,hl0_,mbc1,l1,hl1)
                        Eq mbc1_ -> UBT5(l,hl,mbc1_,r,hr)
                        Gt       ->                             case forkR_ l hl of
                                    UBT5(l0,hl0,mbc1,l1,hl1) -> case spliceH l1 hl1 e r hr of
                                     UBT2(l1_,hl1_)          -> UBT5(l0,hl0,mbc1,l1_,hl1_)
-----------------------------------------------------------------------
-------------------- intersectionMaybeH Ends Here ---------------------
-----------------------------------------------------------------------

-- | Uses the supplied comparison to evaluate the difference between two sets represented as
-- sorted AVL trees.
--
-- N.B. This function works with relative heights for the first tree and needs no height
-- information for the second tree, so it_s OK to initialise the height of the first to zero,
-- rather than calculating the absolute height. However, if you do this the height of the resulting
-- tree will be incorrect also (it will have the same fixed offset as the first tree).
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
differenceH :: (a -> b -> Ordering) -> AVL a -> UINT -> AVL b -> UBT2(AVL a,UINT)
differenceH comp = d where
 -- d :: AVL a -> UINT -> AVL b -> UBT2(AVL a,UINT)
 d  E           h0  _           = UBT2(E ,h0) -- Relative heights!!
 d  t0          h0  E           = UBT2(t0,h0)
 d (N l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (N l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (N l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (Z l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (Z l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (Z l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (P l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 e1 r1
 d (P l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 e1 r1
 d (P l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 e1 r1
 d_ l0 hl0 e0 r0 hr0 l1 e1 r1 =
  case comp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  LT ->                                 case forkR r0 hr0 e1 of
        UBT4(rl0,hrl0,    rr0,hrr0)  -> case forkL e0 l1     of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
         UBT5(ll1,_   ,be0,lr1,_   ) ->                         -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
          -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                           case d rr0 hrr0  r1  of  -- right
          UBT2(r,hr)    -> case d rl0 hrl0 lr1  of  -- middle
           UBT2(m,hm)   -> case d  l0  hl0 ll1  of  -- left
            UBT2(l,hl)  -> case joinH m hm r hr of  -- join middle right
             UBT2(y,hy) -> if be0
                           then spliceH l hl e0 y hy
                           else joinH   l hl    y hy
  -- e0 = e1
  EQ ->                case d r0 hr0 r1 of -- right
        UBT2(r,hr)  -> case d l0 hl0 l1 of -- left
         UBT2(l,hl) -> joinH l hl r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  GT ->                                 case forkL e0 r1     of
        UBT5(rl1,_   ,be0,rr1,_   )  -> case forkR l0 hl0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
         UBT4(ll0,hll0,    lr0,hlr0) ->                         -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0 + rl1) < e0 < (r0 + rr1)
                           case d  r0  hr0 rr1  of  -- right
          UBT2(r,hr)    -> case d lr0 hlr0 rl1  of  -- middle
           UBT2(m,hm)   -> case d ll0 hll0  l1  of  -- left
            UBT2(l,hl)  -> case joinH l hl m hm of  -- join left middle
             UBT2(x,hx) -> if be0
                           then spliceH x hx e0 r hr
                           else joinH   x hx    r hr
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1), and for other algorithmic reasons in this case.
 -- N.B. forkL returns True if t1 does not contain e0 (I.E. If e0 is an element of the result).
 -- forkL :: a -> AVL b -> UBT5(AVL b, UINT, Bool, AVL b, UINT)
 forkL e0 t1 = forkL_ t1 L(0) where
  forkL_  E        h = UBT5(E,h,True,E,h) -- Relative heights!!
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case comp e0 e of
                        LT ->                            case forkL_ l hl           of
                              UBT5(x0,hx0,be0,x1,hx1) -> case spliceH x1 hx1 e r hr of
                               UBT2(x1_,hx1_)         -> UBT5(x0,hx0,be0,x1_,hx1_)
                        EQ -> UBT5(l,hl,False,r,hr)
                        GT ->                            case forkL_ r hr           of
                              UBT5(x0,hx0,be0,x1,hx1) -> case spliceH l hl e x0 hx0 of
                               UBT2(x0_,hx0_)         -> UBT5(x0_,hx0_,be0,x1,hx1)
 -- N.B. forkR t0, according to e1. Neither of the resulting forks will contain an element
 -- which is "equal" to e1.
 -- forkR :: AVL a -> UINT -> b -> UBT4(AVL a, UINT, AVL a, UINT)
 forkR t0 ht0 e1 = forkR_ t0 ht0 where
  forkR_  E        h = UBT4(E,h,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case comp e e1 of
                        LT ->                        case forkR_ r hr           of
                              UBT4(x0,hx0,x1,hx1) -> case spliceH l hl e x0 hx0 of
                               UBT2(x0_,hx0_)     -> UBT4(x0_,hx0_,x1,hx1)
                        EQ -> UBT4(l,hl,r,hr)  -- e1 is dropped.
                        GT ->                        case forkR_ l hl           of
                              UBT4(x0,hx0,x1,hx1) -> case spliceH x1 hx1 e r hr of
                               UBT2(x1_,hx1_)     -> UBT4(x0,hx0,x1_,hx1_)
-----------------------------------------------------------------------
----------------------- differenceH Ends Here -------------------------
-----------------------------------------------------------------------

-- | Similar to _differenceH_, but the resulting tree also includes those elements a\_ for which the
-- combining comparison returns @Eq (Just a\_)@.
--
-- N.B. This function works with relative heights for the first tree and needs no height
-- information for the second tree, so it_s OK to initialise the height of the first to zero,
-- rather than calculating the absolute height. However, if you do this the height of the resulting
-- tree will be incorrect also (it will have the same fixed offset as the first tree).
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
differenceMaybeH :: (a -> b -> COrdering (Maybe a)) -> AVL a -> UINT -> AVL b -> UBT2(AVL a,UINT)
differenceMaybeH comp = d where
 -- d :: AVL a -> UINT -> AVL b -> UBT2(AVL a,UINT)
 d  E           h0  _           = UBT2(E ,h0) -- Relative heights!!
 d  t0          h0  E           = UBT2(t0,h0)
 d (N l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (N l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (N l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (Z l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (Z l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (Z l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 e1 r1
 d (P l0 e0 r0) h0 (N l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 e1 r1
 d (P l0 e0 r0) h0 (Z l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 e1 r1
 d (P l0 e0 r0) h0 (P l1 e1 r1) = d_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 e1 r1
 d_ l0 hl0 e0 r0 hr0 l1 e1 r1 =
  case comp e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  Lt ->                                  case forkR r0 hr0 e1 of
        UBT5( rl0,hrl0,mbe1,rr0,hrr0) -> case forkL e0 l1     of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
         UBT5(ll1,_   ,mbe0,lr1,_   ) ->                         -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
          -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                           case d rr0 hrr0  r1  of  -- right
          UBT2(r,hr)    -> case d rl0 hrl0 lr1  of  -- middle
           UBT2(m,hm)   -> case d  l0  hl0 ll1  of  -- left
            UBT2(l,hl)  -> case (case mbe1 of
                                 Just e1_ -> spliceH m hm e1_ r hr      -- splice middle right with e1_
                                 Nothing  -> joinH   m hm     r hr) of  -- join   middle right
             UBT2(y,hy) -> case mbe0 of
                           Just e0_ -> spliceH l hl e0_ y hy
                           Nothing  -> joinH   l hl    y hy
  -- e0 = e1
  Eq mbe0 ->           case d r0 hr0 r1 of -- right
        UBT2(r,hr)  -> case d l0 hl0 l1 of -- left
         UBT2(l,hl) -> case mbe0 of
                       Just e0_ -> spliceH l hl e0_ r hr -- retain updated e0
                       Nothing  -> joinH   l hl     r hr -- discard original e0
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  Gt ->                                  case forkL e0 r1     of
        UBT5( rl1,_   ,mbe0,rr1,_   ) -> case forkR l0 hl0 e1 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
         UBT5(ll0,hll0,mbe1,lr0,hlr0) ->                         -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
            -- (ll0 + l1) < e1 < (lr0 + rl1) < e0 < (r0 + rr1)
                           case d  r0  hr0 rr1  of  -- right
          UBT2(r,hr)    -> case d lr0 hlr0 rl1  of  -- middle
           UBT2(m,hm)   -> case d ll0 hll0  l1  of  -- left
            UBT2(l,hl)  -> case (case mbe1 of
                                 Just e1_ -> spliceH l hl e1_ m hm      -- splice left middle with e1_
                                 Nothing  -> joinH   l hl     m hm) of  -- join left middle
             UBT2(x,hx) -> case mbe0 of
                           Just e0_ -> spliceH x hx e0_ r hr
                           Nothing  -> joinH   x hx     r hr
 -- We need 2 different versions of fork (L & R) to ensure that comparison arguments are used in
 -- the right order (c e0 e1), and for other algorithmic reasons in this case.
 -- N.B. forkL returns (Just e0) if t1 does not contain e0 (I.E. If original e0 is an element of the result).
 -- forkL :: a -> AVL b -> UBT5(AVL b, UINT, Maybe a, AVL b, UINT)
 forkL e0 t1 = forkL_ t1 L(0) where
  forkL_  E        h = UBT5(E,h,Just e0,E,h) -- Relative heights!!
  forkL_ (N l e r) h = forkL__ l DECINT2(h) e r DECINT1(h)
  forkL_ (Z l e r) h = forkL__ l DECINT1(h) e r DECINT1(h)
  forkL_ (P l e r) h = forkL__ l DECINT1(h) e r DECINT2(h)
  forkL__ l hl e r hr = case comp e0 e of
                        Lt      ->                             case forkL_ l hl           of
                                   UBT5(x0,hx0,mbe0,x1,hx1) -> case spliceH x1 hx1 e r hr of
                                    UBT2(x1_,hx1_)          -> UBT5(x0,hx0,mbe0,x1_,hx1_)
                        Eq mbe0 -> UBT5(l,hl,mbe0,r,hr)
                        Gt      ->                             case forkL_ r hr           of
                                   UBT5(x0,hx0,mbe0,x1,hx1) -> case spliceH l hl e x0 hx0 of
                                    UBT2(x0_,hx0_)          -> UBT5(x0_,hx0_,mbe0,x1,hx1)
 -- N.B. forkR t0, according to e1. Returns Nothing if t0 does not contain e1.
 -- forkR :: AVL a -> UINT -> b -> UBT5(AVL a, UINT, Maybe a, AVL a, UINT)
 forkR t0 ht0 e1 = forkR_ t0 ht0 where
  forkR_  E        h = UBT5(E,h,Nothing,E,h) -- Relative heights!!
  forkR_ (N l e r) h = forkR__ l DECINT2(h) e r DECINT1(h)
  forkR_ (Z l e r) h = forkR__ l DECINT1(h) e r DECINT1(h)
  forkR_ (P l e r) h = forkR__ l DECINT1(h) e r DECINT2(h)
  forkR__ l hl e r hr = case comp e e1 of
                        Lt      ->                             case forkR_ r hr           of
                                   UBT5(x0,hx0,mbe1,x1,hx1) -> case spliceH l hl e x0 hx0 of
                                    UBT2(x0_,hx0_)          -> UBT5(x0_,hx0_,mbe1,x1,hx1)
                        Eq mbe1 -> UBT5(l,hl,mbe1,r,hr)
                        Gt      ->                             case forkR_ l hl           of
                                   UBT5(x0,hx0,mbe1,x1,hx1) -> case spliceH x1 hx1 e r hr of
                                    UBT2(x1_,hx1_)          -> UBT5(x0,hx0,mbe1,x1_,hx1_)
-----------------------------------------------------------------------
--------------------- differenceMaybeH Ends Here ----------------------
-----------------------------------------------------------------------

-- | The symmetric difference is the set of elements which occur in one set or the other but /not both/.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
symDifferenceH :: (e -> e -> Ordering) -> AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
symDifferenceH c = u where
 -- u :: AVL e -> UINT -> AVL e -> UINT -> UBT2(AVL e,UINT)
 u  E           _   t1          h1 = UBT2(t1,h1)
 u  t0          h0  E           _  = UBT2(t0,h0)
 u (N l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (N l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (N l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT2(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u (Z l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (Z l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (Z l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT1(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u (P l0 e0 r0) h0 (N l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT2(h1) e1 r1 DECINT1(h1)
 u (P l0 e0 r0) h0 (Z l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT1(h1) e1 r1 DECINT1(h1)
 u (P l0 e0 r0) h0 (P l1 e1 r1) h1 = u_ l0 DECINT1(h0) e0 r0 DECINT2(h0) l1 DECINT1(h1) e1 r1 DECINT2(h1)
 u_ l0 hl0 e0 r0 hr0 l1 hl1 e1 r1 hr1 =
  case c e0 e1 of
  -- e0 < e1, so (l0 < e0 < e1) & (e0 < e1 < r1)
  LT ->                                 case fork e1 r0 hr0 of
        UBT5(rl0,hrl0,be1,rr0,hrr0)  -> case fork e0 l1 hl1 of -- (e0  < rl0 < e1) & (e0 < e1  < rr0)
         UBT5(ll1,hll1,be0,lr1,hlr1) ->                        -- (ll1 < e0  < e1) & (e0 < lr1 < e1)
          -- (l0 + ll1) < e0 < (rl0 + lr1) < e1 < (rr0 + r1)
                                        case u  l0  hl0 ll1 hll1 of
          UBT2(l,hl)                 -> case u rl0 hrl0 lr1 hlr1 of
           UBT2(m,hm)                -> case u rr0 hrr0  r1  hr1 of
            UBT2(r,hr)               -> case (if be1 then spliceH m hm e1 r hr
                                                     else joinH   m hm    r hr
                                             ) of
             UBT2(t,ht)              -> if be0 then spliceH l hl e0 t ht
                                               else joinH   l hl    t ht
  -- e0 = e1
  EQ ->                case u l0 hl0 l1 hl1 of
        UBT2(l,hl)  -> case u r0 hr0 r1 hr1 of
         UBT2(r,hr) -> joinH l hl r hr
  -- e1 < e0, so (l1 < e1 < e0) & (e1 < e0 < r0)
  GT ->                                 case fork e0 r1 hr1 of
        UBT5(rl1,hrl1,be0,rr1,hrr1)  -> case fork e1 l0 hl0 of -- (e1  < rl1 < e0) & (e1 < e0  < rr1)
         UBT5(ll0,hll0,be1,lr0,hlr0) ->                        -- (ll0 < e1  < e0) & (e1 < lr0 < e0)
          -- (ll0 + l1) < e1 < (lr0  + rl1) < e0 < (r0 + rr1)
                                        case u ll0 hll0  l1  hl1 of
          UBT2(l,hl)                 -> case u lr0 hlr0 rl1 hrl1 of
           UBT2(m,hm)                -> case u  r0  hr0 rr1 hrr1 of
            UBT2(r,hr)               -> case (if be1 then spliceH l hl e1 m hm
                                                     else joinH   l hl    m hm
                                             ) of
             UBT2(t,ht)              -> if be0 then spliceH t ht e0 r hr
                                               else joinH   t ht    r hr
 -- fork :: e -> AVL e -> UINT -> UBT5(AVL e,UINT,Bool,AVL e,UINT)
 fork e0 t1 ht1 = fork_ t1 ht1 where
  fork_  E        _ = UBT5(E, L(0), True, E, L(0))
  fork_ (N l e r) h = fork__ l DECINT2(h) e r DECINT1(h)
  fork_ (Z l e r) h = fork__ l DECINT1(h) e r DECINT1(h)
  fork_ (P l e r) h = fork__ l DECINT1(h) e r DECINT2(h)
  fork__ l hl e r hr = case c e0 e of
                       LT ->                            case fork_ l hl of
                             UBT5(l0,hl0,be0,l1,hl1) -> case spliceH l1 hl1 e r hr of
                              UBT2(l1_,hl1_)         -> UBT5(l0,hl0,be0,l1_,hl1_)
                       EQ -> UBT5(l,hl,False,r,hr)
                       GT ->                            case fork_ r hr of
                             UBT5(l0,hl0,be0,l1,hl1) -> case spliceH l hl e l0 hl0 of
                              UBT2(l0_,hl0_)         -> UBT5(l0_,hl0_,be0,l1,hl1)
-----------------------------------------------------------------------
----------------------- symDifferenceH Ends Here ----------------------
-----------------------------------------------------------------------


-- | Given two Sets @A@ and @B@ represented as sorted AVL trees, this function extracts
-- the \'Venn diagram\' components @A-B@, @A.B@ and @B-A@.
-- The two difference components are sorted AVL trees.
-- The intersection component is prepended to the input List in ascending sorted in ascending order.
-- The number of elements prepended is added to the corresponding Int argument (which may or may
-- not be the List length).
-- See also 'vennMaybeH'.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
vennH :: (a -> b -> COrdering c) -> [c] -> UINT -> AVL a -> UINT -> AVL b -> UINT -> UBT6(AVL a,UINT,[c],UINT,AVL b,UINT)
vennH cmp = v where
 -- v :: [c] -> UINT -> AVL a -> UINT -> AVL b -> UINT -> UBT6(AVL a,UINT,[c],UINT,AVL b,UINT)
 v cs cl  E          ha  tb         hb = UBT6(E ,ha,cs,cl,tb,hb)
 v cs cl  ta         ha  E          hb = UBT6(ta,ha,cs,cl,E ,hb)
 v cs cl (N la a ra) ha (N lb b rb) hb = v_ cs cl la DECINT2(ha) a ra DECINT1(ha) lb DECINT2(hb) b rb DECINT1(hb)
 v cs cl (N la a ra) ha (Z lb b rb) hb = v_ cs cl la DECINT2(ha) a ra DECINT1(ha) lb DECINT1(hb) b rb DECINT1(hb)
 v cs cl (N la a ra) ha (P lb b rb) hb = v_ cs cl la DECINT2(ha) a ra DECINT1(ha) lb DECINT1(hb) b rb DECINT2(hb)
 v cs cl (Z la a ra) ha (N lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT1(ha) lb DECINT2(hb) b rb DECINT1(hb)
 v cs cl (Z la a ra) ha (Z lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT1(ha) lb DECINT1(hb) b rb DECINT1(hb)
 v cs cl (Z la a ra) ha (P lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT1(ha) lb DECINT1(hb) b rb DECINT2(hb)
 v cs cl (P la a ra) ha (N lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT2(ha) lb DECINT2(hb) b rb DECINT1(hb)
 v cs cl (P la a ra) ha (Z lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT2(ha) lb DECINT1(hb) b rb DECINT1(hb)
 v cs cl (P la a ra) ha (P lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT2(ha) lb DECINT1(hb) b rb DECINT2(hb)
 v_ cs cl la hla a ra hra lb hlb b rb hrb =
  case cmp a b of
  -- a < b, so (la < a < b) & (a < b < rb)
  Lt   ->                                  case forka cmp a lb hlb of
   UBT5(llb,hllb,mbca,rlb,hrlb)         -> case forkb cmp b ra hra of
    UBT5(lra,hlra,mbcb,rra,hrra)        ->
     -- (la + llb) < a < (lra + rlb) < b < (rra + rb)
                                           case v cs cl rra hrra rb hrb of
     UBT6(rab,hrab,cs0,cl0,rba,hrba)    -> case (case mbcb of
                                                 Nothing -> case v     cs0          cl0  lra hlra rlb hrlb of
                                                  UBT6(mab,hmab,cs1,cl1,mba,hmba) -> case spliceH mba hmba b rba hrba of
                                                   UBT2(mrba,hmrba)               -> UBT6(mab,hmab,cs1,cl1,mrba,hmrba)
                                                 Just cb -> case v (cb:cs0) INCINT1(cl0) lra hlra rlb hrlb of
                                                  UBT6(mab,hmab,cs1,cl1,mba,hmba) -> case joinH   mba hmba   rba hrba of
                                                   UBT2(mrba,hmrba)               -> UBT6(mab,hmab,cs1,cl1,mrba,hmrba)
                                                ) of
      UBT6(mab,hmab,cs1,cl1,mrba,hmrba) -> case joinH mab hmab rab hrab of
       UBT2(mrab,hmrab)                 -> case (case mbca of
                                                 Nothing -> case v     cs1          cl1  la hla llb hllb of
                                                  UBT6(lab,hlab,cs2,cl2,lba,hlba) -> case spliceH lab hlab a mrab hmrab of
                                                   UBT2(ab,hab)                   -> UBT6(ab,hab,cs2,cl2,lba,hlba)
                                                 Just ca -> case v (ca:cs1) INCINT1(cl1) la hla llb hllb of
                                                  UBT6(lab,hlab,cs2,cl2,lba,hlba) -> case joinH   lab hlab   mrab hmrab of
                                                   UBT2(ab,hab)                   -> UBT6(ab,hab,cs2,cl2,lba,hlba)
                                                ) of
        UBT6(ab,hab,cs2,cl2,lba,hlba)  -> case joinH lba hlba mrba hmrba of
         UBT2(ba,hba)                  -> UBT6(ab,hab,cs2,cl2,ba,hba)
  -- a = b
  Eq c ->                                     case v    cs           cl   ra hra rb hrb of
   UBT6(rab,hrab,cs0,cl0,rba,hrba)  -> case v (c:cs0) INCINT1(cl0) la hla lb hlb of
    UBT6(lab,hlab,cs1,cl1,lba,hlba) -> case joinH lab hlab rab hrab of
     UBT2(ab,hab)                   -> case joinH lba hlba rba hrba of
      UBT2(ba,hba)                  -> UBT6(ab,hab,cs1,cl1,ba,hba)
  -- b < a, so (lb < b < a) & (b < a < ra)
  Gt   ->                                 case forka cmp a rb hrb of
   UBT5(lrb,hlrb,mbca,rrb,hrrb)        -> case forkb cmp b la hla of
    UBT5(lla,hlla,mbcb,rla,hrla)       ->
     -- (lla + lb) < b < (rla + lrb) < a < (ra + rrb)
                                          case v cs cl ra hra rrb hrrb of
     UBT6(rab,hrab,cs0,cl0,rba,hrba)   -> case (case mbca of
                                                Nothing -> case v     cs0          cl0  rla hrla lrb hlrb of
                                                 UBT6(mab,hmab,cs1,cl1,mba,hmba) -> case spliceH mab hmab a rab hrab of
                                                  UBT2(mrab,hmrab)               -> UBT6(mrab,hmrab,cs1,cl1,mba,hmba)
                                                Just ca -> case v (ca:cs0) INCINT1(cl0) rla hrla lrb hlrb of
                                                 UBT6(mab,hmab,cs1,cl1,mba,hmba) -> case joinH   mab hmab   rab hrab of
                                                  UBT2(mrab,hmrab)               -> UBT6(mrab,hmrab,cs1,cl1,mba,hmba)
                                               ) of
      UBT6(mrab,hmrab,cs1,cl1,mba,hmba) -> case joinH mba hmba rba hrba of
       UBT2(mrba,hmrba)                 -> case (case mbcb of
                                                 Nothing -> case v     cs1          cl1  lla hlla lb hlb of
                                                  UBT6(lab,hlab,cs2,cl2,lba,hlba) -> case spliceH lba hlba b mrba hmrba of
                                                   UBT2(ba,hba)                   -> UBT6(lab,hlab,cs2,cl2,ba,hba)
                                                 Just cb -> case v (cb:cs1) INCINT1(cl1) lla hlla lb hlb of
                                                  UBT6(lab,hlab,cs2,cl2,lba,hlba) -> case joinH   lba hlba   mrba hmrba of
                                                   UBT2(ba,hba)                   -> UBT6(lab,hlab,cs2,cl2,ba,hba)
                                                ) of
        UBT6(lab,hlab,cs2,cl2,ba,hba)   -> case joinH lab hlab mrab hmrab of
         UBT2(ab,hab)                   -> UBT6(ab,hab,cs2,cl2,ba,hba)
-----------------------------------------------------------------------
--------------------------- vennH Ends Here ---------------------------
-----------------------------------------------------------------------

-- | Similar to 'vennH', but intersection elements for which the combining comparison
-- returns @('Eq' 'Nothing')@ are deleted from the intersection list.
--
-- Complexity: Not sure, but I\'d appreciate it if someone could figure it out.
vennMaybeH :: (a -> b -> COrdering (Maybe c)) -> [c] -> UINT -> AVL a -> UINT -> AVL b -> UINT -> UBT6(AVL a,UINT,[c],UINT,AVL b,UINT)
vennMaybeH cmp = v where
 -- v :: [c] -> UINT -> AVL a -> UINT -> AVL b -> UINT -> UBT6(AVL a,UINT,[c],UINT,AVL b,UINT)
 v cs cl  E          ha  tb         hb = UBT6(E ,ha,cs,cl,tb,hb)
 v cs cl  ta         ha  E          hb = UBT6(ta,ha,cs,cl,E ,hb)
 v cs cl (N la a ra) ha (N lb b rb) hb = v_ cs cl la DECINT2(ha) a ra DECINT1(ha) lb DECINT2(hb) b rb DECINT1(hb)
 v cs cl (N la a ra) ha (Z lb b rb) hb = v_ cs cl la DECINT2(ha) a ra DECINT1(ha) lb DECINT1(hb) b rb DECINT1(hb)
 v cs cl (N la a ra) ha (P lb b rb) hb = v_ cs cl la DECINT2(ha) a ra DECINT1(ha) lb DECINT1(hb) b rb DECINT2(hb)
 v cs cl (Z la a ra) ha (N lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT1(ha) lb DECINT2(hb) b rb DECINT1(hb)
 v cs cl (Z la a ra) ha (Z lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT1(ha) lb DECINT1(hb) b rb DECINT1(hb)
 v cs cl (Z la a ra) ha (P lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT1(ha) lb DECINT1(hb) b rb DECINT2(hb)
 v cs cl (P la a ra) ha (N lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT2(ha) lb DECINT2(hb) b rb DECINT1(hb)
 v cs cl (P la a ra) ha (Z lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT2(ha) lb DECINT1(hb) b rb DECINT1(hb)
 v cs cl (P la a ra) ha (P lb b rb) hb = v_ cs cl la DECINT1(ha) a ra DECINT2(ha) lb DECINT1(hb) b rb DECINT2(hb)
 v_ cs cl la hla a ra hra lb hlb b rb hrb =
  case cmp a b of
  -- a < b, so (la < a < b) & (a < b < rb)
  Lt   ->                                  case forka cmp a lb hlb of
   UBT5(llb,hllb,mbmbca,rlb,hrlb)       -> case forkb cmp b ra hra of
    UBT5(lra,hlra,mbmbcb,rra,hrra)      ->
     -- (la + llb) < a < (lra + rlb) < b < (rra + rb)
                                           case v cs cl rra hrra rb hrb of
     UBT6(rab,hrab,cs0,cl0,rba,hrba)    -> case (case mbmbcb of
                                                 Nothing   -> case v     cs0          cl0  lra hlra rlb hrlb of
                                                  UBT6(mab,hmab,cs1,cl1,mba,hmba) -> case spliceH mba hmba b rba hrba of
                                                   UBT2(mrba,hmrba)               -> UBT6(mab,hmab,cs1,cl1,mrba,hmrba)
                                                 Just mbcb -> case (case mbcb of
                                                                    Nothing -> v     cs0          cl0  lra hlra rlb hrlb
                                                                    Just cb -> v (cb:cs0) INCINT1(cl0) lra hlra rlb hrlb
                                                                   ) of
                                                  UBT6(mab,hmab,cs1,cl1,mba,hmba) -> case joinH   mba hmba   rba hrba of
                                                   UBT2(mrba,hmrba)               -> UBT6(mab,hmab,cs1,cl1,mrba,hmrba)
                                                ) of
      UBT6(mab,hmab,cs1,cl1,mrba,hmrba) -> case joinH mab hmab rab hrab of
       UBT2(mrab,hmrab)                 -> case (case mbmbca of
                                                 Nothing   -> case v     cs1          cl1  la hla llb hllb of
                                                  UBT6(lab,hlab,cs2,cl2,lba,hlba) -> case spliceH lab hlab a mrab hmrab of
                                                   UBT2(ab,hab)                   -> UBT6(ab,hab,cs2,cl2,lba,hlba)
                                                 Just mbca -> case (case mbca of
                                                                    Nothing -> v     cs1          cl1  la hla llb hllb
                                                                    Just ca -> v (ca:cs1) INCINT1(cl1) la hla llb hllb
                                                                   ) of
                                                  UBT6(lab,hlab,cs2,cl2,lba,hlba) -> case joinH   lab hlab   mrab hmrab of
                                                   UBT2(ab,hab)                   -> UBT6(ab,hab,cs2,cl2,lba,hlba)
                                                ) of
        UBT6(ab,hab,cs2,cl2,lba,hlba)   -> case joinH lba hlba mrba hmrba of
         UBT2(ba,hba)                   -> UBT6(ab,hab,cs2,cl2,ba,hba)
  -- a = b
  Eq mbc ->                            case v    cs           cl   ra hra rb hrb of
   UBT6(rab,hrab,cs0,cl0,rba,hrba)  -> case (case mbc of
                                             Nothing -> v    cs0          cl0  la hla lb hlb
                                             Just c  -> v (c:cs0) INCINT1(cl0) la hla lb hlb
                                            ) of
    UBT6(lab,hlab,cs1,cl1,lba,hlba) -> case joinH lab hlab rab hrab of
     UBT2(ab,hab)                   -> case joinH lba hlba rba hrba of
      UBT2(ba,hba)                  -> UBT6(ab,hab,cs1,cl1,ba,hba)
  -- b < a, so (lb < b < a) & (b < a < ra)
  Gt   ->                                   case forka cmp a rb hrb of
   UBT5(lrb,hlrb,mbmbca,rrb,hrrb)        -> case forkb cmp b la hla of
    UBT5(lla,hlla,mbmbcb,rla,hrla)       ->
     -- (lla + lb) < b < (rla + lrb) < a < (ra + rrb)
                                            case v cs cl ra hra rrb hrrb of
     UBT6(rab,hrab,cs0,cl0,rba,hrba)     -> case (case mbmbca of
                                                  Nothing   -> case v     cs0          cl0  rla hrla lrb hlrb of
                                                   UBT6(mab,hmab,cs1,cl1,mba,hmba) -> case spliceH mab hmab a rab hrab of
                                                    UBT2(mrab,hmrab)               -> UBT6(mrab,hmrab,cs1,cl1,mba,hmba)
                                                  Just mbca -> case (case mbca of
                                                                     Nothing -> v     cs0          cl0  rla hrla lrb hlrb
                                                                     Just ca -> v (ca:cs0) INCINT1(cl0) rla hrla lrb hlrb
                                                                    ) of
                                                   UBT6(mab,hmab,cs1,cl1,mba,hmba) -> case joinH   mab hmab   rab hrab of
                                                    UBT2(mrab,hmrab)               -> UBT6(mrab,hmrab,cs1,cl1,mba,hmba)
                                                 ) of
      UBT6(mrab,hmrab,cs1,cl1,mba,hmba)  -> case joinH mba hmba rba hrba of
       UBT2(mrba,hmrba)                  -> case (case mbmbcb of
                                                  Nothing   -> case v     cs1          cl1  lla hlla lb hlb of
                                                   UBT6(lab,hlab,cs2,cl2,lba,hlba) -> case spliceH lba hlba b mrba hmrba of
                                                    UBT2(ba,hba)                   -> UBT6(lab,hlab,cs2,cl2,ba,hba)
                                                  Just mbcb -> case (case mbcb of
                                                                     Nothing -> v     cs1          cl1  lla hlla lb hlb
                                                                     Just cb -> v (cb:cs1) INCINT1(cl1) lla hlla lb hlb
                                                                    ) of
                                                   UBT6(lab,hlab,cs2,cl2,lba,hlba) -> case joinH   lba hlba   mrba hmrba of
                                                    UBT2(ba,hba)                   -> UBT6(lab,hlab,cs2,cl2,ba,hba)
                                                 ) of
        UBT6(lab,hlab,cs2,cl2,ba,hba)    -> case joinH lab hlab mrab hmrab of
         UBT2(ab,hab)                    -> UBT6(ab,hab,cs2,cl2,ba,hba)
-----------------------------------------------------------------------
------------------------ vennMaybeH Ends Here -------------------------
-----------------------------------------------------------------------

-- Common forks used by vennH,vennMaybeH
-- We need 2 different versions of fork to ensure that comparison arguments are used in
-- the right order (c a b)
forka :: (a -> b -> COrdering c) -> a -> AVL b -> UINT -> UBT5(AVL b,UINT,Maybe c,AVL b,UINT)
forka cmp a tb htb = f tb htb where
 f    E        _    = UBT5(E,L(0),Nothing,E,L(0))
 f n@(N _ b r) L(2) = case cmp a b of -- l must be E, r must be Z
                      Lt   -> UBT5(E,L(0),Nothing,n,L(2))
                      Eq c -> UBT5(E,L(0),Just c ,r,L(1))
                      Gt   -> case r of
                              Z _ br _ -> case cmp a br of -- l & r must be E
                                          Lt   -> UBT5(Z E b E,L(1),Nothing,r,L(1))
                                          Eq c -> UBT5(Z E b E,L(1),Just c ,E,L(0))
                                          Gt   -> UBT5(n      ,L(2),Nothing,E,L(0))
                              _        -> undefined `seq` UBT5(E,L(0),Nothing,E,L(0))
 f   (N l b r) h    = f_ l DECINT2(h) b r DECINT1(h)
 f z@(Z l b r) L(2) = case cmp a b of -- l & r must be Z
                      Lt   -> case l of
                              Z _ bl _ -> case cmp a bl of -- l & r must be E
                                          Lt   -> UBT5(E,L(0),Nothing,z      ,L(2))
                                          Eq c -> UBT5(E,L(0),Just c ,N E b r,L(2))
                                          Gt   -> UBT5(l,L(1),Nothing,N E b r,L(2))
                              _        -> undefined `seq` UBT5(E,L(0),Nothing,E,L(0))
                      Eq c -> UBT5(l,L(1),Just c,r,L(1))
                      Gt   -> case r of
                              Z _ br _ -> case cmp a br of -- l & r must be E
                                          Lt   -> UBT5(P l b E,L(2),Nothing,r,L(1))
                                          Eq c -> UBT5(P l b E,L(2),Just c ,E,L(0))
                                          Gt   -> UBT5(z      ,L(2),Nothing,E,L(0))
                              _        -> undefined `seq` UBT5(E,L(0),Nothing,E,L(0))
 f z@(Z _ b _) L(1) = case cmp a b of -- l & r must be E
                      Lt   -> UBT5(E,L(0),Nothing,z,L(1))
                      Eq c -> UBT5(E,L(0),Just c ,E,L(0))
                      Gt   -> UBT5(z,L(1),Nothing,E,L(0))
 f   (Z l b r) h    = f_ l DECINT1(h) b r DECINT1(h)
 f p@(P l b _) L(2) = case cmp a b of -- l must be Z, r must be E
                      Lt   -> case l of
                              Z _ bl _ -> case cmp a bl of -- l & r must be E
                                          Lt   -> UBT5(E,L(0),Nothing,p      ,L(2))
                                          Eq c -> UBT5(E,L(0),Just c ,Z E b E,L(1))
                                          Gt   -> UBT5(l,L(1),Nothing,Z E b E,L(1))
                              _        -> undefined `seq` UBT5(E,L(0),Nothing,E,L(0))
                      Eq c -> UBT5(l,L(1),Just c ,E,L(0))
                      Gt   -> UBT5(p,L(2),Nothing,E,L(0))
 f   (P l b r) h    = f_ l DECINT1(h) b r DECINT2(h)
 f_ l hl b r hr = case cmp a b of
                  Lt   ->                            case f l hl of
                          UBT5(ll,hll,mbc,lr,hlr) -> case spliceH lr hlr b r hr of
                           UBT2(r_,hr_)           -> UBT5(ll,hll,mbc,r_,hr_)
                  Eq c -> UBT5(l,hl,Just c,r,hr)
                  Gt   ->                            case f r hr of
                          UBT5(rl,hrl,mbc,rr,hrr) -> case spliceH l hl b rl hrl of
                           UBT2(l_,hl_)           -> UBT5(l_,hl_,mbc,rr,hrr)

-- This should be exactly the same as forka, but with the following swaps:
--  * a <-> b, except is compare!
--  * Lt <-> Gt (becasuse we didn't swap in compare)
forkb :: (a -> b -> COrdering c) -> b -> AVL a -> UINT -> UBT5(AVL a,UINT,Maybe c,AVL a,UINT)
forkb cmp b ta hta = f ta hta where
 f    E        _    = UBT5(E,L(0),Nothing,E,L(0))
 f n@(N _ a r) L(2) = case cmp a b of -- l must be E, r must be Z
                      Gt   -> UBT5(E,L(0),Nothing,n,L(2))
                      Eq c -> UBT5(E,L(0),Just c ,r,L(1))
                      Lt   -> case r of
                              Z _ ar _ -> case cmp ar b of -- l & r must be E
                                          Gt   -> UBT5(Z E a E,L(1),Nothing,r,L(1))
                                          Eq c -> UBT5(Z E a E,L(1),Just c ,E,L(0))
                                          Lt   -> UBT5(n      ,L(2),Nothing,E,L(0))
                              _        -> undefined `seq` UBT5(E,L(0),Nothing,E,L(0))
 f   (N l a r) h    = f_ l DECINT2(h) a r DECINT1(h)
 f z@(Z l a r) L(2) = case cmp a b of -- l & r must be Z
                      Gt   -> case l of
                              Z _ al _ -> case cmp al b of -- l & r must be E
                                          Gt   -> UBT5(E,L(0),Nothing,z      ,L(2))
                                          Eq c -> UBT5(E,L(0),Just c ,N E a r,L(2))
                                          Lt   -> UBT5(l,L(1),Nothing,N E a r,L(2))
                              _        -> undefined `seq` UBT5(E,L(0),Nothing,E,L(0))
                      Eq c -> UBT5(l,L(1),Just c,r,L(1))
                      Lt   -> case r of
                              Z _ ar _ -> case cmp ar b of -- l & r must be E
                                          Gt   -> UBT5(P l a E,L(2),Nothing,r,L(1))
                                          Eq c -> UBT5(P l a E,L(2),Just c ,E,L(0))
                                          Lt   -> UBT5(z      ,L(2),Nothing,E,L(0))
                              _        -> undefined `seq` UBT5(E,L(0),Nothing,E,L(0))
 f z@(Z _ a _) L(1) = case cmp a b of -- l & r must be E
                      Gt   -> UBT5(E,L(0),Nothing,z,L(1))
                      Eq c -> UBT5(E,L(0),Just c ,E,L(0))
                      Lt   -> UBT5(z,L(1),Nothing,E,L(0))
 f   (Z l a r) h    = f_ l DECINT1(h) a r DECINT1(h)
 f p@(P l a _) L(2) = case cmp a b of -- l must be Z, r must be E
                      Gt   -> case l of
                              Z _ al _ -> case cmp al b of -- l & r must be E
                                          Gt   -> UBT5(E,L(0),Nothing,p      ,L(2))
                                          Eq c -> UBT5(E,L(0),Just c ,Z E a E,L(1))
                                          Lt   -> UBT5(l,L(1),Nothing,Z E a E,L(1))
                              _        -> undefined `seq` UBT5(E,L(0),Nothing,E,L(0))
                      Eq c -> UBT5(l,L(1),Just c ,E,L(0))
                      Lt   -> UBT5(p,L(2),Nothing,E,L(0))
 f   (P l a r) h    = f_ l DECINT1(h) a r DECINT2(h)
 f_ l hl a r hr = case cmp a b of
                  Gt   ->                            case f l hl of
                          UBT5(ll,hll,mbc,lr,hlr) -> case spliceH lr hlr a r hr of
                           UBT2(r_,hr_)           -> UBT5(ll,hll,mbc,r_,hr_)
                  Eq c -> UBT5(l,hl,Just c,r,hr)
                  Lt   ->                            case f r hr of
                          UBT5(rl,hrl,mbc,rr,hrr) -> case spliceH l hl a rl hrl of
                           UBT2(l_,hl_)           -> UBT5(l_,hl_,mbc,rr,hrr)


