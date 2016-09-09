{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Internals.HPush
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- Functions for pushing elements into trees of known height.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Internals.HPush
        (pushHL,pushHR,pushHL_,pushHR_,
        ) where

import Data.Tree.AVL.Types(AVL(..))

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | A version of 'pushL' for an AVL tree of known height. Returns an AVL tree of known height.
-- It's OK if height is relative, with fixed offset. In this case the height of the result
-- will have the same fixed offset.
{-# INLINE pushHL #-}
pushHL :: e -> AVL e -> UINT -> UBT2(AVL e,UINT)
pushHL e t h = pushHL_ (Z E e E) t h

-- | A version of 'pushR' for an AVL tree of known height. Returns an AVL tree of known height.
-- It's OK if height is relative, with fixed offset. In this case the height of the result
-- will have the same fixed offset.
{-# INLINE pushHR #-}
pushHR :: AVL e -> UINT -> e -> UBT2(AVL e,UINT)
pushHR t h e = pushHR_ t h (Z E e E)

-- | Push a singleton tree (first arg) in the leftmost position of an AVL tree of known height,
-- returning an AVL tree of known height. It's OK if height is relative, with fixed offset.
-- In this case the height of the result will have the same fixed offset.
--
-- Complexity: O(log n)
pushHL_ :: AVL e -> AVL e -> UINT -> UBT2(AVL e,UINT)
pushHL_ t0 t h = case t of
                 E       -> UBT2(t0, INCINT1(h)) -- Relative Heights
                 N l e r -> let t_ = putNL l e r in t_ `seq` UBT2(t_,h)
                 P l e r -> let t_ = putPL l e r in t_ `seq` UBT2(t_,h)
                 Z l e r -> let t_ = putZL l e r
                            in case t_ of
                               Z _ _ _ -> UBT2(t_,         h )
                               P _ _ _ -> UBT2(t_, INCINT1(h))
                               _       -> error "pushHL_: Bug0" -- impossible
 where
 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNL, putZL, putPL                          --
 -----------------------------------------------------------------------

 -- (putNL l e r): Put in L subtree of (N l e r), BF=-1 (Never requires rebalancing) , (never returns P)
 putNL  E           e r = Z t0 e r                    -- L subtree empty, H:0->1, parent BF:-1-> 0
 putNL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (Z ll le lr) e r = let l' = putZL ll le lr     -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          Z _ _ _ -> N l' e r         -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                          P _ _ _ -> Z l' e r         -- L subtree BF:0->+1, H:h->h+1, parent BF:-1-> 0
                          _       -> error "pushHL_: Bug1" -- impossible

 -- (putZL l e r): Put in L subtree of (Z l e r), BF= 0  (Never requires rebalancing) , (never returns N)
 putZL  E           e r = P t0 e r                    -- L subtree        H:0->1, parent BF: 0->+1
 putZL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (Z ll le lr) e r = let l' = putZL ll le lr     -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          Z _ _ _ -> Z l' e r         -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          N _ _ _ -> error "pushHL_: Bug2" -- impossible
                          _       -> P l' e r         -- L subtree BF: 0->+1, H:h->h+1, parent BF: 0->+1

      -------- This case (PL) may need rebalancing if it goes to LEVEL 3 ---------

 -- (putPL l e r): Put in L subtree of (P l e r), BF=+1 , (never returns N)
 putPL  E           _ _ = error "pushHL_: Bug3"       -- impossible if BF=+1
 putPL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (Z ll le lr) e r = putPLL ll le lr e r         -- LL (never returns N)

 ----------------------------- LEVEL 3 ---------------------------------
 --                            putPLL                                 --
 -----------------------------------------------------------------------

 -- (putPLL ll le lr e r): Put in LL subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLL #-}
 putPLL  E le lr e r              = Z t0 le (Z lr e r)                  -- r and lr must also be E, special CASE LL!!
 putPLL (N lll lle llr) le lr e r = let ll' = putNL lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (P lll lle llr) le lr e r = let ll' = putPL lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (Z lll lle llr) le lr e r = let ll' = putZL lll lle llr         -- LL subtree BF= 0, so need to look for changes
                                    in case ll' of
                                    Z _ _ _ -> P (Z ll' le lr) e r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                    N _ _ _ -> error "pushHL_: Bug4" -- impossible
                                    _       -> Z ll' le (Z lr e r) -- LL subtree BF: 0->+1, H:h->h+1, parent BF:-1->-2, CASE LL !!
-----------------------------------------------------------------------
-------------------------- pushHL_ Ends Here --------------------------
-----------------------------------------------------------------------


-- | Push a singleton tree (third arg) in the rightmost position of an AVL tree of known height,
-- returning an AVL tree of known height. It's OK if height is relative, with fixed offset.
-- In this case the height of the result will have the same fixed offset.
--
-- Complexity: O(log n)
pushHR_ :: AVL e -> UINT -> AVL e -> UBT2(AVL e,UINT)
pushHR_ t h t0 = case t of
                 E         -> UBT2(t0, INCINT1(h)) -- Relative Heights
                 N l e r -> let t_ = putNR l e r in t_ `seq` UBT2(t_,h)
                 P l e r -> let t_ = putPR l e r in t_ `seq` UBT2(t_,h)
                 Z l e r -> let t_ = putZR l e r
                              in case t_ of
                                 Z _ _ _ -> UBT2(t_,         h )
                                 N _ _ _ -> UBT2(t_, INCINT1(h))
                                 _       -> error "pushHR_: Bug0" -- impossible
 where
 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNR, putZR, putPR                          --
 -----------------------------------------------------------------------

 -- (putZR l e r): Put in R subtree of (Z l e r), BF= 0 (Never requires rebalancing) , (never returns P)
 putZR l e E            = N l e t0                    -- R subtree        H:0->1, parent BF: 0->-1
 putZR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (Z rl re rr) = let r' = putZR rl re rr     -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          Z _ _ _ -> Z l e r'         -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          N _ _ _ -> N l e r'         -- R subtree BF: 0->-1, H:h->h+1, parent BF: 0->-1
                          _       -> error "pushHR_: Bug1" -- impossible

 -- (putPR l e r): Put in R subtree of (P l e r), BF=+1 (Never requires rebalancing) , (never returns N)
 putPR l e  E           = Z l e t0                    -- R subtree empty, H:0->1,     parent BF:+1-> 0
 putPR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (Z rl re rr) = let r' = putZR rl re rr     -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          Z _ _ _ -> P l e r'         -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                          N _ _ _ -> Z l e r'         -- R subtree BF:0->-1, H:h->h+1, parent BF:+1-> 0
                          _       -> error "pushHR_: Bug2" -- impossible

      -------- This case (NR) may need rebalancing if it goes to LEVEL 3 ---------

 -- (putNR l e r): Put in R subtree of (N l e r), BF=-1 , (never returns P)
 putNR _ _ E            = error "pushHR_: Bug3"       -- impossible if BF=-1
 putNR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (Z rl re rr) = putNRR l e rl re rr         -- RR (never returns P)

 ----------------------------- LEVEL 3 ---------------------------------
 --                            putNRR                                 --
 -----------------------------------------------------------------------

 -- (putNRR l e rl re rr): Put in RR subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRR #-}
 putNRR l e rl re  E              = Z (Z l e rl) re t0                  -- l and rl must also be E, special CASE RR!!
 putNRR l e rl re (N rrl rre rrr) = let rr' = putNR rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (P rrl rre rrr) = let rr' = putPR rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (Z rrl rre rrr) = let rr' = putZR rrl rre rrr         -- RR subtree BF= 0, so need to look for changes
                                    in case rr' of
                                    Z _ _ _ -> N l e (Z rl re rr')      -- RR subtree BF: 0-> 0, H:h->h, so no change
                                    N _ _ _ -> Z (Z l e rl) re rr'      -- RR subtree BF: 0->-1, H:h->h+1, parent BF:-1->-2, CASE RR !!
                                    _       -> error "pushHR_: Bug4"    -- impossible
-----------------------------------------------------------------------
-------------------------- pushHR_ Ends Here --------------------------
-----------------------------------------------------------------------

