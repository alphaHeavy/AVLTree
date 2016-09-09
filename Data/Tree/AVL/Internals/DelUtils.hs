{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Internals.DelUtils
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines utility functions for deleting elements from AVL trees.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Internals.DelUtils
        (-- * Deleting utilities.
         delRN,delRZ,delRP,delLN,delLZ,delLP,

         -- * Popping utilities.
         popRN,popRZ,popRP,popLN,popLZ,popLP,
         popHL,popHLN,popHLZ,popHLP,

         -- * Balancing utilities.
         chkLN,chkLZ,chkLP,chkRN,chkRZ,chkRP,
         chkLN',chkLZ',chkLP',chkRN',chkRZ',chkRP',

         -- * Node substitution utilities.
         subN,subZR,subZL,subP,

         -- * BinPath related.
         deletePath,
        ) where

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.BinPath(sel,goL,goR)

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

{------------------------------------------------------------------------------------------------------------------------------
 -------------------------------------- Notes about Deletion and Rebalancing -------------------------------------------------
 ------------------------------------------------------------------------------------------------------------------------------
If you go through a similar analysis to that indicated in the Push.hs module (which I haven't illustrated
here with ASCII art) it can be seen that (as with insertion) the height change in a tree which occurs
as a result of deletion of a node can be infered from the change in BF, (whether or not a re-balancing
rotation was required). The rules are:
      BF +/-1 ->    0, height decreased by 1
      BF    0 -> +/-1, height unchanged.
      BF unchanged   , height unchanged.
      BF +/-1 -> -/+1, height unchanged.

Unlike insertion, rebalancing on deletion requires pattern matching on nodes which aren't on the
current path, hence the existance of separate rebalancing functions (rebalN and rebalP).

-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------}


-----------------------------------------------------------------------
------------------------ delL Starts Here -----------------------------
-----------------------------------------------------------------------
-------------------------- delL LEVEL 1 -------------------------------
--                      delLN, delLZ, delLP                          --
-----------------------------------------------------------------------
-- Delete leftmost from (N l e r)
delLN :: AVL e -> e -> AVL e -> AVL e
delLN  E           _ r = r                          -- Terminal case, r must be of form (Z E re E)
delLN (N ll le lr) e r = chkLN (delLN ll le lr) e r
delLN (Z ll le lr) e r = delLNZ ll le lr e r
delLN (P ll le lr) e r = chkLN (delLP ll le lr) e r

-- Delete leftmost from (Z l e r)
delLZ :: AVL e -> e -> AVL e -> AVL e
delLZ  E           _ _ = E                          -- Terminal case, r must be E
delLZ (N ll le lr) e r = delLZN ll le lr e r
delLZ (Z ll le lr) e r = delLZZ ll le lr e r
delLZ (P ll le lr) e r = delLZP ll le lr e r

-- Delete leftmost from (P l e r)
delLP :: AVL e -> e -> AVL e -> AVL e
delLP  E           _ _ = error "delLP: Bug0"       -- Impossible if BF=+1
delLP (N ll le lr) e r = chkLP (delLN ll le lr) e r
delLP (Z ll le lr) e r = delLPZ ll le lr e r
delLP (P ll le lr) e r = chkLP (delLP ll le lr) e r

-------------------------- delL LEVEL 2 -------------------------------
--                     delLNZ, delLZZ, delLPZ                        --
--                        delLZN, delLZP                             --
-----------------------------------------------------------------------

-- Delete leftmost from (N (Z ll le lr) e r), height of left sub-tree can't change in this case
{-# INLINE delLNZ #-}
delLNZ :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
delLNZ  E              _  _  e r = rebalN E e r                     -- Terminal case, Needs rebalancing
delLNZ (N lll lle llr) le lr e r = let l' = delLZN lll lle llr le lr in l' `seq` N l' e r
delLNZ (Z lll lle llr) le lr e r = let l' = delLZZ lll lle llr le lr in l' `seq` N l' e r
delLNZ (P lll lle llr) le lr e r = let l' = delLZP lll lle llr le lr in l' `seq` N l' e r

-- Delete leftmost from (Z (Z ll le lr) e r), height of left sub-tree can't change in this case
-- Don't inline
delLZZ :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
delLZZ  E              _  _  e r = N E e r                           -- Terminal case
delLZZ (N lll lle llr) le lr e r = let l' = delLZN lll lle llr le lr in l' `seq` Z l' e r
delLZZ (Z lll lle llr) le lr e r = let l' = delLZZ lll lle llr le lr in l' `seq` Z l' e r
delLZZ (P lll lle llr) le lr e r = let l' = delLZP lll lle llr le lr in l' `seq` Z l' e r

-- Delete leftmost from (P (Z ll le lr) e r), height of left sub-tree can't change in this case
{-# INLINE delLPZ #-}
delLPZ :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
delLPZ  E              _  _  e _ = Z E e E                           -- Terminal case
delLPZ (N lll lle llr) le lr e r = let l' = delLZN lll lle llr le lr in l' `seq` P l' e r
delLPZ (Z lll lle llr) le lr e r = let l' = delLZZ lll lle llr le lr in l' `seq` P l' e r
delLPZ (P lll lle llr) le lr e r = let l' = delLZP lll lle llr le lr in l' `seq` P l' e r

-- Delete leftmost from (Z (N ll le lr) e r)
{-# INLINE delLZN #-}
delLZN :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
delLZN ll le lr e r = chkLZ (delLN ll le lr) e r

-- Delete leftmost from (Z (P ll le lr) e r)
{-# INLINE delLZP #-}
delLZP :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
delLZP ll le lr e r = chkLZ (delLP ll le lr) e r
-----------------------------------------------------------------------
-------------------------- delL Ends Here -----------------------------
-----------------------------------------------------------------------



-----------------------------------------------------------------------
------------------------ delR Starts Here -----------------------------
-----------------------------------------------------------------------
-------------------------- delR LEVEL 1 -------------------------------
--                      delRN, delRZ, delRP                          --
-----------------------------------------------------------------------
-- Delete rightmost from (N l e r)
delRN :: AVL e -> e -> AVL e -> AVL e
delRN _ _  E           = error "delRN: Bug0"           -- Impossible if BF=-1
delRN l e (N rl re rr) = chkRN l e (delRN rl re rr)
delRN l e (Z rl re rr) = delRNZ l e rl re rr
delRN l e (P rl re rr) = chkRN l e (delRP rl re rr)

-- Delete rightmost from (Z l e r)
delRZ :: AVL e -> e -> AVL e -> AVL e
delRZ _ _  E           = E                          -- Terminal case, l must be E
delRZ l e (N rl re rr) = delRZN l e rl re rr
delRZ l e (Z rl re rr) = delRZZ l e rl re rr
delRZ l e (P rl re rr) = delRZP l e rl re rr

-- Delete rightmost from (P l e r)
delRP :: AVL e -> e -> AVL e -> AVL e
delRP l _  E           = l                          -- Terminal case, l must be of form (Z E le E)
delRP l e (N rl re rr) = chkRP l e (delRN rl re rr)
delRP l e (Z rl re rr) = delRPZ l e rl re rr
delRP l e (P rl re rr) = chkRP l e (delRP rl re rr)

-------------------------- delR LEVEL 2 -------------------------------
--                     delRNZ, delRZZ, delRPZ                        --
--                        delRZN, delRZP                             --
-----------------------------------------------------------------------

-- Delete rightmost from (N l e (Z rl re rr)), height of right sub-tree can't change in this case
delRNZ :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
{-# INLINE delRNZ #-}
delRNZ _ e _  _   E              = Z E e E                           -- Terminal case
delRNZ l e rl re (N rrl rre rrr) = let r' = delRZN rl re rrl rre rrr in r' `seq` N l e r'
delRNZ l e rl re (Z rrl rre rrr) = let r' = delRZZ rl re rrl rre rrr in r' `seq` N l e r'
delRNZ l e rl re (P rrl rre rrr) = let r' = delRZP rl re rrl rre rrr in r' `seq` N l e r'

-- Delete rightmost from (Z l e (Z rl re rr)), height of right sub-tree can't change in this case
delRZZ :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
delRZZ l e _  _   E              = P l e E                           -- Terminal case
delRZZ l e rl re (N rrl rre rrr) = let r' = delRZN rl re rrl rre rrr in r' `seq` Z l e r'
delRZZ l e rl re (Z rrl rre rrr) = let r' = delRZZ rl re rrl rre rrr in r' `seq` Z l e r'
delRZZ l e rl re (P rrl rre rrr) = let r' = delRZP rl re rrl rre rrr in r' `seq` Z l e r'

-- Delete rightmost from (P l e (Z rl re rr)), height of right sub-tree can't change in this case
delRPZ :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
{-# INLINE delRPZ #-}
delRPZ l e _  _   E              = rebalP l e E                     -- Terminal case, Needs rebalancing
delRPZ l e rl re (N rrl rre rrr) = let r' = delRZN rl re rrl rre rrr in r' `seq` P l e r'
delRPZ l e rl re (Z rrl rre rrr) = let r' = delRZZ rl re rrl rre rrr in r' `seq` P l e r'
delRPZ l e rl re (P rrl rre rrr) = let r' = delRZP rl re rrl rre rrr in r' `seq` P l e r'

-- Delete rightmost from (Z l e (N rl re rr))
delRZN :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
{-# INLINE delRZN #-}
delRZN l e rl re rr = chkRZ l e (delRN rl re rr)

-- Delete rightmost from (Z l e (P rl re rr))
delRZP :: AVL e -> e -> AVL e -> e -> AVL e -> AVL e
{-# INLINE delRZP #-}
delRZP l e rl re rr = chkRZ l e (delRP rl re rr)
-----------------------------------------------------------------------
-------------------------- delR Ends Here -----------------------------
-----------------------------------------------------------------------



-----------------------------------------------------------------------
------------------------ popL Starts Here -----------------------------
-----------------------------------------------------------------------
-------------------------- popL LEVEL 1 -------------------------------
--                      popLN, popLZ, popLP                          --
-----------------------------------------------------------------------
-- Delete leftmost from (N l e r)
popLN :: AVL e -> e -> AVL e -> UBT2(e,AVL e)
popLN  E           e r = UBT2(e,r)                  -- Terminal case, r must be of form (Z E re E)
popLN (N ll le lr) e r = case popLN ll le lr of
                         UBT2(v,l) -> let t = chkLN l e r in  t `seq` UBT2(v,t)
popLN (Z ll le lr) e r = popLNZ ll le lr e r
popLN (P ll le lr) e r = case popLP ll le lr of
                         UBT2(v,l) -> let t = chkLN l e r in  t `seq` UBT2(v,t)

-- Delete leftmost from (Z l e r)
popLZ :: AVL e -> e -> AVL e -> UBT2(e,AVL e)
popLZ  E           e _ = UBT2(e,E)                  -- Terminal case, r must be E
popLZ (N ll le lr) e r = popLZN ll le lr e r
popLZ (Z ll le lr) e r = popLZZ ll le lr e r
popLZ (P ll le lr) e r = popLZP ll le lr e r

-- Delete leftmost from (P l e r)
popLP :: AVL e -> e -> AVL e -> UBT2(e,AVL e)
popLP  E           _ _ = error "popLP: Bug!"        -- Impossible if BF=+1
popLP (N ll le lr) e r = case popLN ll le lr of
                         UBT2(v,l) -> let t = chkLP l e r in  t `seq` UBT2(v,t)
popLP (Z ll le lr) e r = popLPZ ll le lr e r
popLP (P ll le lr) e r = case popLP ll le lr of
                         UBT2(v,l) -> let t = chkLP l e r in  t `seq` UBT2(v,t)

-------------------------- popL LEVEL 2 -------------------------------
--                     popLNZ, popLZZ, popLPZ                        --
--                        popLZN, popLZP                             --
-----------------------------------------------------------------------

-- Delete leftmost from (N (Z ll le lr) e r), height of left sub-tree can't change in this case
popLNZ :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(e,AVL e)
{-# INLINE popLNZ #-}
popLNZ  E              le _  e r = let t = rebalN E e r              -- Terminal case, Needs rebalancing
                                   in  t `seq` UBT2(le,t)
popLNZ (N lll lle llr) le lr e r = case popLZN lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, N l e r)
popLNZ (Z lll lle llr) le lr e r = case popLZZ lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, N l e r)
popLNZ (P lll lle llr) le lr e r = case popLZP lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, N l e r)

-- Delete leftmost from (Z (Z ll le lr) e r), height of left sub-tree can't change in this case
-- Don't INLINE this!
popLZZ :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(e,AVL e)
popLZZ  E              le _  e r = UBT2(le, N E e r)                     -- Terminal case
popLZZ (N lll lle llr) le lr e r = case popLZN lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, Z l e r)
popLZZ (Z lll lle llr) le lr e r = case popLZZ lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, Z l e r)
popLZZ (P lll lle llr) le lr e r = case popLZP lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, Z l e r)

-- Delete leftmost from (P (Z ll le lr) e r), height of left sub-tree can't change in this case
popLPZ :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(e,AVL e)
{-# INLINE popLPZ #-}
popLPZ  E              le _  e _ = UBT2(le, Z E e E)                     -- Terminal case
popLPZ (N lll lle llr) le lr e r = case popLZN lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, P l e r)
popLPZ (Z lll lle llr) le lr e r = case popLZZ lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, P l e r)
popLPZ (P lll lle llr) le lr e r = case popLZP lll lle llr le lr of
                                   UBT2(v,l) -> UBT2(v, P l e r)

-- Delete leftmost from (Z (N ll le lr) e r)
-- Don't INLINE this!
popLZN :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(e,AVL e)
popLZN ll le lr e r = case popLN ll le lr of
                      UBT2(v,l) -> let t = chkLZ l e r in  t `seq` UBT2(v,t)
-- Delete leftmost from (Z (P ll le lr) e r)
-- Don't INLINE this!
popLZP :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(e,AVL e)
popLZP ll le lr e r = case popLP ll le lr of
                      UBT2(v,l) -> let t = chkLZ l e r in t `seq` UBT2(v,t)
-----------------------------------------------------------------------
-------------------------- popL Ends Here -----------------------------
-----------------------------------------------------------------------



-----------------------------------------------------------------------
------------------------ popR Starts Here -----------------------------
-----------------------------------------------------------------------
-------------------------- popR LEVEL 1 -------------------------------
--                      popRN, popRZ, popRP                          --
-----------------------------------------------------------------------
-- Delete rightmost from (N l e r)
popRN :: AVL e -> e -> AVL e -> UBT2(AVL e,e)
popRN _ _  E           = error "popRN: Bug!"        -- Impossible if BF=-1
popRN l e (N rl re rr) = case popRN rl re rr of
                         UBT2(r,v) -> let t = chkRN l e r in t `seq` UBT2(t,v)
popRN l e (Z rl re rr) = popRNZ l e rl re rr
popRN l e (P rl re rr) = case popRP rl re rr of
                         UBT2(r,v) -> let t = chkRN l e r in t `seq` UBT2(t,v)

-- Delete rightmost from (Z l e r)
popRZ :: AVL e -> e -> AVL e -> UBT2(AVL e,e)
popRZ _ e  E           = UBT2(E,e)                  -- Terminal case, l must be E
popRZ l e (N rl re rr) = popRZN l e rl re rr
popRZ l e (Z rl re rr) = popRZZ l e rl re rr
popRZ l e (P rl re rr) = popRZP l e rl re rr

-- Delete rightmost from (P l e r)
popRP :: AVL e -> e -> AVL e -> UBT2(AVL e,e)
popRP l e  E           = UBT2(l,e)                  -- Terminal case, l must be of form (Z E le E)
popRP l e (N rl re rr) = case popRN rl re rr of
                         UBT2(r,v) -> let t = chkRP l e r in t `seq` UBT2(t,v)
popRP l e (Z rl re rr) = popRPZ l e rl re rr
popRP l e (P rl re rr) = case popRP rl re rr of
                         UBT2(r,v) -> let t = chkRP l e r in t `seq` UBT2(t,v)

-------------------------- popR LEVEL 2 -------------------------------
--                     popRNZ, popRZZ, popRPZ                        --
--                        popRZN, popRZP                             --
-----------------------------------------------------------------------

-- Delete rightmost from (N l e (Z rl re rr)), height of right sub-tree can't change in this case
popRNZ :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(AVL e,e)
{-# INLINE popRNZ #-}
popRNZ _ e _  re  E              = UBT2(Z E e E, re)                 -- Terminal case
popRNZ l e rl re (N rrl rre rrr) = case popRZN rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(N l e r, v)
popRNZ l e rl re (Z rrl rre rrr) = case popRZZ rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(N l e r, v)
popRNZ l e rl re (P rrl rre rrr) = case popRZP rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(N l e r, v)

-- Delete rightmost from (Z l e (Z rl re rr)), height of right sub-tree can't change in this case
-- Don't INLINE this!
popRZZ :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(AVL e,e)
popRZZ l e _  re  E              = UBT2(P l e E, re)                 -- Terminal case
popRZZ l e rl re (N rrl rre rrr) = case popRZN rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(Z l e r, v)
popRZZ l e rl re (Z rrl rre rrr) = case popRZZ rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(Z l e r, v)
popRZZ l e rl re (P rrl rre rrr) = case popRZP rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(Z l e r, v)

-- Delete rightmost from (P l e (Z rl re rr)), height of right sub-tree can't change in this case
popRPZ :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(AVL e,e)
{-# INLINE popRPZ #-}
popRPZ l e _  re  E              = let t = rebalP l e E             -- Terminal case, Needs rebalancing
                                   in  t `seq` UBT2(t,re)
popRPZ l e rl re (N rrl rre rrr) = case popRZN rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(P l e r, v)
popRPZ l e rl re (Z rrl rre rrr) = case popRZZ rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(P l e r, v)
popRPZ l e rl re (P rrl rre rrr) = case popRZP rl re rrl rre rrr of
                                   UBT2(r,v) -> UBT2(P l e r, v)

-- Delete rightmost from (Z l e (N rl re rr))
-- Don't INLINE this!
popRZN :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(AVL e,e)
popRZN l e rl re rr = case popRN rl re rr of
                      UBT2(r,v) -> let t = chkRZ l e r in  t `seq` UBT2(t,v)

-- Delete rightmost from (Z l e (P rl re rr))
-- Don't INLINE this!
popRZP :: AVL e -> e -> AVL e -> e -> AVL e -> UBT2(AVL e,e)
popRZP l e rl re rr = case popRP rl re rr of
                      UBT2(r,v) -> let t = chkRZ l e r in  t `seq` UBT2(t,v)
-----------------------------------------------------------------------
-------------------------- popR Ends Here -----------------------------
-----------------------------------------------------------------------



-----------------------------------------------------------------------
--------------------- deletePath Starts Here --------------------------
-----------------------------------------------------------------------
-- | Deletes a tree element. Assumes the path bits were extracted from a 'FullBP' constructor.
--
-- Complexity: O(log n)
deletePath :: UINT -> AVL e -> AVL e
deletePath _ E         = error "deletePath: Element not found."
deletePath p (N l e r) = delN p l e r
deletePath p (Z l e r) = delZ p l e r
deletePath p (P l e r) = delP p l e r

----------------------------- LEVEL 1 ---------------------------------
--                       delN, delZ, delP                            --
-----------------------------------------------------------------------

-- Delete from (N l e r)
delN :: UINT -> AVL e -> e -> AVL e -> AVL e
delN p l e r = case sel p of
               LT -> delNL p l e r
               EQ -> subN l r
               GT -> delNR p l e r

-- Delete from (Z l e r)
delZ :: UINT -> AVL e -> e -> AVL e -> AVL e
delZ p l e r = case sel p of
               LT -> delZL p l e r
               EQ -> subZR l r
               GT -> delZR p l e r

-- Delete from (P l e r)
delP :: UINT -> AVL e -> e -> AVL e -> AVL e
delP p l e r = case sel p of
               LT -> delPL p l e r
               EQ -> subP l r
               GT -> delPR p l e r

----------------------------- LEVEL 2 ---------------------------------
--                      delNL, delZL, delPL                          --
--                      delNR, delZR, delPR                          --
-----------------------------------------------------------------------

-- Delete from the left subtree of (N l e r)
delNL :: UINT -> AVL e -> e -> AVL e -> AVL e
delNL p t = dNL (goL p) t
{-# INLINE dNL #-}
dNL :: UINT -> AVL e -> e -> AVL e -> AVL e
dNL _  E           _ _ = error "deletePath: Element not found."              -- Left sub-tree is empty
dNL p (N ll le lr) e r = case sel p of
                         LT -> chkLN  (delNL p ll le lr) e r
                         EQ -> chkLN  (subN  ll    lr) e r
                         GT -> chkLN  (delNR p ll le lr) e r
dNL p (Z ll le lr) e r = case sel p of
                         LT -> let l' = delZL p ll le lr in l' `seq` N l' e r  -- height can't change
                         EQ -> chkLN' (subZR ll    lr) e r                    -- << But it can here
                         GT -> let l' = delZR p ll le lr in l' `seq` N l' e r  -- height can't change
dNL p (P ll le lr) e r = case sel p of
                         LT -> chkLN  (delPL p ll le lr) e r
                         EQ -> chkLN  (subP  ll    lr) e r
                         GT -> chkLN  (delPR p ll le lr) e r

-- Delete from the right subtree of (N l e r)
delNR :: UINT -> AVL e -> e -> AVL e -> AVL e
delNR p t = dNR (goR p) t
{-# INLINE dNR #-}
dNR :: UINT -> AVL e -> e -> AVL e -> AVL e
dNR _ _ _  E           = error "delNR: Bug0"             -- Impossible
dNR p l e (N rl re rr) = case sel p of
                         LT -> chkRN  l e (delNL p rl re rr)
                         EQ -> chkRN  l e (subN  rl    rr)
                         GT -> chkRN  l e (delNR p rl re rr)
dNR p l e (Z rl re rr) = case sel p of
                         LT -> let r' = delZL p rl re rr in r' `seq` N l e r'   -- height can't change
                         EQ -> chkRN' l e (subZL rl    rr)                    -- << But it can here
                         GT -> let r' = delZR p rl re rr in r' `seq` N l e r'   -- height can't change
dNR p l e (P rl re rr) = case sel p of
                         LT -> chkRN  l e (delPL p rl re rr)
                         EQ -> chkRN  l e (subP  rl    rr)
                         GT -> chkRN  l e (delPR p rl re rr)

-- Delete from the left subtree of (Z l e r)
delZL :: UINT -> AVL e -> e -> AVL e -> AVL e
delZL p t = dZL (goL p) t
{-# INLINE dZL #-}
dZL :: UINT -> AVL e -> e -> AVL e -> AVL e
dZL _  E           _ _ = error "deletePath: Element not found."               -- Left sub-tree is empty
dZL p (N ll le lr) e r = case sel p of
                         LT -> chkLZ  (delNL p ll le lr) e r
                         EQ -> chkLZ  (subN  ll    lr) e r
                         GT -> chkLZ  (delNR p ll le lr) e r
dZL p (Z ll le lr) e r = case sel p of
                         LT -> let l' = delZL p ll le lr in l' `seq` Z l' e r  -- height can't change
                         EQ -> chkLZ'  (subZR ll    lr) e r                  -- << But it can here
                         GT -> let l' = delZR p ll le lr in l' `seq` Z l' e r  -- height can't change
dZL p (P ll le lr) e r = case sel p of
                         LT -> chkLZ  (delPL p ll le lr) e r
                         EQ -> chkLZ  (subP  ll    lr) e r
                         GT -> chkLZ  (delPR p ll le lr) e r

-- Delete from the right subtree of (Z l e r)
delZR :: UINT -> AVL e -> e -> AVL e -> AVL e
delZR p t = dZR (goR p) t
{-# INLINE dZR #-}
dZR :: UINT -> AVL e -> e -> AVL e -> AVL e
dZR _ _ _  E           = error "deletePath: Element not found."              -- Right sub-tree is empty
dZR p l e (N rl re rr) = case sel p of
                         LT -> chkRZ  l e (delNL p rl re rr)
                         EQ -> chkRZ  l e (subN  rl    rr)
                         GT -> chkRZ  l e (delNR p rl re rr)
dZR p l e (Z rl re rr) = case sel p of
                         LT -> let r' = delZL p rl re rr in r' `seq` Z l e r'  -- height can't change
                         EQ -> chkRZ' l e (subZL rl rr)                      -- << But it can here
                         GT -> let r' = delZR p rl re rr in r' `seq` Z l e r'  -- height can't change
dZR p l e (P rl re rr) = case sel p of
                         LT -> chkRZ  l e (delPL p rl re rr)
                         EQ -> chkRZ  l e (subP    rl    rr)
                         GT -> chkRZ  l e (delPR p rl re rr)

-- Delete from the left subtree of (P l e r)
delPL :: UINT -> AVL e -> e -> AVL e -> AVL e
delPL p t = dPL (goL p) t
{-# INLINE dPL #-}
dPL :: UINT -> AVL e -> e -> AVL e -> AVL e
dPL _  E           _ _ = error "delPL: Bug0"             -- Impossible
dPL p (N ll le lr) e r = case sel p of
                         LT -> chkLP  (delNL p ll le lr) e r
                         EQ -> chkLP  (subN    ll    lr) e r
                         GT -> chkLP  (delNR p ll le lr) e r
dPL p (Z ll le lr) e r = case sel p of
                         LT -> let l' = delZL p ll le lr in l' `seq` P l' e r  -- height can't change
                         EQ -> chkLP' (subZR ll lr) e r                        -- << But it can here
                         GT -> let l' = delZR p ll le lr in l' `seq` P l' e r  -- height can't change
dPL p (P ll le lr) e r = case sel p of
                         LT -> chkLP  (delPL p ll le lr) e r
                         EQ -> chkLP  (subP    ll    lr) e r
                         GT -> chkLP  (delPR p ll le lr) e r

-- Delete from the right subtree of (P l e r)
delPR :: UINT -> AVL e -> e -> AVL e -> AVL e
delPR p t = dPR (goR p) t
{-# INLINE dPR #-}
dPR :: UINT -> AVL e -> e -> AVL e -> AVL e
dPR _ _ _  E           = error "deletePath: Element not found."               -- Right sub-tree is empty
dPR p l e (N rl re rr) = case sel p of
                         LT -> chkRP  l e (delNL p rl re rr)
                         EQ -> chkRP  l e (subN    rl    rr)
                         GT -> chkRP  l e (delNR p rl re rr)
dPR p l e (Z rl re rr) = case sel p of
                         LT -> let r' = delZL p rl re rr in r' `seq` P l e r'  -- height can't change
                         EQ -> chkRP' l e (subZL rl rr)                        -- << But it can here
                         GT -> let r' = delZR p rl re rr in r' `seq` P l e r'  -- height can't change
dPR p l e (P rl re rr) = case sel p of
                         LT -> chkRP  l e (delPL p rl re rr)
                         EQ -> chkRP  l e (subP    rl    rr)
                         GT -> chkRP  l e (delPR p rl re rr)
-----------------------------------------------------------------------
----------------------- deletePath Ends Here --------------------------
-----------------------------------------------------------------------



-------------------------------------------------------------------------------------
-- This is a modified version of popL which returns the (popped) tree height as well.
-------------------------------------------------------------------------------------
popHL :: AVL e -> UBT3(e,AVL e,UINT)
popHL  E        = error "popHL: Empty tree."
popHL (N l e r) = popHLN l e r
popHL (Z l e r) = popHLZ l e r
popHL (P l e r) = popHLP l e r

popHLN :: AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLN l e r = case popHLN_ L(2) l e r of
               UBT3(e_,t,h) -> case t of
                  E        -> error "popHLN: Bug0"           -- impossible
                  Z _ _ _  -> UBT3(e_,t,DECINT1(h))          -- dH = -1
                  _        -> UBT3(e_,t,        h )          -- dH =  0

popHLZ :: AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLZ l e r = case popHLZ_ L(1) l e r of
               UBT3(e_,t,h) -> case t of
                  E        -> UBT3(e,E,L(0))                 -- Resulting tree is empty
                  P _ _ _  -> error "popHLZ: Bug0"           -- impossible
                  _        -> UBT3(e_,t,        h )          -- dH =  0

popHLP :: AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLP l e r = case popHLP_ L(1) l e r of
               UBT3(e_,t,h) -> case t of
                  Z _ _ _  -> UBT3(e_,t,DECINT1(h))          -- dH = -1
                  P _ _ _  -> UBT3(e_,t,        h )          -- dH =  0
                  _        -> error "popHLP: Bug0"           -- impossible

-------------------------- popHL LEVEL 1 ------------------------------
--                      popHLN_, popHLZ_, popHLP_                    --
-----------------------------------------------------------------------
-- Delete leftmost from (N l e r)
popHLN_ :: UINT -> AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLN_ h  E           e r = UBT3(e,r,h)                        -- Terminal case, r must be of form (Z E re E)
popHLN_ h (N ll le lr) e r = case popHLN_ INCINT2(h) ll le lr of
                             UBT3(e_,l,hl) -> let t = chkLN l e r in t `seq` UBT3(e_,t,hl)
popHLN_ h (Z ll le lr) e r = popHLNZ INCINT1(h) ll le lr e r
popHLN_ h (P ll le lr) e r = case popHLP_ INCINT1(h) ll le lr of
                             UBT3(e_,l,hl) -> let t = chkLN l e r in t `seq` UBT3(e_,t,hl)

-- Delete leftmost from (Z l e r)
{-# INLINE popHLZ_ #-}
popHLZ_ :: UINT -> AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLZ_ h  E           e _ = UBT3(e,E,h)                       -- Terminal case, r must be E
popHLZ_ h (N ll le lr) e r = popHLZN INCINT2(h) ll le lr e r
popHLZ_ h (Z ll le lr) e r = popHLZZ INCINT1(h) ll le lr e r
popHLZ_ h (P ll le lr) e r = popHLZP INCINT1(h) ll le lr e r

-- Delete leftmost from (P l e r)
popHLP_ :: UINT -> AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLP_ _  E           _ _ = error "popHLP_: Bug0"             -- Impossible if BF=+1
popHLP_ h (N ll le lr) e r = case popHLN_ INCINT2(h) ll le lr of
                             UBT3(e_,l,hl) -> let t = chkLP l e r in  t `seq` UBT3(e_,t,hl)
popHLP_ h (Z ll le lr) e r = popHLPZ INCINT1(h) ll le lr e r
popHLP_ h (P ll le lr) e r = case popHLP_ INCINT1(h) ll le lr of
                             UBT3(e_,l,hl) -> let t = chkLP l e r in  t `seq` UBT3(e_,t,hl)

-------------------------- popHL LEVEL 2 ------------------------------
--                     popHLNZ, popHLZZ, popHLPZ                     --
--                        popHLZN, popHLZP                           --
-----------------------------------------------------------------------

-- Delete leftmost from (N (Z ll le lr) e r), height of left sub-tree can't change in this case
{-# INLINE popHLNZ #-}
popHLNZ :: UINT -> AVL e -> e -> AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLNZ h  E              le _  e r = let t = rebalN E e r         -- Terminal case, Needs rebalancing
                                      in  t `seq` UBT3(le,t,h)
popHLNZ h (N lll lle llr) le lr e r = case popHLZN INCINT2(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, N l e r, hl)
popHLNZ h (Z lll lle llr) le lr e r = case popHLZZ INCINT1(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, N l e r, hl)
popHLNZ h (P lll lle llr) le lr e r = case popHLZP INCINT1(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, N l e r, hl)

-- Delete leftmost from (Z (Z ll le lr) e r), height of left sub-tree can't change in this case
-- Don't INLINE this!
popHLZZ :: UINT -> AVL e -> e -> AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLZZ h  E              le _  e r = UBT3(le, N E e r, h)            -- Terminal case
popHLZZ h (N lll lle llr) le lr e r = case popHLZN INCINT2(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, Z l e r, hl)
popHLZZ h (Z lll lle llr) le lr e r = case popHLZZ INCINT1(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, Z l e r, hl)
popHLZZ h (P lll lle llr) le lr e r = case popHLZP INCINT1(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, Z l e r, hl)

-- Delete leftmost from (P (Z ll le lr) e r), height of left sub-tree can't change in this case
{-# INLINE popHLPZ #-}
popHLPZ :: UINT -> AVL e -> e -> AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLPZ h  E              le _  e _ = UBT3(le, Z E e E, h)            -- Terminal case
popHLPZ h (N lll lle llr) le lr e r = case popHLZN INCINT2(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, P l e r, hl)
popHLPZ h (Z lll lle llr) le lr e r = case popHLZZ INCINT1(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, P l e r, hl)
popHLPZ h (P lll lle llr) le lr e r = case popHLZP INCINT1(h) lll lle llr le lr of
                                      UBT3(e_,l,hl) -> UBT3(e_, P l e r, hl)

-- Delete leftmost from (Z (N ll le lr) e r)
-- Don't INLINE this!
popHLZN :: UINT -> AVL e -> e -> AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLZN h ll le lr e r = case popHLN_ h ll le lr of
                         UBT3(e_,l,hl) -> let t = chkLZ l e r in  t `seq` UBT3(e_,t,hl)
-- Delete leftmost from (Z (P ll le lr) e r)
-- Don't INLINE this!
popHLZP :: UINT -> AVL e -> e -> AVL e -> e -> AVL e -> UBT3(e,AVL e,UINT)
popHLZP h ll le lr e r = case popHLP_ h ll le lr of
                         UBT3(e_,l,hl) -> let t = chkLZ l e r in  t `seq` UBT3(e_,t,hl)
-----------------------------------------------------------------------
------------------------- popHL Ends Here -----------------------------
-----------------------------------------------------------------------

{-************************** Balancing Utilities Below Here ************************************-}

-- Rebalance a tree of form (N l e r) which has become unbalanced as
-- a result of the height of the left sub-tree (l) decreasing by 1.
-- N.B Result is never of form (N _ _ _) (or E!)
rebalN :: AVL e -> e -> AVL e -> AVL e
rebalN _ _  E                        = error "rebalN: Bug0"             -- impossible case
rebalN l e (N rl              re rr) = Z (Z l e rl) re rr               -- N->Z, dH=-1
rebalN l e (Z rl              re rr) = P (N l e rl) re rr               -- N->P, dH= 0
rebalN _ _ (P  E               _  _) = error "rebalN: Bug1"             -- impossible case
rebalN l e (P (N rll rle rlr) re rr) = Z (P l e rll) rle (Z rlr re rr)  -- N->Z, dH=-1
rebalN l e (P (Z rll rle rlr) re rr) = Z (Z l e rll) rle (Z rlr re rr)  -- N->Z, dH=-1
rebalN l e (P (P rll rle rlr) re rr) = Z (Z l e rll) rle (N rlr re rr)  -- N->Z, dH=-1

-- Rebalance a tree of form (P l e r) which has become unbalanced as
-- a result of the height of the right sub-tree (r) decreasing by 1.
-- N.B Result is never of form (P _ _ _) (or E!)
rebalP :: AVL e -> e -> AVL e -> AVL e
rebalP  E                        _ _ = error "rebalP: Bug0"             -- impossible case
rebalP (P ll le lr             ) e r = Z ll le (Z lr e r)               -- P->Z, dH=-1
rebalP (Z ll le lr             ) e r = N ll le (P lr e r)               -- P->N, dH= 0
rebalP (N  _  _  E             ) _ _ = error "rebalP: Bug1"             -- impossible case
rebalP (N ll le (P lrl lre lrr)) e r = Z (Z ll le lrl) lre (N lrr e r)  -- P->Z, dH=-1
rebalP (N ll le (Z lrl lre lrr)) e r = Z (Z ll le lrl) lre (Z lrr e r)  -- P->Z, dH=-1
rebalP (N ll le (N lrl lre lrr)) e r = Z (P ll le lrl) lre (Z lrr e r)  -- P->Z, dH=-1

-- Check for height changes in left subtree of (N l e r),
-- where l was (N ll le lr) or (P ll le lr)
chkLN :: AVL e -> e -> AVL e -> AVL e
chkLN l e r = case l of
              E       -> error "chkLN: Bug0"   -- impossible if BF<>0
              N _ _ _ -> N l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> rebalN l e r          -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> N l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in left subtree of (Z l e r),
-- where l was (N ll le lr) or (P ll le lr)
chkLZ :: AVL e -> e -> AVL e -> AVL e
chkLZ l e r = case l of
              E       -> error "chkLZ: Bug0"   -- impossible if BF<>0
              N _ _ _ -> Z l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> N l e r               -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> Z l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in left subtree of (P l e r),
-- where l was (N ll le lr) or (P ll le lr)
chkLP :: AVL e -> e -> AVL e -> AVL e
chkLP l e r = case l of
              E       -> error "chkLP: Bug0"   -- impossible if BF<>0
              N _ _ _ -> P l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> Z l e r               -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> P l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (N l e r),
-- where r was (N rl re rr) or (P rl re rr)
chkRN :: AVL e -> e -> AVL e -> AVL e
chkRN l e r = case r of
              E       -> error "chkRN: Bug0"   -- impossible if BF<>0
              N _ _ _ -> N l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> Z l e r               -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> N l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (Z l e r),
-- where r was (N rl re rr) or (P rl re rr)
chkRZ :: AVL e -> e -> AVL e -> AVL e
chkRZ l e r = case r of
              E       -> error "chkRZ: Bug0"   -- impossible if BF<>0
              N _ _ _ -> Z l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> P l e r               -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> Z l e r               -- BF +/-1 -> +1, so dH= 0
-- Check for height changes in right subtree of (P l e r),
-- where l was (N rl re rr) or (P rl re rr)
chkRP :: AVL e -> e -> AVL e -> AVL e
chkRP l e r = case r of
              E       -> error "chkRP: Bug0"   -- impossible if BF<>0
              N _ _ _ -> P l e r               -- BF +/-1 -> -1, so dH= 0
              Z _ _ _ -> rebalP l e r          -- BF +/-1 ->  0, so dH=-1
              P _ _ _ -> P l e r               -- BF +/-1 -> +1, so dH= 0

-- Substitute deleted element from (N l _ r)
subN :: AVL e -> AVL e -> AVL e
subN _  E            = error "subN: Bug0"      -- Impossible
subN l (N rl re rr)  = case popLN rl re rr of UBT2(e,r_) -> chkRN  l e r_
subN l (Z rl re rr)  = case popLZ rl re rr of UBT2(e,r_) -> chkRN' l e r_
subN l (P rl re rr)  = case popLP rl re rr of UBT2(e,r_) -> chkRN  l e r_

-- Substitute deleted element from (Z l _ r)
-- Pops the replacement from the right sub-tree, so result may be (P _ _ _)
subZR :: AVL e -> AVL e -> AVL e
subZR _  E            = E   -- Both left and right subtrees must have been empty
subZR l (N rl re rr)  = case popLN rl re rr of UBT2(e,r_) -> chkRZ  l e r_
subZR l (Z rl re rr)  = case popLZ rl re rr of UBT2(e,r_) -> chkRZ' l e r_
subZR l (P rl re rr)  = case popLP rl re rr of UBT2(e,r_) -> chkRZ  l e r_

-- Local utility to substitute deleted element from (Z l _ r)
-- Pops the replacement from the left sub-tree, so result may be (N _ _ _)
subZL :: AVL e -> AVL e -> AVL e
subZL  E           _  = E   -- Both left and right subtrees must have been empty
subZL (N ll le lr) r  = case popRN ll le lr of UBT2(l_,e) -> chkLZ  l_ e r
subZL (Z ll le lr) r  = case popRZ ll le lr of UBT2(l_,e) -> chkLZ' l_ e r
subZL (P ll le lr) r  = case popRP ll le lr of UBT2(l_,e) -> chkLZ  l_ e r

-- Substitute deleted element from (P l _ r)
subP :: AVL e -> AVL e -> AVL e
subP  E           _  = error "subP: Bug0"      -- Impossible
subP (N ll le lr) r  = case popRN ll le lr of UBT2(l_,e) -> chkLP  l_ e r
subP (Z ll le lr) r  = case popRZ ll le lr of UBT2(l_,e) -> chkLP' l_ e r
subP (P ll le lr) r  = case popRP ll le lr of UBT2(l_,e) -> chkLP  l_ e r

-- Check for height changes in left subtree of (N l e r),
-- where l was (Z ll le lr)
chkLN' :: AVL e -> e -> AVL e -> AVL e
chkLN' l e r = case l of
               E       -> rebalN l e r  -- BF 0 -> E, so dH=-1
               _       -> N l e r       -- Otherwise dH=0
-- Check for height changes in left subtree of (Z l e r),
-- where l was (Z ll le lr)
chkLZ' :: AVL e -> e -> AVL e -> AVL e
chkLZ' l e r = case l of
               E       -> N l e r      -- BF 0 -> E, so dH=-1
               _       -> Z l e r      -- Otherwise dH=0
-- Check for height changes in left subtree of (P l e r),
-- where l was (Z ll le lr)
chkLP' :: AVL e -> e -> AVL e -> AVL e
chkLP' l e r = case l of
               E       -> Z l e r      -- BF 0 -> E, so dH=-1
               _       -> P l e r      -- Otherwise dH=0
-- Check for height changes in right subtree of (N l e r),
-- where r was (Z rl re rr)
chkRN' :: AVL e -> e -> AVL e -> AVL e
chkRN' l e r = case r of
               E       -> Z l e r      -- BF 0 -> E, so dH=-1
               _       -> N l e r      -- Otherwise dH=0
-- Check for height changes in right subtree of (Z l e r),
-- where r was (Z rl re rr)
chkRZ' :: AVL e -> e -> AVL e -> AVL e
chkRZ' l e r = case r of
               E       -> P l e r      -- BF 0 -> E, so dH=-1
               _       -> Z l e r      -- Otherwise dH=0
-- Check for height changes in right subtree of (P l e r),
-- where l was (Z rl re rr)
chkRP' :: AVL e -> e -> AVL e -> AVL e
chkRP' l e r = case r of
               E       -> rebalP l e r -- BF 0 -> E, so dH=-1
               _       -> P l e r      -- Otherwise dH=0

