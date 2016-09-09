{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Delete
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.Delete
(-- * Deleting elements from AVL trees

 -- ** Deleting from extreme left or right
 delL,delR,assertDelL,assertDelR,tryDelL,tryDelR,

 -- ** Deleting from /sorted/ trees
 delete,deleteFast,deleteIf,deleteMaybe,

 -- * \"Popping\" elements from AVL trees
 -- | \"Popping\" means reading and deleting a tree element in a single operation.

 -- ** Popping from extreme left or right
 assertPopL,assertPopR,tryPopL,tryPopR,

 -- ** Popping from /sorted/ trees
 assertPop,tryPop,assertPopMaybe,tryPopMaybe,assertPopIf,tryPopIf,
) where

import Prelude -- so haddock finds the symbols there

import Data.COrdering
import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.BinPath(BinPath(..),findFullPath,openPathWith,writePath)

import Data.Tree.AVL.Internals.DelUtils
         (-- Deleting Utilities
          delRN,delRZ,delRP,delLN,delLZ,delLP,
          -- Popping Utilities.
          popRN,popRZ,popRP,popLN,popLZ,popLP,
          -- Balancing Utilities
          chkLN,chkLZ,chkLP,chkRN,chkRZ,chkRP,
          chkLN',chkLZ',chkLP',chkRN',chkRZ',chkRP',
          -- Node substitution utilities.
          subN,subZR,subZL,subP,
          -- BinPath related
          deletePath
         )

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | Delete the left-most element of an AVL tree. If the tree is sorted this will be the
-- least element. This function returns an empty tree if it's argument is an empty tree.
--
-- Complexity: O(log n)
delL :: AVL e -> AVL e
delL  E        = E
delL (N l e r) = delLN l e r
delL (Z l e r) = delLZ l e r
delL (P l e r) = delLP l e r

-- | Delete the left-most element of a /non-empty/ AVL tree. If the tree is sorted this will be the
-- least element. This function raises an error if it's argument is an empty tree.
--
-- Complexity: O(log n)
assertDelL :: AVL e -> AVL e
assertDelL  E        = error "assertDelL: Empty tree."
assertDelL (N l e r) = delLN l e r
assertDelL (Z l e r) = delLZ l e r
assertDelL (P l e r) = delLP l e r

-- | Try to delete the left-most element of a /non-empty/ AVL tree. If the tree is sorted this will be the
-- least element. This function returns 'Nothing' if it's argument is an empty tree.
--
-- Complexity: O(log n)
tryDelL :: AVL e -> Maybe (AVL e)
tryDelL  E        = Nothing
tryDelL (N l e r) = Just $! delLN l e r
tryDelL (Z l e r) = Just $! delLZ l e r
tryDelL (P l e r) = Just $! delLP l e r

-- | Delete the right-most element of an AVL tree. If the tree is sorted this will be the
-- greatest element. This function returns an empty tree if it's argument is an empty tree.
--
-- Complexity: O(log n)
delR :: AVL e -> AVL e
delR  E        = E
delR (N l e r) = delRN l e r
delR (Z l e r) = delRZ l e r
delR (P l e r) = delRP l e r

-- | Delete the right-most element of a /non-empty/ AVL tree. If the tree is sorted this will be the
-- greatest element. This function raises an error if it's argument is an empty tree.
--
-- Complexity: O(log n)
assertDelR :: AVL e -> AVL e
assertDelR  E        = error "assertDelR: Empty tree."
assertDelR (N l e r) = delRN l e r
assertDelR (Z l e r) = delRZ l e r
assertDelR (P l e r) = delRP l e r

-- | Try to delete the right-most element of a /non-empty/ AVL tree. If the tree is sorted this will be the
-- greatest element. This function returns 'Nothing' if it's argument is an empty tree.
--
-- Complexity: O(log n)
tryDelR :: AVL e -> Maybe (AVL e)
tryDelR  E        = Nothing
tryDelR (N l e r) = Just $! delRN l e r
tryDelR (Z l e r) = Just $! delRZ l e r
tryDelR (P l e r) = Just $! delRP l e r

-- | Pop the left-most element from a non-empty AVL tree, returning the popped element and the
-- modified AVL tree. If the tree is sorted this will be the least element.
-- This function raises an error if it's argument is an empty tree.
--
-- Complexity: O(log n)
assertPopL :: AVL e -> (e,AVL e)
assertPopL  E        = error "assertPopL: Empty tree."
assertPopL (N l e r) = case popLN l e r of UBT2(v,t) -> (v,t)
assertPopL (Z l e r) = case popLZ l e r of UBT2(v,t) -> (v,t)
assertPopL (P l e r) = case popLP l e r of UBT2(v,t) -> (v,t)

-- | Same as 'assertPopL', except this version returns 'Nothing' if it's argument is an empty tree.
--
-- Complexity: O(log n)
tryPopL :: AVL e -> Maybe (e,AVL e)
tryPopL  E        = Nothing
tryPopL (N l e r) = Just $! case popLN l e r of UBT2(v,t) -> (v,t)
tryPopL (Z l e r) = Just $! case popLZ l e r of UBT2(v,t) -> (v,t)
tryPopL (P l e r) = Just $! case popLP l e r of UBT2(v,t) -> (v,t)


-- | Pop the right-most element from a non-empty AVL tree, returning the popped element and the
-- modified AVL tree. If the tree is sorted this will be the greatest element.
-- This function raises an error if it's argument is an empty tree.
--
-- Complexity: O(log n)
assertPopR :: AVL e -> (AVL e,e)
assertPopR  E        = error "assertPopR: Empty tree."
assertPopR (N l e r) = case popRN l e r of UBT2(t,v) -> (t,v)
assertPopR (Z l e r) = case popRZ l e r of UBT2(t,v) -> (t,v)
assertPopR (P l e r) = case popRP l e r of UBT2(t,v) -> (t,v)

-- | Same as 'assertPopR', except this version returns 'Nothing' if it's argument is an empty tree.
--
-- Complexity: O(log n)
tryPopR :: AVL e -> Maybe (AVL e,e)
tryPopR  E        = Nothing
tryPopR (N l e r) = Just $! case popRN l e r of UBT2(t,v) -> (t,v)
tryPopR (Z l e r) = Just $! case popRZ l e r of UBT2(t,v) -> (t,v)
tryPopR (P l e r) = Just $! case popRP l e r of UBT2(t,v) -> (t,v)

-- | General purpose function for deletion of elements from a sorted AVL tree.
-- If a matching element is not found then this function returns the original tree.
--
-- Complexity: O(log n)
delete :: (e -> Ordering) -> AVL e -> AVL e
delete c t = case findFullPath c t of
             L(-1) -> t                -- Not found, p<0
             p     -> deletePath p t   -- Found, so delete

-- | This version only deletes the element if the supplied selector returns @('Eq' 'True')@.
-- If it returns @('Eq' 'False')@ or if no matching element is found then this function returns
-- the original tree.
--
-- Complexity: O(log n)
deleteIf :: (e -> COrdering Bool) -> AVL e -> AVL e
deleteIf c t = case openPathWith c t of
               FullBP p True -> deletePath p t
               _             -> t

-- | This version only deletes the element if the supplied selector returns @('Eq' 'Nothing')@.
-- If it returns @('Eq' ('Just' e))@  then the matching element is replaced by e.
-- If no matching element is found then this function returns the original tree.
--
-- Complexity: O(log n)
deleteMaybe :: (e -> COrdering (Maybe e)) -> AVL e -> AVL e
deleteMaybe c t = case openPathWith c t of
                  FullBP p Nothing  -> deletePath p t
                  FullBP p (Just e) -> writePath p e t
                  _                 -> t

-- | Functionally identical to 'delete', but returns an identical tree (one with all the nodes on
-- the path duplicated) if the search fails. This should probably only be used if you know the
-- search will succeed.
--
-- Complexity: O(log n)
deleteFast :: (e -> Ordering) -> AVL e -> AVL e
-- This was the old delete so it's been tested OK, but as a different name.
deleteFast c = delete' where
 delete'  E        = E
 delete' (N l e r) = delN l e r
 delete' (Z l e r) = delZ l e r
 delete' (P l e r) = delP l e r

 ----------------------------- LEVEL 1 ---------------------------------
 --                       delN, delZ, delP                            --
 -----------------------------------------------------------------------

 -- Delete from (N l e r)
 delN l e r = case c e of
              LT -> delNL l e r
              EQ -> subN l r
              GT -> delNR l e r

 -- Delete from (Z l e r)
 delZ l e r = case c e of
              LT -> delZL l e r
              EQ -> subZR l r
              GT -> delZR l e r

 -- Delete from (P l e r)
 delP l e r = case c e of
              LT -> delPL l e r
              EQ -> subP l r
              GT -> delPR l e r

 ----------------------------- LEVEL 2 ---------------------------------
 --                      delNL, delZL, delPL                          --
 --                      delNR, delZR, delPR                          --
 -----------------------------------------------------------------------

 -- Delete from the left subtree of (N l e r)
 delNL  E           e r = N E e r                            -- Left sub-tree is empty
 delNL (N ll le lr) e r = case c le of
                          LT -> chkLN  (delNL ll le lr) e r
                          EQ -> chkLN  (subN  ll    lr) e r
                          GT -> chkLN  (delNR ll le lr) e r
 delNL (Z ll le lr) e r = case c le of
                          LT -> let l' = delZL ll le lr in l' `seq` N l' e r  -- height can't change
                          EQ -> chkLN' (subZR ll    lr) e r                    -- << But it can here
                          GT -> let l' = delZR ll le lr in l' `seq` N l' e r  -- height can't change
 delNL (P ll le lr) e r = case c le of
                          LT -> chkLN  (delPL ll le lr) e r
                          EQ -> chkLN  (subP  ll    lr) e r
                          GT -> chkLN  (delPR ll le lr) e r

 -- Delete from the right subtree of (N l e r)
 delNR _ _  E           = error "delNR: Bug0"             -- Impossible
 delNR l e (N rl re rr) = case c re of
                          LT -> chkRN  l e (delNL rl re rr)
                          EQ -> chkRN  l e (subN  rl    rr)
                          GT -> chkRN  l e (delNR rl re rr)
 delNR l e (Z rl re rr) = case c re of
                          LT -> let r' = delZL rl re rr in r' `seq` N l e r'   -- height can't change
                          EQ -> chkRN' l e (subZL rl    rr)                    -- << But it can here
                          GT -> let r' = delZR rl re rr in r' `seq` N l e r'   -- height can't change
 delNR l e (P rl re rr) = case c re of
                          LT -> chkRN  l e (delPL rl re rr)
                          EQ -> chkRN  l e (subP  rl    rr)
                          GT -> chkRN  l e (delPR rl re rr)

 -- Delete from the left subtree of (Z l e r)
 delZL  E           e r = Z E e r                            -- Left sub-tree is empty
 delZL (N ll le lr) e r = case c le of
                          LT -> chkLZ  (delNL ll le lr) e r
                          EQ -> chkLZ  (subN  ll    lr) e r
                          GT -> chkLZ  (delNR ll le lr) e r
 delZL (Z ll le lr) e r = case c le of
                          LT -> let l' = delZL ll le lr in l' `seq` Z l' e r  -- height can't change
                          EQ -> chkLZ'  (subZR ll    lr) e r                  -- << But it can here
                          GT -> let l' = delZR ll le lr in l' `seq` Z l' e r  -- height can't change
 delZL (P ll le lr) e r = case c le of
                          LT -> chkLZ  (delPL ll le lr) e r
                          EQ -> chkLZ  (subP  ll    lr) e r
                          GT -> chkLZ  (delPR ll le lr) e r

 -- Delete from the right subtree of (Z l e r)
 delZR l e  E           = Z l e E                            -- Right sub-tree is empty
 delZR l e (N rl re rr) = case c re of
                          LT -> chkRZ  l e (delNL rl re rr)
                          EQ -> chkRZ  l e (subN  rl    rr)
                          GT -> chkRZ  l e (delNR rl re rr)
 delZR l e (Z rl re rr) = case c re of
                          LT -> let r' = delZL rl re rr in r' `seq` Z l e r'  -- height can't change
                          EQ -> chkRZ' l e (subZL rl    rr)                   -- << But it can here
                          GT -> let r' = delZR rl re rr in r' `seq` Z l e r'  -- height can't change
 delZR l e (P rl re rr) = case c re of
                          LT -> chkRZ  l e (delPL rl re rr)
                          EQ -> chkRZ  l e (subP  rl    rr)
                          GT -> chkRZ  l e (delPR rl re rr)

 -- Delete from the left subtree of (P l e r)
 delPL  E           _ _ = error "delPL: Bug0"             -- Impossible
 delPL (N ll le lr) e r = case c le of
                          LT -> chkLP  (delNL ll le lr) e r
                          EQ -> chkLP  (subN  ll    lr) e r
                          GT -> chkLP  (delNR ll le lr) e r
 delPL (Z ll le lr) e r = case c le of
                          LT -> let l' = delZL ll le lr in l' `seq` P l' e r  -- height can't change
                          EQ -> chkLP' (subZR ll    lr) e r                   -- << But it can here
                          GT -> let l' = delZR ll le lr in l' `seq` P l' e r  -- height can't change
 delPL (P ll le lr) e r = case c le of
                          LT -> chkLP  (delPL ll le lr) e r
                          EQ -> chkLP  (subP  ll    lr) e r
                          GT -> chkLP  (delPR ll le lr) e r

 -- Delete from the right subtree of (P l e r)
 delPR l e  E           = P l e E                            -- Right sub-tree is empty
 delPR l e (N rl re rr) = case c re of
                          LT -> chkRP  l e (delNL rl re rr)
                          EQ -> chkRP  l e (subN  rl    rr)
                          GT -> chkRP  l e (delNR rl re rr)
 delPR l e (Z rl re rr) = case c re of
                          LT -> let r' = delZL rl re rr in r' `seq` P l e r'  -- height can't change
                          EQ -> chkRP' l e (subZL rl    rr)                   -- << But it can here
                          GT -> let r' = delZR rl re rr in r' `seq` P l e r'  -- height can't change
 delPR l e (P rl re rr) = case c re of
                          LT -> chkRP  l e (delPL rl re rr)
                          EQ -> chkRP  l e (subP  rl    rr)
                          GT -> chkRP  l e (delPR rl re rr)
-----------------------------------------------------------------------
------------------------- deleteFast Ends Here ------------------------
-----------------------------------------------------------------------

-- | General purpose function for popping elements from a sorted AVL tree.
-- An error is raised if a matching element is not found. The pair returned
-- by this function consists of the popped value and the modified tree.
--
-- Complexity: O(log n)
assertPop :: (e -> COrdering a) -> AVL e -> (a,AVL e)
assertPop c = genPop_ where
 genPop_  E        = error "assertPop: element not found."
 genPop_ (N l e r) = case popN l e r of UBT2(v,t) -> (v,t)
 genPop_ (Z l e r) = case popZ l e r of UBT2(v,t) -> (v,t)
 genPop_ (P l e r) = case popP l e r of UBT2(v,t) -> (v,t)

 ----------------------------- LEVEL 1 ---------------------------------
 --                       popN, popZ, popP                            --
 -----------------------------------------------------------------------

 -- Pop from (N l e r)
 popN l e r = case c e of
              Lt   -> popNL l e r
              Eq a -> let t = subN l r in t `seq` UBT2(a,t)
              Gt   -> popNR l e r

 -- Pop from (Z l e r)
 popZ l e r = case c e of
              Lt   -> popZL l e r
              Eq a -> let t = subZR l r in t `seq` UBT2(a,t)
              Gt   -> popZR l e r

 -- Pop from (P l e r)
 popP l e r = case c e of
              Lt   -> popPL l e r
              Eq a -> let t = subP l r in t `seq` UBT2(a,t)
              Gt   -> popPR l e r

 ----------------------------- LEVEL 2 ---------------------------------
 --                      popNL, popZL, popPL                          --
 --                      popNR, popZR, popPR                          --
 -----------------------------------------------------------------------

 -- Pop from the left subtree of (N l e r)
 popNL  E           _ _ = error "assertPop: element not found."     -- Left sub-tree is empty
 popNL (N ll le lr) e r = case c le of
                          Lt   -> case popNL ll le lr of
                                  UBT2(a,l_) -> let t = chkLN l_ e r in t `seq` UBT2(a,t)
                          Eq a -> let t = chkLN (subN ll lr) e r     in t `seq` UBT2(a,t)
                          Gt   -> case popNR ll le lr of
                                  UBT2(a,l_) -> let t = chkLN l_ e r in t `seq` UBT2(a,t)
 popNL (Z ll le lr) e r = case c le of
                          Lt   -> case popZL ll le lr of UBT2(a,l_) -> UBT2(a, N l_ e r)
                          Eq a -> let t = chkLN' (subZR ll lr) e r
                                                                     in t `seq` UBT2(a,t)
                          Gt   -> case popZR ll le lr of UBT2(a,l_) -> UBT2(a, N l_ e r)
 popNL (P ll le lr) e r = case c le of
                          Lt   -> case popPL ll le lr of
                                  UBT2(a,l_) -> let t = chkLN l_ e r in t `seq` UBT2(a,t)
                          Eq a -> let t = chkLN (subP ll lr) e r     in t `seq` UBT2(a,t)
                          Gt   -> case popPR ll le lr of
                                  UBT2(a,l_) -> let t = chkLN l_ e r in t `seq` UBT2(a,t)

 -- Pop from the right subtree of (N l e r)
 popNR _ _  E           = error "genPop.popNR: Bug!"             -- Impossible
 popNR l e (N rl re rr) = case c re of
                          Lt   -> case popNL rl re rr of
                                  UBT2(a,r_) -> let t = chkRN l e r_ in t `seq` UBT2(a,t)
                          Eq a -> let t = chkRN l e (subN rl rr)     in t `seq` UBT2(a,t)
                          Gt   -> case popNR rl re rr of
                                  UBT2(a,r_) -> let t = chkRN l e r_ in t `seq` UBT2(a,t)
 popNR l e (Z rl re rr) = case c re of
                          Lt   -> case popZL rl re rr of UBT2(a,r_) -> UBT2(a, N l e r_)
                          Eq a -> let t = chkRN' l e (subZL rl rr)
                                                                     in t `seq` UBT2(a,t)
                          Gt   -> case popZR rl re rr of UBT2(a,r_) -> UBT2(a, N l e r_)
 popNR l e (P rl re rr) = case c re of
                          Lt   -> case popPL rl re rr of
                                  UBT2(a,r_) -> let t = chkRN l e r_ in t `seq` UBT2(a,t)
                          Eq a -> let t = chkRN l e (subP rl rr)     in t `seq` UBT2(a,t)
                          Gt   -> case popPR rl re rr of
                                  UBT2(a,r_) -> let t = chkRN l e r_ in t `seq` UBT2(a,t)

 -- Pop from the left subtree of (Z l e r)
 popZL  E           _ _ = error "assertPop: element not found."  -- Left sub-tree is empty
 popZL (N ll le lr) e r = case c le of
                          Lt   -> case popNL ll le lr of
                                  UBT2(a,l_) -> let t = chkLZ l_ e r in t `seq` UBT2(a,t)
                          Eq a -> let t = chkLZ (subN ll lr) e r     in t `seq` UBT2(a,t)
                          Gt   -> case popNR ll le lr of
                                  UBT2(a,l_) -> let t = chkLZ l_ e r in t `seq` UBT2(a,t)
 popZL (Z ll le lr) e r = case c le of
                          Lt   -> case popZL ll le lr of UBT2(a,l_) -> UBT2(a, Z l_ e r)
                          Eq a -> let t = chkLZ' (subZR ll lr) e r
                                                                     in t `seq` UBT2(a,t)
                          Gt   -> case popZR ll le lr of UBT2(a,l_) -> UBT2(a, Z l_ e r)
 popZL (P ll le lr) e r = case c le of
                          Lt   -> case popPL ll le lr of
                                  UBT2(a,l_) -> let t = chkLZ l_ e r in t `seq` UBT2(a,t)
                          Eq a -> let t = chkLZ (subP ll lr) e r     in t `seq` UBT2(a,t)
                          Gt   -> case popPR ll le lr of
                                  UBT2(a,l_) -> let t = chkLZ l_ e r in t `seq` UBT2(a,t)

 -- Pop from the right subtree of (Z l e r)
 popZR _ _  E           = error "assertPop: element not found."    -- Right sub-tree is empty
 popZR l e (N rl re rr) = case c re of
                          Lt   -> case popNL rl re rr of
                                  UBT2(a,r_) -> let t = chkRZ l e r_ in t `seq` UBT2(a,t)
                          Eq a -> let t = chkRZ l e (subN rl rr)     in t `seq` UBT2(a,t)
                          Gt   -> case popNR rl re rr of
                                  UBT2(a,r_) -> let t = chkRZ l e r_ in t `seq` UBT2(a,t)
 popZR l e (Z rl re rr) = case c re of
                          Lt   -> case popZL rl re rr of UBT2(a,r_) -> UBT2(a, Z l e r_)
                          Eq a -> let t = chkRZ' l e (subZL rl rr)
                                                                     in t `seq` UBT2(a,t)
                          Gt   -> case popZR rl re rr of UBT2(a,r_) -> UBT2(a, Z l e r_)
 popZR l e (P rl re rr) = case c re of
                          Lt   -> case popPL rl re rr of
                                  UBT2(a,r_) -> let t = chkRZ l e r_ in t `seq` UBT2(a,t)
                          Eq a -> let t = chkRZ l e (subP rl rr)     in t `seq` UBT2(a,t)
                          Gt   -> case popPR rl re rr of
                                  UBT2(a,r_) -> let t = chkRZ l e r_ in t `seq` UBT2(a,t)

 -- Pop from the left subtree of (P l e r)
 popPL  E           _ _ = error "genPop.popPL: Bug!"             -- Impossible
 popPL (N ll le lr) e r = case c le of
                          Lt   -> case popNL ll le lr of
                                  UBT2(a,l_) -> let t = chkLP l_ e r in t `seq` UBT2(a,t)
                          Eq a -> let t = chkLP (subN ll lr) e r     in t `seq` UBT2(a,t)
                          Gt   -> case popNR ll le lr of
                                  UBT2(a,l_) -> let t = chkLP l_ e r in t `seq` UBT2(a,t)
 popPL (Z ll le lr) e r = case c le of
                          Lt   -> case popZL ll le lr of UBT2(a,l_) -> UBT2(a, P l_ e r)
                          Eq a -> let t = chkLP' (subZR ll lr) e r
                                                                     in t `seq` UBT2(a,t)
                          Gt   -> case popZR ll le lr of UBT2(a,l_) -> UBT2(a, P l_ e r)
 popPL (P ll le lr) e r = case c le of
                          Lt   -> case popPL ll le lr of
                                  UBT2(a,l_) -> let t = chkLP l_ e r in t `seq` UBT2(a,t)
                          Eq a -> let t = chkLP (subP ll lr) e r     in t `seq` UBT2(a,t)
                          Gt   -> case popPR ll le lr of
                                  UBT2(a,l_) -> let t = chkLP l_ e r in t `seq` UBT2(a,t)

 -- Pop from the right subtree of (P l e r)
 popPR _ _  E           = error "assertPop: element not found."                  -- Right sub-tree is empty
 popPR l e (N rl re rr) = case c re of
                          Lt   -> case popNL rl re rr of
                                  UBT2(a,r_) -> let t = chkRP l e r_ in t `seq` UBT2(a,t)
                          Eq a -> let t = chkRP l e (subN rl rr)     in t `seq` UBT2(a,t)
                          Gt   -> case popNR rl re rr of
                                  UBT2(a,r_) -> let t = chkRP l e r_ in t `seq` UBT2(a,t)
 popPR l e (Z rl re rr) = case c re of
                          Lt   -> case popZL rl re rr of UBT2(a,r_) -> UBT2(a, P l e r_)
                          Eq a -> let t = chkRP' l e (subZL rl rr)
                                                                     in t `seq` UBT2(a,t)
                          Gt   -> case popZR rl re rr of UBT2(a,r_) -> UBT2(a, P l e r_)
 popPR l e (P rl re rr) = case c re of
                          Lt   -> case popPL rl re rr of
                                  UBT2(a,r_) -> let t = chkRP l e r_ in t `seq` UBT2(a,t)
                          Eq a -> let t = chkRP l e (subP rl rr)     in t `seq` UBT2(a,t)
                          Gt   -> case popPR rl re rr of
                                  UBT2(a,r_) -> let t = chkRP l e r_ in t `seq` UBT2(a,t)
-----------------------------------------------------------------------
------------------------ assertPop Ends Here -----------------------
-----------------------------------------------------------------------

-- | Similar to 'genPop', but this function returns 'Nothing' if the search fails.
--
-- Complexity: O(log n)
tryPop :: (e -> COrdering a) -> AVL e -> Maybe (a,AVL e)
tryPop c t = case openPathWith c t of
                FullBP pth a -> let t' = deletePath pth t in t' `seq` Just (a,t')
                _            -> Nothing

-- | In this case the selector returns two values if a search succeeds.
-- If the second is @('Just' e)@ then the new value (@e@) is substituted in the same place in the tree.
-- If the second is 'Nothing' then the corresponding tree element is deleted.
-- This function raises an error if the search fails.
--
-- Complexity: O(log n)
assertPopMaybe :: (e -> COrdering (a,Maybe e)) -> AVL e -> (a,AVL e)
assertPopMaybe c t = case openPathWith c t of
                      FullBP pth (a,Just e ) -> let t' = writePath  pth e t in t' `seq` (a,t')
                      FullBP pth (a,Nothing) -> let t' = deletePath pth   t in t' `seq` (a,t')
                      _                      -> error "assertPopMaybe: element not found."

-- | Similar to 'assertPopMaybe', but returns 'Nothing' if the search fails.
--
-- Complexity: O(log n)
tryPopMaybe :: (e -> COrdering (a,Maybe e)) -> AVL e -> Maybe (a,AVL e)
tryPopMaybe c t = case openPathWith c t of
                     FullBP pth (a,Just e ) -> let t' = writePath  pth e t in t' `seq` Just (a,t')
                     FullBP pth (a,Nothing) -> let t' = deletePath pth   t in t' `seq` Just (a,t')
                     _                      -> Nothing


-- | A simpler version of 'assertPopMaybe'. The corresponding element is deleted if the second value
-- returned by the selector is 'True'. If it\'s 'False', the original tree is returned.
-- This function raises an error if the search fails.
--
-- Complexity: O(log n)
assertPopIf :: (e -> COrdering (a,Bool)) -> AVL e -> (a,AVL e)
assertPopIf c t = case openPathWith c t of
                     FullBP _   (a,False) -> (a,t)
                     FullBP pth (a,True ) -> let t' = deletePath pth t in t' `seq` (a,t')
                     _                    -> error "assertPopIf: element not found."

-- | Similar to 'genPopIf', but returns 'Nothing' if the search fails.
--
-- Complexity: O(log n)
tryPopIf :: (e -> COrdering (a,Bool)) -> AVL e -> Maybe (a,AVL e)
tryPopIf c t = case openPathWith c t of
                  FullBP _   (a,False) -> Just (a,t)
                  FullBP pth (a,True ) -> let t' = deletePath pth t in t' `seq` Just (a,t')
                  _                    -> Nothing

