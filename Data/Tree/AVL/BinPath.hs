{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Internals.BinPath
-- Copyright   :  (c) Adrian Hey 2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides a cheap but extremely limited and dangerous alternative
-- to using the Zipper. A BinPath provides a way of finding a particular element
-- in an AVL tree again without doing any comparisons. But a BinPath is ONLY VALID
-- IF THE TREE SHAPE DOES NOT CHANGE.
--
-- See the BAVL type in Data.Tree.AVL.Zipper module for a safer wrapper round these
-- functions.
-----------------------------------------------------------------------------
module Data.Tree.AVL.BinPath
        (BinPath(..),findFullPath,findEmptyPath,openPath,openPathWith,readPath,writePath,insertPath,
        --  These are used by deletePath, which currently resides in Data.Tree.AVL.Internals.DelUtils
        sel,goL,goR,
        ) where
-- N.B. The deletePath function should really be here too, but has been put
-- in Data.Tree.AVL.Internals.DelUtils instead because deletion is a tangled web of circular
-- depencency.

import Data.Tree.AVL.Types(AVL(..))
import Data.COrdering

#if __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"

-- Test path LSB
bit0 :: Int# -> Bool
{-# INLINE bit0 #-}
bit0 p = IS_TRUE(word2Int# (and# (int2Word# p) (int2Word# 1#)) ==# 1#)

-- A pseudo comparison..
-- N.B. If the path was bit reversed, this could be a straight comparison.??
sel :: Int# -> Ordering
{-# INLINE sel #-}
sel p = if IS_TRUE(p ==# 0#)
                    then EQ
                    else if bit0 p then LT -- Left  if Bit 0 == 1
                                   else GT -- Right if Bit 0 == 0


-- Modify path for entering left subtree
goL :: Int# -> Int#
{-# INLINE goL #-}
goL p = iShiftRL# p 1#

-- Modify path for entering right subtree
goR :: Int# -> Int#
{-# INLINE goR #-}
goR p = iShiftRL# (p -# 1#) 1#

#else
#include "h98defs.h"
import Data.Bits((.&.),shiftL)

-- A pseudo comparison..
-- N.B. If the path was bit reversed, this could be a straight comparison.??
sel :: Int -> Ordering
{-# INLINE sel #-}
sel p = if p == 0 then EQ
                  else if bit0 p then LT -- Left  if Bit 0 == 1
                                 else GT -- Right if Bit 0 == 0
bit0 :: Int -> Bool
{-# INLINE bit0 #-}
bit0 p = (p .&. 1) == 1

-- Modify path for entering left subtree
goL :: Int -> Int
{-# INLINE goL #-}
goL p = shiftL p 1

-- Modify path for entering right subtree
goR :: Int -> Int
{-# INLINE goR #-}
goR p = shiftL (p-1) 1
#endif

-- | A BinPath is full if the search succeeded, empty otherwise.
data BinPath a = FullBP   {-# UNPACK #-} !UINT a -- Found
               | EmptyBP  {-# UNPACK #-} !UINT   -- Not Found

{-------------------------------------------------------------------------------------------
                                        Notes:
--------------------------------------------------------------------------------------------
The Binary paths are based on an indexing scheme that:
 1- Uniquely identifies each tree node
 2- Provides a simple algorithm for path generation.
 3- Provides a simple algorithm to locate a node in the tree, given it's path.

Imagine an infinite Binary Tree, with nodes indexed as follows:

          _____00_____             <- d=1
         /            \
      _01_            _02_         <- d=2
     /    \          /    \
   03      05      04      06      <- d=4
  /  \    /  \    /  \    /  \
 07  11  09  13  08  12  10  14    <- d=8
 <-------- More Layers ------->

To generate the node index (path) as we move down the tree we..
 1- Initialise index (i) to 0, and a parameter (d) to 1
 2- If we've arrived where we want, output i.
 3- Either Move left:  i <- i+d,  d <- 2d, goto 2
    or     Move right: i <- i+2d, d <- 2d, goto 2

To find a node, given its index (path) i, we..
 1- If i=0 then stop, we've arrived.
 2- If i is odd then move left , i <- (i-1)>>1,  goto 1  -- (i-1)>>1 =  i>>1     if i is odd
                else move right, i <- (i-1)>>1,  goto 1  -- (i-1)>>1 = (i>>1)-1  if i is even
Examples:
 i=05: (left ,i<-2):(right,i<-0):(stop)
 i=12: (right,i<-5):(left ,i<-2):(right,i<-0):(stop)

See also: pathTree in Data.Tree.AVL.Test.Utils for recursive implementation of the indexing scheme.
--------------------------------------------------------------------------------------------}

-- | Find the path to a AVL tree element, returns -1 (invalid path) if element not found
--
-- Complexity: O(log n)
findFullPath :: (e -> Ordering) -> AVL e -> UINT
-- ?? What about strictness if UINT is boxed (i.e. non-ghc)?
findFullPath c t = find L(1) L(0) t where
 find  _ _  E        = L(-1)
 find  d i (N l e r) = find' d i l e r
 find  d i (Z l e r) = find' d i l e r
 find  d i (P l e r) = find' d i l e r
 find' d i    l e r  = case c e of
                       LT    -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d ) l
                       EQ    -> i
                       GT    -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d_) r -- d_ = 2d

-- | Find the path to a non-existant AVL tree element, returns -1 (invalid path) if element is found
--
-- Complexity: O(log n)
findEmptyPath :: (e -> Ordering) -> AVL e -> UINT
-- ?? What about strictness if UINT is boxed (i.e. non-ghc)?
findEmptyPath c t = find L(1) L(0) t where
 find  _ i  E        = i
 find  d i (N l e r) = find' d i l e r
 find  d i (Z l e r) = find' d i l e r
 find  d i (P l e r) = find' d i l e r
 find' d i    l e r  = case c e of
                       LT    -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d ) l
                       EQ    -> L(-1)
                       GT    -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d_) r -- d_ = 2d

-- | Get the BinPath of an element using the supplied selector.
--
-- Complexity: O(log n)
openPath :: (e -> Ordering) -> AVL e -> BinPath e
openPath c t = find L(1) L(0) t where
 find  _ i  E        = EmptyBP i
 find  d i (N l e r) = find' d i l e r
 find  d i (Z l e r) = find' d i l e r
 find  d i (P l e r) = find' d i l e r
 find' d i    l e r  = case c e of
                       LT    -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d ) l
                       EQ    -> FullBP i e
                       GT    -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d_) r -- d_ = 2d

-- | Get the BinPath of an element using the supplied (combining) selector.
--
-- Complexity: O(log n)
openPathWith :: (e -> COrdering a) -> AVL e -> BinPath a
openPathWith c t = find L(1) L(0) t where
 find  _ i  E        = EmptyBP i
 find  d i (N l e r) = find' d i l e r
 find  d i (Z l e r) = find' d i l e r
 find  d i (P l e r) = find' d i l e r
 find' d i    l e r  = case c e of
                       Lt   -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d ) l
                       Eq a -> FullBP i a
                       Gt   -> let d_ = ADDINT(d,d) in find d_ ADDINT(i,d_) r -- d_ = 2d

-- | Overwrite a tree element. Assumes the path bits were extracted from 'FullBP' constructor.
-- Raises an error if the path leads to an empty tree.
--
-- N.B This operation does not change tree shape (no insertion occurs).
--
-- Complexity: O(log n)
writePath :: UINT -> e -> AVL e -> AVL e
writePath i0 e' t = wp i0 t where
 wp L(0)  E        = error "writePath: Bug0" -- Needed to force strictness in path
 wp L(0) (N l _ r) = N l e' r
 wp L(0) (Z l _ r) = Z l e' r
 wp L(0) (P l _ r) = P l e' r
 wp _  E        = error "writePath: Bug1"
 wp i (N l e r) = if bit0 i then let l' = wp (goL i) l in l' `seq` N l' e r
                            else let r' = wp (goR i) r in r' `seq` N l  e r'
 wp i (Z l e r) = if bit0 i then let l' = wp (goL i) l in l' `seq` Z l' e r
                            else let r' = wp (goR i) r in r' `seq` Z l  e r'
 wp i (P l e r) = if bit0 i then let l' = wp (goL i) l in l' `seq` P l' e r
                            else let r' = wp (goR i) r in r' `seq` P l  e r'

-- | Read a tree element. Assumes the path bits were extracted from 'FullBP' constructor.
-- Raises an error if the path leads to an empty tree.
--
-- Complexity: O(log n)
readPath :: UINT -> AVL e -> e
readPath L(0)  E        = error "readPath: Bug0" -- Needed to force strictness in path
readPath L(0) (N _ e _) = e
readPath L(0) (Z _ e _) = e
readPath L(0) (P _ e _) = e
readPath _     E        = error "readPath: Bug1"
readPath i    (N l _ r) = readPath_ i l r
readPath i    (Z l _ r) = readPath_ i l r
readPath i    (P l _ r) = readPath_ i l r
readPath_ :: UINT -> AVL e -> AVL e -> e
readPath_ i l r = if bit0 i then readPath (goL i) l
                            else readPath (goR i) r

-- | Inserts a new tree element. Assumes the path bits were extracted from a 'EmptyBP' constructor.
-- This function replaces the first Empty node it encounters with the supplied value, regardless
-- of the current path bits (which are not checked). DO NOT USE THIS FOR REPLACING ELEMENTS ALREADY
-- PRESENT IN THE TREE (use 'writePath' for this).
--
-- Complexity: O(log n)
insertPath :: UINT -> e -> AVL e -> AVL e
insertPath i0 e0 t = put i0 t where
 ----------------------------- LEVEL 0 ---------------------------------
 --                             put                                   --
 -----------------------------------------------------------------------
 put _  E        = Z E e0 E
 put i (N l e r) = putN i l e  r
 put i (Z l e r) = putZ i l e  r
 put i (P l e r) = putP i l e  r

 ----------------------------- LEVEL 1 ---------------------------------
 --                       putN, putZ, putP                            --
 -----------------------------------------------------------------------
 -- Put in (N l e r), BF=-1  , (never returns P)
 putN i l e r = if bit0 i then putNL i l e r  -- put in L subtree
                          else putNR i l e r  -- put in R subtree

 -- Put in (Z l e r), BF= 0
 putZ i l e r = if bit0 i then putZL i l e r  -- put in L subtree
                          else putZR i l e r  -- put in R subtree

 -- Put in (P l e r), BF=+1 , (never returns N)
 putP i l e r = if bit0 i then putPL i l e r  -- put in L subtree
                          else putPR i l e r  -- put in R subtree

 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNL, putZL, putPL                          --
 --                      putNR, putZR, putPR                          --
 -----------------------------------------------------------------------

 -- (putNL l e r): Put in L subtree of (N l e r), BF=-1 (Never requires rebalancing) , (never returns P)
 {-# INLINE putNL #-}
 putNL _ E            e r = Z (Z E e0 E) e r               -- L subtree empty, H:0->1, parent BF:-1-> 0
 putNL i (N ll le lr) e r = let l' = putN (goL i) ll le lr -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                            in l' `seq` N l' e r
 putNL i (P ll le lr) e r = let l' = putP (goL i) ll le lr -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                            in l' `seq` N l' e r
 putNL i (Z ll le lr) e r = let l' = putZ (goL i) ll le lr -- L subtree BF= 0, so need to look for changes
                            in case l' of
                            E       -> error "insertPath: Bug0" -- impossible
                            Z _ _ _ -> N l' e r         -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                            _       -> Z l' e r         -- L subtree BF:0->+/-1, H:h->h+1, parent BF:-1-> 0

 -- (putZL l e r): Put in L subtree of (Z l e r), BF= 0  (Never requires rebalancing) , (never returns N)
 {-# INLINE putZL #-}
 putZL _  E           e r = P (Z E e0 E) e r               -- L subtree        H:0->1, parent BF: 0->+1
 putZL i (N ll le lr) e r = let l' = putN (goL i) ll le lr -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                            in l' `seq` Z l' e r
 putZL i (P ll le lr) e r = let l' = putP (goL i) ll le lr -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                            in l' `seq` Z l' e r
 putZL i (Z ll le lr) e r = let l' = putZ (goL i) ll le lr -- L subtree BF= 0, so need to look for changes
                            in case l' of
                            E       -> error "insertPath: Bug1" -- impossible
                            Z _ _ _ -> Z l' e r         -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                            _       -> P l' e r         -- L subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->+1

 -- (putZR l e r): Put in R subtree of (Z l e r), BF= 0 (Never requires rebalancing) , (never returns P)
 {-# INLINE putZR #-}
 putZR _ l e E            = N l e (Z E e0 E)               -- R subtree        H:0->1, parent BF: 0->-1
 putZR i l e (N rl re rr) = let r' = putN (goR i) rl re rr -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                            in r' `seq` Z l e r'
 putZR i l e (P rl re rr) = let r' = putP (goR i) rl re rr -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                            in r' `seq` Z l e r'
 putZR i l e (Z rl re rr) = let r' = putZ (goR i) rl re rr -- R subtree BF= 0, so need to look for changes
                            in case r' of
                            E       -> error "insertPath: Bug2" -- impossible
                            Z _ _ _ -> Z l e r'         -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                            _       -> N l e r'         -- R subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->-1

 -- (putPR l e r): Put in R subtree of (P l e r), BF=+1 (Never requires rebalancing) , (never returns N)
 {-# INLINE putPR #-}
 putPR _ l e  E           = Z l e (Z E e0 E)               -- R subtree empty, H:0->1,     parent BF:+1-> 0
 putPR i l e (N rl re rr) = let r' = putN (goR i) rl re rr -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                            in r' `seq` P l e r'
 putPR i l e (P rl re rr) = let r' = putP (goR i) rl re rr -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                            in r' `seq` P l e r'
 putPR i l e (Z rl re rr) = let r' = putZ (goR i) rl re rr -- R subtree BF= 0, so need to look for changes
                            in case r' of
                            E       -> error "insertPath: Bug3" -- impossible
                            Z _ _ _ -> P l e r'         -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                            _       -> Z l e r'         -- R subtree BF:0->+/-1, H:h->h+1, parent BF:+1-> 0

      -------- These 2 cases (NR and PL) may need rebalancing if they go to LEVEL 3 ---------

 -- (putNR l e r): Put in R subtree of (N l e r), BF=-1 , (never returns P)
 {-# INLINE putNR #-}
 putNR _ _ _ E            = error "insertPath: Bug4"           -- impossible if BF=-1
 putNR i l e (N rl re rr) = let r' = putN (goR i) rl re rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                            in r' `seq` N l e r'
 putNR i l e (P rl re rr) = let r' = putP (goR i) rl re rr  -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                            in r' `seq` N l e r'
 putNR i l e (Z rl re rr) = let i' = goR i in if bit0 i' then putNRL i' l e rl re rr -- RL (never returns P)
                                                         else putNRR i' l e rl re rr -- RR (never returns P)

 -- (putPL l e r): Put in L subtree of (P l e r), BF=+1 , (never returns N)
 {-# INLINE putPL #-}
 putPL _  E           _ _ = error "insertPath: Bug5"           -- impossible if BF=+1
 putPL i (N ll le lr) e r = let l' = putN (goL i) ll le lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                            in l' `seq` P l' e r
 putPL i (P ll le lr) e r = let l' = putP (goL i) ll le lr  -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                            in l' `seq` P l' e r
 putPL i (Z ll le lr) e r = let i' = goL i in if bit0 i' then putPLL i' ll le lr e r -- LL (never returns N)
                                                         else putPLR i' ll le lr e r -- LR (never returns N)

 ----------------------------- LEVEL 3 ---------------------------------
 --                        putNRR, putPLL                             --
 --                        putNRL, putPLR                             --
 -----------------------------------------------------------------------

 -- (putNRR l e rl re rr): Put in RR subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRR #-}
 putNRR _ l e rl re  E              = Z (Z l e rl) re (Z E e0 E)         -- l and rl must also be E, special CASE RR!!
 putNRR i l e rl re (N rrl rre rrr) = let rr' = putN (goR i) rrl rre rrr -- RR subtree BF<>0, H:h->h, so no change
                                      in rr' `seq` N l e (Z rl re rr')
 putNRR i l e rl re (P rrl rre rrr) = let rr' = putP (goR i) rrl rre rrr -- RR subtree BF<>0, H:h->h, so no change
                                      in rr' `seq` N l e (Z rl re rr')
 putNRR i l e rl re (Z rrl rre rrr) = let rr' = putZ (goR i) rrl rre rrr -- RR subtree BF= 0, so need to look for changes
                                      in case rr' of
                                      E       -> error "insertPath: Bug6"   -- impossible
                                      Z _ _ _ -> N l e (Z rl re rr')     -- RR subtree BF: 0-> 0, H:h->h, so no change
                                      _       -> Z (Z l e rl) re rr'     -- RR subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE RR !!

 -- (putPLL ll le lr e r): Put in LL subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLL #-}
 putPLL _  E le lr e r              = Z (Z E e0 E) le (Z lr e r)         -- r and lr must also be E, special CASE LL!!
 putPLL i (N lll lle llr) le lr e r = let ll' = putN (goL i) lll lle llr -- LL subtree BF<>0, H:h->h, so no change
                                      in ll' `seq` P (Z ll' le lr) e r
 putPLL i (P lll lle llr) le lr e r = let ll' = putP (goL i) lll lle llr -- LL subtree BF<>0, H:h->h, so no change
                                      in ll' `seq` P (Z ll' le lr) e r
 putPLL i (Z lll lle llr) le lr e r = let ll' = putZ (goL i) lll lle llr -- LL subtree BF= 0, so need to look for changes
                                      in case ll' of
                                      E       -> error "insertPath: Bug7"  -- impossible
                                      Z _ _ _ -> P (Z ll' le lr) e r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                      _       -> Z ll' le (Z lr e r) -- LL subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE LL !!

 -- (putNRL l e rl re rr): Put in RL subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRL #-}
 putNRL _ l e  E              re rr = Z (Z l e E) e0 (Z E re rr)          -- l and rr must also be E, special CASE LR !!
 putNRL i l e (N rll rle rlr) re rr = let rl' = putN (goL i) rll rle rlr  -- RL subtree BF<>0, H:h->h, so no change
                                      in rl' `seq` N l e (Z rl' re rr)
 putNRL i l e (P rll rle rlr) re rr = let rl' = putP (goL i) rll rle rlr  -- RL subtree BF<>0, H:h->h, so no change
                                      in rl' `seq` N l e (Z rl' re rr)
 putNRL i l e (Z rll rle rlr) re rr = let rl' = putZ (goL i) rll rle rlr  -- RL subtree BF= 0, so need to look for changes
                                      in case rl' of
                                      E                -> error "insertPath: Bug8" -- impossible
                                      Z _    _    _    -> N l e (Z rl' re rr)                -- RL subtree BF: 0-> 0, H:h->h, so no change
                                      N rll' rle' rlr' -> Z (P l e rll') rle' (Z rlr' re rr) -- RL subtree BF: 0->-1, SO.. CASE RL(1) !!
                                      P rll' rle' rlr' -> Z (Z l e rll') rle' (N rlr' re rr) -- RL subtree BF: 0->+1, SO.. CASE RL(2) !!

 -- (putPLR ll le lr e r): Put in LR subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLR #-}
 putPLR _ ll le  E              e r = Z (Z ll le E) e0 (Z E e r)          -- r and ll must also be E, special CASE LR !!
 putPLR i ll le (N lrl lre lrr) e r = let lr' = putN (goR i) lrl lre lrr  -- LR subtree BF<>0, H:h->h, so no change
                                      in lr' `seq` P (Z ll le lr') e r
 putPLR i ll le (P lrl lre lrr) e r = let lr' = putP (goR i) lrl lre lrr  -- LR subtree BF<>0, H:h->h, so no change
                                      in lr' `seq` P (Z ll le lr') e r
 putPLR i ll le (Z lrl lre lrr) e r = let lr' = putZ (goR i) lrl lre lrr  -- LR subtree BF= 0, so need to look for changes
                                      in case lr' of
                                      E                -> error "insertPath: Bug9" -- impossible
                                      Z _    _    _    -> P (Z ll le lr') e r                -- LR subtree BF: 0-> 0, H:h->h, so no change
                                      N lrl' lre' lrr' -> Z (P ll le lrl') lre' (Z lrr' e r) -- LR subtree BF: 0->-1, SO.. CASE LR(2) !!
                                      P lrl' lre' lrr' -> Z (Z ll le lrl') lre' (N lrr' e r) -- LR subtree BF: 0->+1, SO.. CASE LR(1) !!
-----------------------------------------------------------------------
----------------------- insertPath Ends Here --------------------------
-----------------------------------------------------------------------

