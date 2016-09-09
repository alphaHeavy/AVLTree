{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Test.Utils
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- 'AVL' tree related test and verification utilities.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Test.Utils
        (-- * Correctness checking.
         isBalanced,checkHeight,isSorted,isSortedOK,
         -- * Test data generation.
         TestTrees,allAVL, allNonEmptyAVL, numTrees, flatAVL,
         -- * Exhaustive tests.
         exhaustiveTest,
         -- * Tree parameter utilities.
         minElements,maxElements,
         -- * Testing BinPath module.
         pathTree,
        ) where

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.List(map',asTreeLenL,asListL)

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- | Infinite test tree. Used for test purposes for BinPath module.
-- Value at each node is the path to that node.
pathTree :: AVL Int
pathTree = Z l 0 r where
 l = mapIt (\n -> 2*n+1) pathTree
 r = mapIt (\n -> 2*n+2) pathTree
 -- Need special lazy map for this recursive tree defn
 mapIt f (Z l' n r') = let n'= f n in n' `seq` Z (mapIt f l') n' (mapIt f r')
 mapIt _  _        = undefined

-- | Verify that a tree is height balanced and that the BF of each node is correct.
--
-- Complexity: O(n)
isBalanced :: AVL e -> Bool
isBalanced t = not (IS_TRUE(cH t EQL L(-1)))

-- | Verify that a tree is balanced and the BF of each node is correct.
-- Returns (Just height) if so, otherwise Nothing.
--
-- Complexity: O(n)
checkHeight :: AVL e -> Maybe Int
checkHeight t = let ht = cH t in if IS_TRUE(ht EQL L(-1))
                             then Nothing
                             else Just ASINT(ht)

-- Local utility, returns height if balanced, -1 if not
cH :: AVL e -> UINT
cH  E        = L(0)
cH (N l _ r) = cH_ L(1) l r -- (hr-hl) = 1
cH (Z l _ r) = cH_ L(0) l r -- (hr-hl) = 0
cH (P l _ r) = cH_ L(1) r l -- (hl-hr) = 1
cH_ :: UINT -> AVL e -> AVL e -> UINT
cH_ delta l r = let hl = cH l
                in if IS_TRUE(hl EQL L(-1))
                                   then hl
                                   else let hr = cH r
                                        in if IS_TRUE(hr EQL L(-1))
                                                           then hr
                                                           else if IS_TRUE(SUBINT(hr,hl) EQL delta)
                                                                                           then INCINT1(hr)                                                                                                                                                                else L(-1)

-- | Verify that a tree is sorted.
--
-- Complexity: O(n)
isSorted :: (e -> e -> Ordering) -> AVL e -> Bool
isSorted  c = isSorted' where
 isSorted'  E        = True
 isSorted' (N l e r) = isSorted'' l e r
 isSorted' (Z l e r) = isSorted'' l e r
 isSorted' (P l e r) = isSorted'' l e r
 isSorted''   l e r  = (isSortedU l e) && (isSortedL e r)
 -- Verify tree is sorted and rightmost element is less than an upper limit (ul)
 isSortedU  E        _  = True
 isSortedU (N l e r) ul = isSortedU' l e r ul
 isSortedU (Z l e r) ul = isSortedU' l e r ul
 isSortedU (P l e r) ul = isSortedU' l e r ul
 isSortedU'   l e r  ul = case c e ul of
                          LT -> (isSortedU l e) && (isSortedLU e r ul)
                          _  -> False
 -- Verify tree is sorted and leftmost element is greater than a lower limit (ll)
 isSortedL  _   E        = True
 isSortedL  ll (N l e r) = isSortedL' ll l e r
 isSortedL  ll (Z l e r) = isSortedL' ll l e r
 isSortedL  ll (P l e r) = isSortedL' ll l e r
 isSortedL' ll    l e r  = case c e ll of
                           GT -> (isSortedLU ll l e) && (isSortedL e r)
                           _  -> False
 -- Verify tree is sorted and leftmost element is greater than a lower limit (ll)
 -- and rightmost element is less than an upper limit (ul)
 isSortedLU  _   E        _  = True
 isSortedLU  ll (N l e r) ul = isSortedLU' ll l e r ul
 isSortedLU  ll (Z l e r) ul = isSortedLU' ll l e r ul
 isSortedLU  ll (P l e r) ul = isSortedLU' ll l e r ul
 isSortedLU' ll    l e r  ul = case c e ll of
                               GT -> case c e ul of
                                     LT -> (isSortedLU ll l e) && (isSortedLU e r ul)
                                     _  -> False
                               _  -> False
-- isSorted ends --
-------------------

-- | Verify that a tree is sorted, height balanced and the BF of each node is correct.
--
-- Complexity: O(n)
isSortedOK :: (e -> e -> Ordering) -> AVL e -> Bool
isSortedOK c t = (isBalanced t) && (isSorted c t)

-- | AVL Tree test data. Each element of a the list is a pair consisting of a height,
-- and list of all possible sorted trees of the same height, paired with their sizes.
-- The elements of each tree of size s are 0..s-1.
type TestTrees = [(Int, [(AVL Int, Int)])]

-- | All possible sorted AVL trees.
allAVL :: TestTrees
allAVL = p0 : p1 : moreTrees p1 p0 where
  p0 = (0, [(E      , 0)])  -- All possible trees of height 0
  p1 = (1, [(Z E 0 E, 1)])  -- All possible trees of height 1
  -- Generate more trees of height N, from existing trees of height N-1 and N-2
  moreTrees :: (Int, [(AVL Int, Int)]) -> (Int, [(AVL Int, Int)]) -> [(Int, [(AVL Int, Int)])]
  moreTrees pN1@(hN1, tpsN1)    -- Height N-1
                (_  , tpsN2) =  -- Height N-2
    let hN0  = hN1 + 1          -- Height N
        tsN0 = interleave (interleave [newTree P l r | r <- tpsN2 , l <- tpsN1]  -- BF=+1
                                      [newTree N l r | l <- tpsN2 , r <- tpsN1]) -- BF=-1
                                      [newTree Z l r | l <- tpsN1 , r <- tpsN1]  -- BF= 0
        pN0  = (hN0,tsN0)
    in  hN0 `seq` pN0 : moreTrees pN0 pN1
  -- Generate a new (tree,size) pair using the supplied constructor
  newTree con (l,sizel) (r,sizer) =
    let rootEl   = sizel            -- Value of new root element
        addRight = sizel+1          -- Offset to add to elements of right sub-tree
        newSize  = addRight + sizer -- Size of the new tree
        r'       = map' (addRight+) r
        t        = r' `seq` con l rootEl r'
    in newSize `seq` t `seq` (t, newSize)
  -- interleave two lists (until one or other is [])
  interleave [] ys         = ys
  interleave xs []         = xs
  interleave (x:xs) (y:ys) = (x:y:interleave xs ys)


-- | Same as 'allAVL', but excluding the empty tree (of height 0).
allNonEmptyAVL :: TestTrees
allNonEmptyAVL = tail allAVL

-- | Returns the number of possible AVL trees of a given height.
--
-- Behaves as if defined..
--
-- > numTrees h = (\(_,xs) -> length xs) (allAVL !! h)
--
-- and satisfies this recurrence relation..
--
-- @
-- numTrees 0 = 1
-- numTrees 1 = 1
-- numTrees h = (2*(numTrees (h-2)) + (numTrees (h-1))) * (numTrees (h-1))
-- @
numTrees :: Int -> Integer
numTrees 0 = 1
numTrees 1 = 1
numTrees n = numTrees' 1 1 n where
 numTrees' n1 n2 2 = (2*n2 + n1)*n1
 numTrees' n1 n2 m = numTrees' ((2*n2 + n1)*n1) n1 (m-1)

-- | Apply the test function to each AVL tree in the TestTrees argument, and report
-- progress as test proceeds. The first two arguments of the test function are
-- tree height and size respectively.
exhaustiveTest :: (Int -> Int -> AVL Int -> Bool) -> TestTrees -> IO ()
exhaustiveTest f xs = mapM_ test xs where
 test (h,tps) = do putStr "Tree Height    : " >> print h
                   putStr "Number Of Trees: " >> print (numTrees h)
                   mapM_ test' tps
                   putStrLn "Done."
                where test' (t,s) = if f h s t then return () -- putStr "."
                                               else error $ show $ asListL t -- Temporary Hack

-- | Generates a flat AVL tree of n elements [0..n-1].
flatAVL :: Int -> AVL Int
flatAVL n = asTreeLenL n [0..n-1]

-- | Detetermine the minimum number of elements in an AVL tree of given height.
-- This function satisfies this recurrence relation..
--
-- @
-- minElements 0 = 0
-- minElements 1 = 1
-- minElements h = 1 + minElements (h-1) + minElements (h-2)
--            -- = Some weird expression involving the golden ratio
-- @
minElements :: Int -> Integer
minElements 0 = 0
minElements 1 = 1
minElements h = minElements' 0 1 h where
 minElements' n1 n2 2 = 1 + n1 + n2
 minElements' n1 n2 m = minElements' n2 (1 + n1 + n2) (m-1)

-- | Detetermine the maximum number of elements in an AVL tree of given height.
-- This function satisfies this recurrence relation..
--
-- @
-- maxElements 0 = 0
-- maxElements h = 1 + 2 * maxElements (h-1) -- = 2^h-1
-- @
maxElements :: Int -> Integer
maxElements 0 = 0
maxElements h = maxElements' 0 h where
 maxElements' n1 1 = 1 + 2*n1
 maxElements' n1 m = maxElements' (1 + 2*n1) (m-1)
