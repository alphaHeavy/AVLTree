{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Types
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- AVL Tree data type definition and a few simple utility functions.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Types
        ( -- * Types.
         AVL(..),

         -- * Simple AVL related utilities.
         empty,isEmpty,isNonEmpty,singleton,pair,tryGetSingleton,

        ) where

import Prelude -- so haddock finds the symbols there

import Data.Typeable
#if __GLASGOW_HASKELL__ > 604
import Data.Foldable
import Data.Monoid
#endif

-- | AVL tree data type.
--
-- The balance factor (BF) of an 'AVL' tree node is defined as the difference between the height of
-- the left and right sub-trees. An 'AVL' tree is ALWAYS height balanced, such that |BF| <= 1.
-- The functions in this library ("Data.Tree.AVL") are designed so that they never construct
-- an unbalanced tree (well that's assuming they're not broken). The 'AVL' tree type defined here
-- has the BF encoded the constructors.
--
-- Some functions in this library return 'AVL' trees that are also \"flat\", which (in the context
-- of this library) means that the sizes of left and right sub-trees differ by at most one and
-- are also flat. Flat sorted trees should give slightly shorter searches than sorted trees which
-- are merely height balanced. Whether or not flattening is worth the effort depends on the number
-- of times the tree will be searched and the cost of element comparison.
--
-- In cases where the tree elements are sorted, all the relevant 'AVL' functions follow the
-- convention that the leftmost tree element is least and the rightmost tree element is
-- the greatest. Bear this in mind when defining general comparison functions. It should
-- also be noted that all functions in this library for sorted trees require that the tree
-- does not contain multiple elements which are \"equal\" (according to whatever criterion
-- has been used to sort the elements).
--
-- It is important to be consistent about argument ordering when defining general purpose
-- comparison functions (or selectors) for searching a sorted tree, such as ..
--
-- @
-- myComp  :: (k -> e -> Ordering)
-- -- or..
-- myCComp :: (k -> e -> COrdering a)
-- @
--
-- In these cases the first argument is the search key and the second argument is an element of
-- the 'AVL' tree. For example..
--
-- @
-- key \`myCComp\` element -> Lt  implies key < element, proceed down the left sub-tree
-- key \`myCComp\` element -> Gt  implies key > element, proceed down the right sub-tree
-- @
--
-- This convention is same as that used by the overloaded 'compare' method from 'Ord' class.
--
-- Controlling Strictness.
--
-- The 'AVL' tree data type is declared as non-strict in all it's fields,
-- but all the functions in this library behave as though it is strict in its
-- recursive fields (left and right sub-trees). Strictness in the element field is
-- controlled either by using the strict variants of functions (defined in this library
-- where appropriate), or using strict variants of the combinators defined in "Data.COrdering",
-- or using 'seq' etc. in your own code (in any combining comparisons you define, for example).
--
-- The 'Eq' and 'Ord' instances.
--
-- Begining with version 3.0 these are now derived, and hence are defined in terms of
-- strict structural equality, rather than observational equivalence. The reason for
-- this change is that the observational equivalence abstraction was technically breakable
-- with the exposed API. But since this change, some functions which were previously
-- considered unsafe have become safe to expose (those that measure tree height, for example).
--
-- The 'Read' and 'Show' instances.
--
-- Begining with version 4.0 these are now derived to ensure consistency with 'Eq' instance.
-- (Show now reveals the exact tree structure).
--

data AVL e = E                      -- ^ Empty Tree
           | N (AVL e) e (AVL e)    -- ^ BF=-1 (right height > left height)
           | Z (AVL e) e (AVL e)    -- ^ BF= 0
           | P (AVL e) e (AVL e)    -- ^ BF=+1 (left height > right height)
           deriving(Eq,Ord,Show,Read,Typeable)

#ifndef __GLASGOW_HASKELL__
-- A Typeable instance (not needed by ghc, but Haddock fails to document this instance)
instance Typeable e => Typeable (AVL e) where
 typeOf = typeOfDefault
#endif

#if __GLASGOW_HASKELL__ > 604
instance Foldable AVL where
  foldMap _f E = mempty
  foldMap f (N l v r) = foldMap f l `mappend` f v `mappend` foldMap f r
  foldMap f (Z l v r) = foldMap f l `mappend` f v `mappend` foldMap f r
  foldMap f (P l v r) = foldMap f l `mappend` f v `mappend` foldMap f r
#endif

-- | The empty AVL tree.
{-# INLINE empty #-}
empty :: AVL e
empty = E

-- | Returns 'True' if an AVL tree is empty.
--
-- Complexity: O(1)
isEmpty :: AVL e -> Bool
isEmpty E = True
isEmpty _ = False
{-# INLINE isEmpty #-}

-- | Returns 'True' if an AVL tree is non-empty.
--
-- Complexity: O(1)
isNonEmpty :: AVL e -> Bool
isNonEmpty E = False
isNonEmpty _ = True
{-# INLINE isNonEmpty #-}

-- | Creates an AVL tree with just one element.
--
-- Complexity: O(1)
singleton :: e -> AVL e
singleton e = Z E e E
{-# INLINE singleton #-}

-- | Create an AVL tree of two elements, occuring in same order as the arguments.
pair :: e -> e -> AVL e
pair e0 e1 = P (Z E e0 E) e1 E
{-# INLINE pair #-}

-- | If the AVL tree is a singleton (has only one element @e@) then this function returns @('Just' e)@.
-- Otherwise it returns Nothing.
--
-- Complexity: O(1)
tryGetSingleton :: AVL e -> Maybe e
tryGetSingleton (Z E e _) = Just e -- Right subtree must be E too, but no need to waste time checking
tryGetSingleton _         = Nothing
{-# INLINE tryGetSingleton #-}
