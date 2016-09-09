-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Read
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.Read
(-- * Reading from AVL trees

 -- ** Reading from extreme left or right
 assertReadL,tryReadL,
 assertReadR,tryReadR,

 -- ** Reading from /sorted/ AVL trees
 assertRead,tryRead,tryReadMaybe,defaultRead,

 -- ** Simple searches of /sorted/ AVL trees
 contains,
) where

import Prelude -- so haddock finds the symbols there

import Data.COrdering
import Data.Tree.AVL.Types(AVL(..))

-- | Read the leftmost element from a /non-empty/ tree. Raises an error if the tree is empty.
-- If the tree is sorted this will return the least element.
--
-- Complexity: O(log n)
assertReadL :: AVL e -> e
assertReadL  E        = error "assertReadL: Empty tree."
assertReadL (N l e _) = readLE  l e
assertReadL (Z l e _) = readLE  l e
assertReadL (P l _ _) = readLNE l     -- BF=+1, so left sub-tree cannot be empty.

-- | Similar to 'assertReadL' but returns 'Nothing' if the tree is empty.
--
-- Complexity: O(log n)
tryReadL :: AVL e -> Maybe e
tryReadL  E        = Nothing
tryReadL (N l e _) = Just $! readLE  l e
tryReadL (Z l e _) = Just $! readLE  l e
tryReadL (P l _ _) = Just $! readLNE l     -- BF=+1, so left sub-tree cannot be empty.

-- Local utilities for the above
readLNE :: AVL e -> e
readLNE  E        = error "readLNE: Bug."
readLNE (N l e _) = readLE  l e
readLNE (Z l e _) = readLE  l e
readLNE (P l _ _) = readLNE l     -- BF=+1, so left sub-tree cannot be empty.
readLE :: AVL e -> e -> e
readLE  E        e = e
readLE (N l e _) _ = readLE  l e
readLE (Z l e _) _ = readLE  l e
readLE (P l _ _) _ = readLNE l  -- BF=+1, so left sub-tree cannot be empty.


-- | Read the rightmost element from a /non-empty/ tree. Raises an error if the tree is empty.
-- If the tree is sorted this will return the greatest element.
--
-- Complexity: O(log n)
assertReadR :: AVL e -> e
assertReadR  E        = error "assertReadR: Empty tree."
assertReadR (P _ e r) = readRE  r e
assertReadR (Z _ e r) = readRE  r e
assertReadR (N _ _ r) = readRNE r     -- BF=-1, so right sub-tree cannot be empty.

-- | Similar to 'assertReadR' but returns 'Nothing' if the tree is empty.
--
-- Complexity: O(log n)
tryReadR :: AVL e -> Maybe e
tryReadR  E        = Nothing
tryReadR (P _ e r) = Just $! readRE  r e
tryReadR (Z _ e r) = Just $! readRE  r e
tryReadR (N _ _ r) = Just $! readRNE r   -- BF=-1, so right sub-tree cannot be empty.

-- Local utilities for the above
readRNE :: AVL e -> e
readRNE  E        = error "readRNE: Bug."
readRNE (P _ e r) = readRE  r e
readRNE (Z _ e r) = readRE  r e
readRNE (N _ _ r) = readRNE r     -- BF=-1, so right sub-tree cannot be empty.
readRE :: AVL e -> e -> e
readRE  E        e = e
readRE (P _ e r) _ = readRE  r e
readRE (Z _ e r) _ = readRE  r e
readRE (N _ _ r) _ = readRNE r  -- BF=-1, so right sub-tree cannot be empty.


-- | General purpose function to perform a search of a sorted tree, using the supplied selector.
-- This function raises a error if the search fails.
--
-- Complexity: O(log n)
assertRead :: AVL e -> (e -> COrdering a) -> a
assertRead t c = genRead' t where
 genRead'  E        = error "assertRead failed."
 genRead' (N l e r) = genRead'' l e r
 genRead' (Z l e r) = genRead'' l e r
 genRead' (P l e r) = genRead'' l e r
 genRead''   l e r  = case c e of
                      Lt   -> genRead' l
                      Eq a -> a
                      Gt   -> genRead' r

-- | General purpose function to perform a search of a sorted tree, using the supplied selector.
-- This function is similar to 'assertRead', but returns 'Nothing' if the search failed.
--
-- Complexity: O(log n)
tryRead :: AVL e -> (e -> COrdering a) ->  Maybe a
tryRead t c = tryRead' t where
 tryRead'  E        = Nothing
 tryRead' (N l e r) = tryRead'' l e r
 tryRead' (Z l e r) = tryRead'' l e r
 tryRead' (P l e r) = tryRead'' l e r
 tryRead''   l e r  = case c e of
                      Lt   -> tryRead' l
                      Eq a -> Just a
                      Gt   -> tryRead' r

-- | This version returns the result of the selector (without adding a 'Just' wrapper) if the search
-- succeeds, or 'Nothing' if it fails.
--
-- Complexity: O(log n)
tryReadMaybe :: AVL e -> (e -> COrdering (Maybe a)) ->  Maybe a
tryReadMaybe t c = tryRead' t where
 tryRead'  E        = Nothing
 tryRead' (N l e r) = tryRead'' l e r
 tryRead' (Z l e r) = tryRead'' l e r
 tryRead' (P l e r) = tryRead'' l e r
 tryRead''   l e r  = case c e of
                         Lt     -> tryRead' l
                         Eq mba -> mba
                         Gt     -> tryRead' r

-- | General purpose function to perform a search of a sorted tree, using the supplied selector.
-- This function is similar to 'assertRead', but returns a the default value (first argument) if
-- the search fails.
--
-- Complexity: O(log n)
defaultRead :: a -> AVL e -> (e -> COrdering a) -> a
defaultRead d t c = genRead' t where
 genRead'  E        = d
 genRead' (N l e r) = genRead'' l e r
 genRead' (Z l e r) = genRead'' l e r
 genRead' (P l e r) = genRead'' l e r
 genRead''   l e r  = case c e of
                      Lt   -> genRead' l
                      Eq a -> a
                      Gt   -> genRead' r

-- | General purpose function to perform a search of a sorted tree, using the supplied selector.
-- Returns True if matching element is found.
--
-- Complexity: O(log n)
contains :: AVL e -> (e -> Ordering) -> Bool
contains t c = contains' t where
 contains'  E        = False
 contains' (N l e r) = contains'' l e r
 contains' (Z l e r) = contains'' l e r
 contains' (P l e r) = contains'' l e r
 contains''   l e r  = case c e of
                       LT -> contains' l
                       EQ -> True
                       GT -> contains' r
