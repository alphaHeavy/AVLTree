{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL
-- Copyright   :  (c) Adrian Hey 2004,2008
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- Many of the functions defined by this package make use of generalised comparison functions
-- which return a variant of the Prelude 'Prelude.Ordering' data type: 'Data.COrdering.COrdering'. These
-- are refered to as \"combining comparisons\". (This is because they combine \"equal\"
-- values in some manner defined by the user.)
--
-- The idea is that using this simple mechanism you can define many practical and
-- useful variations of tree (or general set) operations from a few generic primitives,
-- something that would not be so easy using plain 'Prelude.Ordering' comparisons
-- (overloaded or otherwise).
--
-- Functions which involve searching a tree really only require a single argument
-- function which takes the current tree element value as argument and returns
-- an 'Prelude.Ordering' or 'Data.COrdering.COrdering' to direct the next stage of the search down
-- the left or right sub-trees (or stop at the current element). For documentation
-- purposes, these functions are called \"selectors\" throughout this library.
-- Typically a selector will be obtained by partially applying the appropriate
-- combining comparison with the value or key being searched for. For example..
--
-- @
-- mySelector :: Int -> Ordering               Tree elements are Ints
-- or..
-- mySelector :: (key,val) -> COrdering val    Tree elements are (key,val) pairs
-- @
--
-----------------------------------------------------------------------------
module Data.Tree.AVL
(module Data.Tree.AVL.Types,
 module Data.Tree.AVL.Read,
 module Data.Tree.AVL.Write,
 module Data.Tree.AVL.Push,
 module Data.Tree.AVL.Delete,
 module Data.Tree.AVL.Set,
 module Data.Tree.AVL.Zipper,
 module Data.Tree.AVL.Join,
 module Data.Tree.AVL.List,
 module Data.Tree.AVL.Split,
 module Data.Tree.AVL.Size,
 module Data.Tree.AVL.Height,

 -- * Low level Binary Path utilities.
 -- | This is the low level (unsafe) API used by the 'BAVL' type.
 BinPath(..),findFullPath,findEmptyPath,openPath,openPathWith,readPath,writePath,insertPath,deletePath,

 -- * Correctness checking.
 isBalanced,isSorted,isSortedOK,

 -- * Tree parameter utilities.
 minElements,maxElements,

 module Data.Tree.AVL.Deprecated,
) where

import Prelude hiding (map) -- so haddock finds the symbols there

import Data.Tree.AVL.Types hiding (E,N,P,Z)
import Data.Tree.AVL.Size
import Data.Tree.AVL.Height
import Data.Tree.AVL.Read
import Data.Tree.AVL.Write
import Data.Tree.AVL.Push
import Data.Tree.AVL.Delete
import Data.Tree.AVL.List
import Data.Tree.AVL.Join
import Data.Tree.AVL.Split
import Data.Tree.AVL.Set
import Data.Tree.AVL.Zipper
import Data.Tree.AVL.Test.Utils(isBalanced,isSorted,isSortedOK,minElements,maxElements)
import Data.Tree.AVL.BinPath(BinPath(..),findFullPath,findEmptyPath,openPath,openPathWith,readPath,writePath,insertPath)
import Data.Tree.AVL.Internals.DelUtils(deletePath)
import Data.Tree.AVL.Deprecated
#if __GLASGOW_HASKELL__ > 604
import Data.Traversable
#endif


{- These are now derived since switch to structural equality!
-- | Show is based on showing the list produced by 'asListL'. This definition has been placed here
-- to avoid introducing cyclic dependency between Types.hs and List.hs
instance Show e => Show (AVL e) where
 -- showsPrec :: Int -> AVL e -> Shows       -- type Shows = String -> String
 showsPrec _ t = ("AVL " ++) . showList (asListL t)

instance Read e => Read (AVL e) where
 -- readsPrec :: Int -> ReadS a               -- type ReadS a = String -> [(a,String)]
 readsPrec _ str = case lex str of
                   [("AVL",str')] -> [(asTreeL es, str'') | (es,str'') <- readList str']
                   _              -> []
-}

-- | AVL trees are an instance of 'Functor'. This definition has been placed here
-- to avoid introducing cyclic dependency between Types.hs and List.hs
instance Functor AVL where
 fmap = map           -- The lazy version.

#if __GLASGOW_HASKELL__ > 604
instance Traversable AVL where
    traverse = traverseAVL
#endif
