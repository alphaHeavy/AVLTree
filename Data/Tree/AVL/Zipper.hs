{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Zipper
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Data.Tree.AVL.Zipper
(-- * The AVL Zipper
 -- | An implementation of \"The Zipper\" for AVL trees. This can be used like
 -- a functional pointer to a serial data structure which can be navigated
 -- and modified, without having to worry about all those tricky tree balancing
 -- issues. See JFP Vol.7 part 5 or ..
 --
 -- <http://haskell.org/haskellwiki/Zipper>
 --
 -- Notes about efficiency:
 --
 -- The functions defined here provide a useful way to achieve those awkward
 -- operations which may not be covered by the rest of this package. They're
 -- reasonably efficient (mostly O(log n) or better), but zipper flexibility
 -- is bought at the expense of keeping path information explicitly as a heap
 -- data structure rather than implicitly on the stack. Since heap storage
 -- probably costs more, zipper operations will are likely to incur higher
 -- constant factors than equivalent non-zipper operations (if available).
 --
 -- Some of the functions provided here may appear to be weird combinations of
 -- functions from a more logical set of primitives. They are provided because
 -- they are not really simple combinations of the corresponding primitives.
 -- They are more efficient, so you should use them if possible (e.g combining
 -- deleting with Zipper closing).
 --
 -- Also, consider using the 'BAVL' as a cheaper alternative if you don't
 -- need to navigate the tree.

 -- ** Types
 ZAVL,PAVL,

 -- ** Opening
 assertOpenL,assertOpenR,
 tryOpenL,tryOpenR,
 assertOpen,tryOpen,
 tryOpenGE,tryOpenLE,
 openEither,

 -- ** Closing
 close,fillClose,

 -- ** Manipulating the current element.
 getCurrent,putCurrent,applyCurrent,applyCurrent',

 -- ** Moving
 assertMoveL,assertMoveR,tryMoveL,tryMoveR,

 -- ** Inserting elements
 insertL,insertR,insertMoveL,insertMoveR,fill,

 -- ** Deleting elements
 delClose,
 assertDelMoveL,assertDelMoveR,tryDelMoveR,tryDelMoveL,
 delAllL,delAllR,
 delAllCloseL,delAllCloseR,
 delAllIncCloseL,delAllIncCloseR,

 -- ** Inserting AVL trees
 insertTreeL,insertTreeR,

 -- ** Current element status
 isLeftmost,isRightmost,
 sizeL,sizeR,

 -- ** Operations on whole zippers
 sizeZAVL,

 -- ** A cheaper option is to use BAVL
 -- | These are a cheaper but more restrictive alternative to using the full Zipper.
 -- They use \"Binary Paths\" (Ints) to point to a particular element of an 'AVL' tree.
 -- Use these when you don't need to navigate the tree, you just want to look at a
 -- particular element (and perhaps modify or delete it). The advantage of these is
 -- that they don't create the usual Zipper heap structure, so they will be faster
 -- (and reduce heap burn rate too).
 --
 -- If you subsequently decide you need a Zipper rather than a BAVL then some conversion
 -- utilities are provided.

 -- *** Types
 BAVL,

 -- *** Opening and closing
 openBAVL,closeBAVL,

 -- *** Inspecting status
 fullBAVL,emptyBAVL,tryReadBAVL,readFullBAVL,

 -- *** Modifying the tree
 pushBAVL,deleteBAVL,

 -- *** Converting to BAVL to Zipper
 -- | These are O(log n) operations but with low constant factors because no comparisons
 -- are required (and the tree nodes on the path will most likely still be in cache as
 -- a result of opening the BAVL in the first place).
 fullBAVLtoZAVL,emptyBAVLtoPAVL,anyBAVLtoEither,
) where

import Prelude -- so haddock finds the symbols there

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.Size(size,addSize)
import Data.Tree.AVL.Height(height,addHeight)
import Data.Tree.AVL.Internals.DelUtils(deletePath,popRN,popRZ,popRP,popLN,popLZ,popLP)
import Data.Tree.AVL.Internals.HJoin(spliceH,joinH)
import Data.Tree.AVL.Internals.HPush(pushHL,pushHR)
import Data.Tree.AVL.BinPath(BinPath(..),openPath,writePath,insertPath,sel,goL,goR)

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

-- N.B. Zippers are always opened using relative heights for efficiency reasons. On the
-- whole this causes no problems, except when inserting entire AVL trees or substituting
-- the empty tree. (These cases have some minor height computation overhead).

-- | Abstract data type for a successfully opened AVL tree. All ZAVL\'s are non-empty!
-- A ZAVL can be tought of as a functional pointer to an AVL tree element.
data ZAVL e = ZAVL (Path e) (AVL e) !UINT e (AVL e) !UINT

-- | Abstract data type for an unsuccessfully opened AVL tree.
-- A PAVL can be thought of as a functional pointer to the gap
-- where the expected element should be (but isn't). You can fill this gap using
-- the 'fill' function, or fill and close at the same time using the 'fillClose' function.
data PAVL e = PAVL (Path e) !UINT

data Path e = EP                          -- Empty Path
            | LP (Path e) e (AVL e) !UINT -- Left subtree was taken
            | RP (Path e) e (AVL e) !UINT -- Right subtree was taken

-- Local Closing Utility
close_ :: Path e -> AVL e -> UINT -> AVL e
close_  EP        t _ = t
close_ (LP p e r hr) l hl = case spliceH l hl e r hr of UBT2(t,ht) -> close_ p t ht
close_ (RP p e l hl) r hr = case spliceH l hl e r hr of UBT2(t,ht) -> close_ p t ht

-- Local Utility to remove all left paths from a path
noLP :: Path e -> Path e
noLP  EP           = EP
noLP (LP p _ _ _ ) = noLP p
noLP (RP p e l hl) = let p_ = noLP p in p_ `seq` RP p_ e l hl

-- Local Utility to remove all right paths from a path
noRP :: Path e -> Path e
noRP  EP           = EP
noRP (LP p e r hr) = let p_ = noRP p in p_ `seq` LP p_ e r hr
noRP (RP p _ _ _ ) = noRP p

-- Local Closing Utility which ignores all left paths
closeNoLP :: Path e -> AVL e -> UINT -> AVL e
closeNoLP  EP           t _  = t
closeNoLP (LP p _ _ _ ) l hl = closeNoLP p l hl
closeNoLP (RP p e l hl) r hr = case spliceH l hl e r hr of UBT2(t,ht) -> closeNoLP p t ht

-- Local Closing Utility which ignores all right paths
closeNoRP :: Path e -> AVL e -> UINT -> AVL e
closeNoRP  EP           t _  = t
closeNoRP (LP p e r hr) l hl = case spliceH l hl e r hr of UBT2(t,ht) -> closeNoRP p t ht
closeNoRP (RP p _ _ _ ) r hr = closeNoRP p r hr

-- Add size of all path elements.
addSizeP :: Int -> Path e -> Int
addSizeP n  EP          = n
addSizeP n (LP p _ r _) = addSizeP (addSize (n+1) r) p
addSizeP n (RP p _ l _) = addSizeP (addSize (n+1) l) p

-- Add size of all RP path elements.
addSizeRP :: Int -> Path e -> Int
addSizeRP n  EP          = n
addSizeRP n (LP p _ _ _) = addSizeRP n p
addSizeRP n (RP p _ l _) = addSizeRP (addSize (n+1) l) p

-- Add size of all LP path elements.
addSizeLP :: Int -> Path e -> Int
addSizeLP n  EP          = n
addSizeLP n (LP p _ r _) = addSizeLP (addSize (n+1) r) p
addSizeLP n (RP p _ _ _) = addSizeLP n p

-- | Opens a sorted AVL tree at the element given by the supplied selector. This function
-- raises an error if the tree does not contain such an element.
--
-- Complexity: O(log n)
assertOpen :: (e -> Ordering) -> AVL e -> ZAVL e
assertOpen c t = op EP L(0) t where -- Relative heights !!
 -- op :: (Path e) -> UINT -> AVL e -> ZAVL e
 op _ _  E        = error "assertOpen: No matching element."
 op p h (N l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT2(h) l
                    EQ -> ZAVL p l DECINT2(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT2(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (Z l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> ZAVL p l DECINT1(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (P l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT2(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> ZAVL p l DECINT1(h) e r DECINT2(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT2(h) r

-- | Attempts to open a sorted AVL tree at the element given by the supplied selector.
-- This function returns 'Nothing' if there is no such element.
--
-- Note that this operation will still create a zipper path structure on the heap (which
-- is promptly discarded) if the search fails, and so is potentially inefficient if failure
-- is likely. In cases like this it may be better to use 'openBAVL', test for \"fullness\"
-- using 'fullBAVL' and then convert to a 'ZAVL' using 'fullBAVLtoZAVL'.
--
-- Complexity: O(log n)
tryOpen :: (e -> Ordering) -> AVL e -> Maybe (ZAVL e)
tryOpen c t = op EP L(0) t where -- Relative heights !!
 -- op :: (Path e) -> UINT -> AVL e -> Maybe (ZAVL e)
 op _ _  E        = Nothing
 op p h (N l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT2(h) l
                    EQ -> Just $! ZAVL p l DECINT2(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT2(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (Z l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> Just $! ZAVL p l DECINT1(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (P l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT2(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> Just $! ZAVL p l DECINT1(h) e r DECINT2(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT2(h) r

-- | Attempts to open a sorted AVL tree at the least element which is greater than or equal, according to
-- the supplied selector. This function returns 'Nothing' if the tree does not contain such an element.
--
-- Complexity: O(log n)
tryOpenGE :: (e -> Ordering) -> AVL e -> Maybe (ZAVL e)
tryOpenGE c t = op EP L(0) t where -- Relative heights !!
 -- op :: (Path e) -> UINT -> AVL e -> ZAVL e
 op p h  E        = backupR p E h where
                     backupR  EP            _ _  = Nothing
                     backupR (LP p_ e r hr) l hl = Just $! ZAVL p_ l hl e r hr
                     backupR (RP p_ e l hl) r hr = case spliceH l hl e r hr of UBT2(t_,ht_) -> backupR p_ t_ ht_
 op p h (N l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT2(h) l
                    EQ -> Just $! ZAVL p l DECINT2(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT2(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (Z l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> Just $! ZAVL p l DECINT1(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (P l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT2(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> Just $! ZAVL p l DECINT1(h) e r DECINT2(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT2(h) r

-- | Attempts to open a sorted AVL tree at the greatest element which is less than or equal, according to
-- the supplied selector. This function returns _Nothing_ if the tree does not contain such an element.
--
-- Complexity: O(log n)
tryOpenLE :: (e -> Ordering) -> AVL e -> Maybe (ZAVL e)
tryOpenLE c t = op EP L(0) t where -- Relative heights !!
 -- op :: (Path e) -> UINT -> AVL e -> ZAVL e
 op p h  E        = backupL p E h where
                     backupL  EP            _ _  = Nothing
                     backupL (LP p_ e r hr) l hl = case spliceH l hl e r hr of UBT2(t_,ht_) -> backupL p_ t_ ht_
                     backupL (RP p_ e l hl) r hr = Just $! ZAVL p_ l hl e r hr
 op p h (N l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT2(h) l
                    EQ -> Just $! ZAVL p l DECINT2(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT2(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (Z l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> Just $! ZAVL p l DECINT1(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (P l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT2(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> Just $! ZAVL p l DECINT1(h) e r DECINT2(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT2(h) r

-- | Opens a non-empty AVL tree at the leftmost element.
-- This function raises an error if the tree is empty.
--
-- Complexity: O(log n)
assertOpenL :: AVL e -> ZAVL e
assertOpenL  E        = error "assertOpenL: Empty tree."
assertOpenL (N l e r) = openLN EP L(0) l e r            -- Relative heights !!
assertOpenL (Z l e r) = openLZ EP L(0) l e r            -- Relative heights !!
assertOpenL (P l e r) = openL_ (LP EP e r L(0)) L(1) l  -- Relative heights !!

-- | Attempts to open a non-empty AVL tree at the leftmost element.
-- This function returns 'Nothing' if the tree is empty.
--
-- Complexity: O(log n)
tryOpenL :: AVL e -> Maybe (ZAVL e)
tryOpenL  E        = Nothing
tryOpenL (N l e r) = Just $! openLN EP L(0) l e r             -- Relative heights !!
tryOpenL (Z l e r) = Just $! openLZ EP L(0) l e r             -- Relative heights !!
tryOpenL (P l e r) = Just $! openL_ (LP EP e r L(0)) L(1) l   -- Relative heights !!

-- Local utility for opening at the leftmost element, using current path and height.
openL_ :: (Path e) -> UINT -> AVL e -> ZAVL e
openL_ _ _  E        = error "openL_: Bug0"
openL_ p h (N l e r) = openLN p h l e r
openL_ p h (Z l e r) = openLZ p h l e r
openL_ p h (P l e r) = let p_ = LP p e r DECINT2(h) in p_ `seq` openL_ p_ DECINT1(h) l

-- Open leftmost of (N l e r), where l may be E
openLN :: (Path e) -> UINT -> AVL e -> e -> AVL e -> ZAVL e
openLN p h  E           e r = ZAVL p E DECINT2(h) e r DECINT1(h)
openLN p h (N ll le lr) e r = let p_  = LP p e r DECINT1(h) in p_ `seq` openLN p_ DECINT2(h) ll le lr
openLN p h (Z ll le lr) e r = let p_  = LP p e r DECINT1(h) in p_ `seq` openLZ p_ DECINT2(h) ll le lr
openLN p h (P ll le lr) e r = let p_  = LP p e r DECINT1(h)
                                  p__ = p_ `seq` LP p_ le lr DECINT4(h)
                              in p__ `seq` openL_ p__ DECINT3(h) ll
-- Open leftmost of (Z l e r), where l may be E
openLZ :: (Path e) -> UINT -> AVL e -> e -> AVL e -> ZAVL e
openLZ p h  E           e r = ZAVL p E DECINT1(h) e r DECINT1(h)
openLZ p h (N ll le lr) e r = let p_  = LP p e r DECINT1(h) in p_ `seq` openLN p_ DECINT1(h) ll le lr
openLZ p h (Z ll le lr) e r = let p_  = LP p e r DECINT1(h) in p_ `seq` openLZ p_ DECINT1(h) ll le lr
openLZ p h (P ll le lr) e r = let p_  = LP p e r DECINT1(h)
                                  p__ = p_ `seq` LP p_ le lr DECINT3(h)
                              in p__ `seq` openL_ p__ DECINT2(h) ll

-- | Opens a non-empty AVL tree at the rightmost element.
-- This function raises an error if the tree is empty.
--
-- Complexity: O(log n)
assertOpenR :: AVL e -> ZAVL e
assertOpenR  E        = error "assertOpenR: Empty tree."
assertOpenR (N l e r) = openR_ (RP EP e l L(0)) L(1) r  -- Relative heights !!
assertOpenR (Z l e r) = openRZ EP L(0) l e r            -- Relative heights !!
assertOpenR (P l e r) = openRP EP L(0) l e r            -- Relative heights !!

-- | Attempts to open a non-empty AVL tree at the rightmost element.
-- This function returns 'Nothing' if the tree is empty.
--
-- Complexity: O(log n)
tryOpenR :: AVL e -> Maybe (ZAVL e)
tryOpenR  E        = Nothing
tryOpenR (N l e r) = Just $! openR_ (RP EP e l L(0)) L(1) r  -- Relative heights !!
tryOpenR (Z l e r) = Just $! openRZ EP L(0) l e r            -- Relative heights !!
tryOpenR (P l e r) = Just $! openRP EP L(0) l e r            -- Relative heights !!

-- Local utility for opening at the rightmost element, using current path and height.
openR_ :: (Path e) -> UINT -> AVL e -> ZAVL e
openR_ _ _  E        = error "openR_: Bug0"
openR_ p h (N l e r) = let p_ = RP p e l DECINT2(h) in p_ `seq` openR_ p_ DECINT1(h) r
openR_ p h (Z l e r) = openRZ p h l e r
openR_ p h (P l e r) = openRP p h l e r
-- Open rightmost of (P l e r), where r may be E
openRP :: (Path e) -> UINT -> AVL e -> e -> AVL e -> ZAVL e
openRP p h l e  E           = ZAVL p l DECINT1(h) e E DECINT2(h)
openRP p h l e (N rl re rr) = let p_  = RP p e l DECINT1(h)
                                  p__ = p_ `seq` RP p_ re rl DECINT4(h)
                              in p__ `seq` openR_ p__ DECINT3(h) rr
openRP p h l e (Z rl re rr) = let p_ = RP p e l DECINT1(h) in p_ `seq` openRZ p_ DECINT2(h) rl re rr
openRP p h l e (P rl re rr) = let p_ = RP p e l DECINT1(h) in p_ `seq` openRP p_ DECINT2(h) rl re rr
-- Open rightmost of (Z l e r), where r may be E
openRZ :: (Path e) -> UINT -> AVL e -> e -> AVL e -> ZAVL e
openRZ p h l e  E           = ZAVL p l DECINT1(h) e E DECINT1(h)
openRZ p h l e (N rl re rr) = let p_  = RP p e l DECINT1(h)
                                  p__ = p_ `seq` RP p_ re rl DECINT3(h)
                              in p__ `seq` openR_ p__ DECINT2(h) rr
openRZ p h l e (Z rl re rr) = let p_ = RP p e l DECINT1(h) in p_ `seq` openRZ p_ DECINT1(h) rl re rr
openRZ p h l e (P rl re rr) = let p_ = RP p e l DECINT1(h) in p_ `seq` openRP p_ DECINT1(h) rl re rr

-- | Returns @('Right' zavl)@ if the expected element was found, @('Left' pavl)@ if the
-- expected element was not found. It's OK to use this function on empty trees.
--
-- Complexity: O(log n)
openEither :: (e -> Ordering) -> AVL e -> Either (PAVL e) (ZAVL e)
openEither c t = op EP L(0) t where -- Relative heights !!
 -- op :: (Path e) -> UINT -> AVL e -> Either (PAVL e) (ZAVL e)
 op p h  E        = Left $! PAVL p h
 op p h (N l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT2(h) l
                    EQ -> Right $! ZAVL p l DECINT2(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT2(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (Z l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> Right $! ZAVL p l DECINT1(h) e r DECINT1(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT1(h) r
 op p h (P l e r) = case c e of
                    LT -> let p_ = LP p e r DECINT2(h) in p_ `seq` op p_ DECINT1(h) l
                    EQ -> Right $! ZAVL p l DECINT1(h) e r DECINT2(h)
                    GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` op p_ DECINT2(h) r

-- | Fill the gap pointed to by a 'PAVL' with the supplied element, which becomes
-- the current element of the resulting 'ZAVL'. The supplied filling element should
-- be \"equal\" to the value used in the search which created the 'PAVL'.
--
-- Complexity: O(1)
fill :: e -> PAVL e -> ZAVL e
fill e (PAVL p h) = ZAVL p E h e E h

-- | Essentially the same operation as 'fill', but the resulting 'ZAVL' is closed
-- immediately.
--
-- Complexity: O(log n)
fillClose :: e -> PAVL e -> AVL e
fillClose e (PAVL p h) = close_ p (Z E e E) INCINT1(h)

-- | Closes a Zipper.
--
-- Complexity: O(log n)
close :: ZAVL e -> AVL e
close (ZAVL p l hl e r hr) = case spliceH l hl e r hr of UBT2(t,ht) -> close_ p t ht

-- | Deletes the current element and then closes the Zipper.
--
-- Complexity: O(log n)
delClose :: ZAVL e -> AVL e
delClose (ZAVL p l hl _ r hr) = case joinH l hl r hr of UBT2(t,ht) -> close_ p t ht

-- | Gets the current element of a Zipper.
--
-- Complexity: O(1)
getCurrent :: ZAVL e -> e
getCurrent (ZAVL _ _ _ e _ _) = e

-- | Overwrites the current element of a Zipper.
--
-- Complexity: O(1)
putCurrent :: e -> ZAVL e -> ZAVL e
putCurrent e (ZAVL p l hl _ r hr) = ZAVL p l hl e r hr

-- | Applies a function to the current element of a Zipper (lazily).
-- See also 'applyCurrent'' for a strict version of this function.
--
-- Complexity: O(1)
applyCurrent :: (e -> e) -> ZAVL e -> ZAVL e
applyCurrent f (ZAVL p l hl e r hr) = ZAVL p l hl (f e) r hr

-- | Applies a function to the current element of a Zipper strictly.
-- See also 'applyCurrent' for a non-strict version of this function.
--
-- Complexity: O(1)
applyCurrent' :: (e -> e) -> ZAVL e -> ZAVL e
applyCurrent' f (ZAVL p l hl e r hr) = let e_ = f e in e_ `seq` ZAVL p l hl e_ r hr

-- | Moves one step left.
-- This function raises an error if the current element is already the leftmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
assertMoveL :: ZAVL e -> ZAVL e
assertMoveL (ZAVL p E           _   e r hr) = case pushHL e r hr of UBT2(t,ht) -> cR p t ht
 where cR  EP               _  _   = error "assertMoveL: Can't move left."
       cR (LP p_ e_ r_ hr_) l_ hl_ = case spliceH l_ hl_ e_ r_ hr_ of UBT2(t,ht) -> cR p_ t ht
       cR (RP p_ e_ l_ hl_) r_ hr_ = ZAVL p_ l_ hl_ e_ r_ hr_
assertMoveL (ZAVL p (N ll le lr) hl e r hr) = let p_ = RP (LP p e r hr) le ll DECINT2(hl)
                                              in p_ `seq` openR_ p_ DECINT1(hl) lr
assertMoveL (ZAVL p (Z ll le lr) hl e r hr) = openRZ (LP p e r hr) hl ll le lr
assertMoveL (ZAVL p (P ll le lr) hl e r hr) = openRP (LP p e r hr) hl ll le lr

-- | Attempts to move one step left.
-- This function returns 'Nothing' if the current element is already the leftmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
tryMoveL :: ZAVL e -> Maybe (ZAVL e)
tryMoveL (ZAVL p E            _  e r hr) = case pushHL e r hr of UBT2(t,ht) -> cR p t ht
 where cR  EP               _  _      = Nothing
       cR (LP p_ e_ r_ hr_) l_ hl_    = case spliceH l_ hl_ e_ r_ hr_ of UBT2(t,ht) -> cR p_ t ht
       cR (RP p_ e_ l_ hl_) r_ hr_    = Just $! ZAVL p_ l_ hl_ e_ r_ hr_
tryMoveL (ZAVL p (N ll le lr) hl e r hr) = Just $! let p_ = RP (LP p e r hr) le ll DECINT2(hl)
                                                   in p_ `seq` openR_ p_ DECINT1(hl) lr
tryMoveL (ZAVL p (Z ll le lr) hl e r hr) = Just $! openRZ (LP p e r hr) hl ll le lr
tryMoveL (ZAVL p (P ll le lr) hl e r hr) = Just $! openRP (LP p e r hr) hl ll le lr

-- | Moves one step right.
-- This function raises an error if the current element is already the rightmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
assertMoveR :: ZAVL e -> ZAVL e
assertMoveR (ZAVL p l hl e  E           _ ) = case pushHR l hl e of UBT2(t,ht) -> cL p t ht
 where cL  EP               _  _   = error "assertMoveR: Can't move right."
       cL (RP p_ e_ l_ hl_) r_ hr_ = case spliceH l_ hl_ e_ r_ hr_ of UBT2(t,ht) -> cL p_ t ht
       cL (LP p_ e_ r_ hr_) l_ hl_ = ZAVL p_ l_ hl_ e_ r_ hr_
assertMoveR (ZAVL p l hl e (N rl re rr) hr) = openLN (RP p e l hl) hr rl re rr
assertMoveR (ZAVL p l hl e (Z rl re rr) hr) = openLZ (RP p e l hl) hr rl re rr
assertMoveR (ZAVL p l hl e (P rl re rr) hr) = let p_ = LP (RP p e l hl) re rr DECINT2(hr)
                                              in p_ `seq` openL_ p_ DECINT1(hr) rl

-- | Attempts to move one step right.
-- This function returns 'Nothing' if the current element is already the rightmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
tryMoveR :: ZAVL e -> Maybe (ZAVL e)
tryMoveR (ZAVL p l hl e  E           _ ) = case pushHR l hl e of UBT2(t,ht) -> cL p t ht
 where cL  EP               _  _   = Nothing
       cL (RP p_ e_ l_ hl_) r_ hr_ = case spliceH l_ hl_ e_ r_ hr_ of UBT2(t,ht) -> cL p_ t ht
       cL (LP p_ e_ r_ hr_) l_ hl_ = Just $! ZAVL p_ l_ hl_ e_ r_ hr_
tryMoveR (ZAVL p l hl e (N rl re rr) hr) = Just $! openLN (RP p e l hl) hr rl re rr
tryMoveR (ZAVL p l hl e (Z rl re rr) hr) = Just $! openLZ (RP p e l hl) hr rl re rr
tryMoveR (ZAVL p l hl e (P rl re rr) hr) = Just $! let p_ = LP (RP p e l hl) re rr DECINT2(hr)
                                                   in p_ `seq` openL_ p_ DECINT1(hr) rl

-- | Returns 'True' if the current element is the leftmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
isLeftmost :: ZAVL e -> Bool
isLeftmost (ZAVL p E _ _ _ _) = iL p
 where iL  EP           = True
       iL (LP p_ _ _ _) = iL p_
       iL (RP _  _ _ _) = False
isLeftmost (ZAVL _ _ _ _ _ _) = False

-- | Returns 'True' if the current element is the rightmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
isRightmost :: ZAVL e -> Bool
isRightmost (ZAVL p _ _ _ E _) = iR p
 where iR  EP           = True
       iR (RP p_ _ _ _) = iR p_
       iR (LP _  _ _ _) = False
isRightmost (ZAVL _ _ _ _ _ _) = False

-- | Inserts a new element to the immediate left of the current element.
--
-- Complexity: O(1) average, O(log n) worst case.
insertL :: e -> ZAVL e -> ZAVL e
insertL e0 (ZAVL p l hl e1 r hr) = case pushHR l hl e0 of UBT2(l_,hl_) -> ZAVL p l_ hl_ e1 r hr

-- | Inserts a new element to the immediate left of the current element and then
-- moves one step left (so the newly inserted element becomes the current element).
--
-- Complexity: O(1) average, O(log n) worst case.
insertMoveL :: e -> ZAVL e -> ZAVL e
insertMoveL e0 (ZAVL p l hl e1 r hr) = case pushHL e1 r hr of UBT2(r_,hr_) -> ZAVL p l hl e0 r_ hr_

-- | Inserts a new element to the immediate right of the current element.
--
-- Complexity: O(1) average, O(log n) worst case.
insertR :: ZAVL e -> e -> ZAVL e
insertR (ZAVL p l hl e0 r hr) e1  = case pushHL e1 r hr of UBT2(r_,hr_) -> ZAVL p l hl e0 r_ hr_

-- | Inserts a new element to the immediate right of the current element and then
-- moves one step right (so the newly inserted element becomes the current element).
--
-- Complexity: O(1) average, O(log n) worst case.
insertMoveR :: ZAVL e -> e -> ZAVL e
insertMoveR (ZAVL p l hl e0 r hr) e1  = case pushHR l hl e0 of UBT2(l_,hl_) -> ZAVL p l_ hl_ e1 r hr

-- | Inserts a new AVL tree to the immediate left of the current element.
--
-- Complexity: O(log n), where n is the size of the inserted tree.
insertTreeL :: AVL e -> ZAVL e -> ZAVL e
insertTreeL E           zavl = zavl
insertTreeL t@(N l _ _) zavl = insertLH t (addHeight L(2) l) zavl -- Absolute height required!!
insertTreeL t@(Z l _ _) zavl = insertLH t (addHeight L(1) l) zavl -- Absolute height required!!
insertTreeL t@(P _ _ r) zavl = insertLH t (addHeight L(2) r) zavl -- Absolute height required!!


-- Local utility to insert an AVL to the immediate left of the current element.
-- This operation carries a minor overhead in that we must convert the absolute
-- AVL height into a relative height with the same offset as the rest of the ZAVL.
-- This requires calculation of the absolute height at the current position, but
-- this should be relatively cheap because the overwhelming majority of elements will
-- be close to the bottom of any tree.
insertLH :: AVL e -> UINT -> ZAVL e -> ZAVL e
insertLH t ht (ZAVL p l hl e r hr) =
 let offset = case COMPAREUINT hl hr of -- chose smaller sub-tree to calculate absolute height
              LT -> SUBINT(hl,height l)
              EQ -> SUBINT(hl,height l)
              GT -> SUBINT(hr,height r)
 in case joinH l hl t ADDINT(ht,offset) of UBT2(l_,hl_) -> ZAVL p l_ hl_ e r hr

-- | Inserts a new AVL tree to the immediate right of the current element.
--
-- Complexity: O(log n), where n is the size of the inserted tree.
insertTreeR :: ZAVL e -> AVL e -> ZAVL e
insertTreeR zavl E           = zavl
insertTreeR zavl t@(N l _ _) = insertRH t (addHeight L(2) l) zavl -- Absolute height required!!
insertTreeR zavl t@(Z l _ _) = insertRH t (addHeight L(1) l) zavl -- Absolute height required!!
insertTreeR zavl t@(P _ _ r) = insertRH t (addHeight L(2) r) zavl -- Absolute height required!!

-- Local utility to insert an AVL to the immediate right of the current element.
-- This operation carries a minor overhead in that we must convert the absolute
-- AVL height into a relative height with the same offset as the rest of the ZAVL.
-- This requires calculation of the absolute height at the current position, but
-- this should be relatively cheap because the overwhelming majority of elements will
-- be close to the bottom of any tree.
insertRH :: AVL e -> UINT -> ZAVL e -> ZAVL e
insertRH t ht (ZAVL p l hl e r hr) =
 let offset = case COMPAREUINT hl hr of -- chose smaller sub-tree to calculate absolute height
              LT -> SUBINT(hl,height l)
              EQ -> SUBINT(hr,height r)
              GT -> SUBINT(hr,height r)
 in case joinH t ADDINT(ht,offset) r hr of UBT2(r_,hr_) -> ZAVL p l hl e r_ hr_


-- | Deletes the current element and moves one step left.
-- This function raises an error if the current element is already the leftmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
assertDelMoveL :: ZAVL e -> ZAVL e
assertDelMoveL (ZAVL p  E            _ _ r hr) = dR p r hr
 where dR  EP               _  _   = error "assertDelMoveL: Can't move left."
       dR (LP p_ e_ r_ hr_) l_ hl_ = case spliceH l_ hl_ e_ r_ hr_ of UBT2(t,ht) -> dR p_ t ht
       dR (RP p_ e_ l_ hl_) r_ hr_ = ZAVL p_ l_ hl_ e_ r_ hr_
assertDelMoveL (ZAVL p (N ll le lr) hl _ r hr) = case popRN ll le lr of
                                                 UBT2(l,e) -> case l of
                                                              Z _ _ _ -> ZAVL p l DECINT1(hl) e r hr
                                                              N _ _ _ -> ZAVL p l         hl  e r hr
                                                              _       -> error "assertDelMoveL: Bug0" -- impossible
assertDelMoveL (ZAVL p (Z ll le lr) hl _ r hr) = case popRZ ll le lr of
                                                 UBT2(l,e) -> case l of
                                                              E       -> ZAVL p l DECINT1(hl) e r hr -- Don't use E!!
                                                              N _ _ _ -> error "assertDelMoveL: Bug1"      -- impossible
                                                              _       -> ZAVL p l         hl  e r hr
assertDelMoveL (ZAVL p (P ll le lr) hl _ r hr) = case popRP ll le lr of
                                                 UBT2(l,e) -> case l of
                                                        E       -> error "assertDelMoveL: Bug2" -- impossible
                                                        Z _ _ _ -> ZAVL p l DECINT1(hl) e r hr
                                                        _       -> ZAVL p l         hl  e r hr


-- | Attempts to delete the current element and move one step left.
-- This function returns 'Nothing' if the current element is already the leftmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
tryDelMoveL :: ZAVL e -> Maybe (ZAVL e)
tryDelMoveL (ZAVL p  E            _ _ r hr) = dR p r hr
 where dR  EP               _  _   = Nothing
       dR (LP p_ e_ r_ hr_) l_ hl_ = case spliceH l_ hl_ e_ r_ hr_ of UBT2(t,ht) -> dR p_ t ht
       dR (RP p_ e_ l_ hl_) r_ hr_ = Just $! ZAVL p_ l_ hl_ e_ r_ hr_
tryDelMoveL (ZAVL p (N ll le lr) hl _ r hr) = Just $! case popRN ll le lr of
                                              UBT2(l,e) -> case l of
                                                           Z _ _ _ -> ZAVL p l DECINT1(hl) e r hr
                                                           N _ _ _ -> ZAVL p l         hl  e r hr
                                                           _       -> error "tryDelMoveL: Bug0" -- impossible
tryDelMoveL (ZAVL p (Z ll le lr) hl _ r hr) = Just $! case popRZ ll le lr of
                                              UBT2(l,e) -> case l of
                                                           E       -> ZAVL p l DECINT1(hl) e r hr -- Don't use E!!
                                                           N _ _ _ -> error "tryDelMoveL: Bug1"   -- impossible
                                                           _       -> ZAVL p l         hl  e r hr
tryDelMoveL (ZAVL p (P ll le lr) hl _ r hr) = Just $! case popRP ll le lr of
                                              UBT2(l,e) -> case l of
                                                           E       -> error "tryDelMoveL: Bug2" -- impossible
                                                           Z _ _ _ -> ZAVL p l DECINT1(hl) e r hr
                                                           _       -> ZAVL p l         hl  e r hr


-- | Deletes the current element and moves one step right.
-- This function raises an error if the current element is already the rightmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
assertDelMoveR :: ZAVL e -> ZAVL e
assertDelMoveR (ZAVL p l hl _ E            _ ) = dL p l hl
 where dL  EP               _  _   = error "delMoveR: Can't move right."
       dL (LP p_ e_ r_ hr_) l_ hl_ = ZAVL p_ l_ hl_ e_ r_ hr_
       dL (RP p_ e_ l_ hl_) r_ hr_ = case spliceH l_ hl_ e_ r_ hr_ of UBT2(t,ht) -> dL p_ t ht
assertDelMoveR (ZAVL p l hl _ (N rl re rr) hr) = case popLN rl re rr of
                                                 UBT2(e,r) -> case r of
                                                              E       -> error "delMoveR: Bug0" -- impossible
                                                              Z _ _ _ -> ZAVL p l hl e r DECINT1(hr)
                                                              _       -> ZAVL p l hl e r         hr
assertDelMoveR (ZAVL p l hl _ (Z rl re rr) hr) = case popLZ rl re rr of
                                                 UBT2(e,r) -> case r of
                                                              E       -> ZAVL p l hl e r DECINT1(hr) -- Don't use E!!
                                                              P _ _ _ -> error "delMoveR: Bug1" -- impossible
                                                              _       -> ZAVL p l hl e r         hr
assertDelMoveR (ZAVL p l hl _ (P rl re rr) hr) = case popLP rl re rr of
                                                 UBT2(e,r) -> case r of
                                                              Z _ _ _ -> ZAVL p l hl e r DECINT1(hr)
                                                              P _ _ _ -> ZAVL p l hl e r         hr
                                                              _       -> error "delMoveR: Bug2" -- impossible


-- | Attempts to delete the current element and move one step right.
-- This function returns 'Nothing' if the current element is already the rightmost element.
--
-- Complexity: O(1) average, O(log n) worst case.
tryDelMoveR :: ZAVL e -> Maybe (ZAVL e)
tryDelMoveR (ZAVL p l hl _ E            _ ) = dL p l hl
 where dL  EP               _  _   = Nothing
       dL (LP p_ e_ r_ hr_) l_ hl_ = Just $! ZAVL p_ l_ hl_ e_ r_ hr_
       dL (RP p_ e_ l_ hl_) r_ hr_ = case spliceH l_ hl_ e_ r_ hr_ of UBT2(t,ht) -> dL p_ t ht
tryDelMoveR (ZAVL p l hl _ (N rl re rr) hr) = Just $! case popLN rl re rr of
                                              UBT2(e,r) -> case r of
                                                           E       -> error "tryDelMoveR: Bug0" -- impossible
                                                           Z _ _ _ -> ZAVL p l hl e r DECINT1(hr)
                                                           _       -> ZAVL p l hl e r         hr
tryDelMoveR (ZAVL p l hl _ (Z rl re rr) hr) = Just $! case popLZ rl re rr of
                                              UBT2(e,r) -> case r of
                                                           E       -> ZAVL p l hl e r DECINT1(hr) -- Don't use E!!
                                                           P _ _ _ -> error "tryDelMoveR: Bug1" -- impossible
                                                           _       -> ZAVL p l hl e r         hr
tryDelMoveR (ZAVL p l hl _ (P rl re rr) hr) = Just $! case popLP rl re rr of
                                              UBT2(e,r) -> case r of
                                                           Z _ _ _ -> ZAVL p l hl e r DECINT1(hr)
                                                           P _ _ _ -> ZAVL p l hl e r         hr
                                                           _       -> error "tryDelMoveR: Bug2" -- impossible


-- | Delete all elements to the left of the current element.
--
-- Complexity: O(log n)
delAllL :: ZAVL e -> ZAVL e
delAllL (ZAVL p l hl e r hr) =
 let hE = case COMPAREUINT hl hr of -- Calculate relative offset and use this as height of empty tree
          LT -> SUBINT(hl,height l)
          EQ -> SUBINT(hr,height r)
          GT -> SUBINT(hr,height r)
     p_ = noRP p -- remove right paths (current element becomes leftmost)
 in p_ `seq` ZAVL p_ E hE e r hr

-- | Delete all elements to the right of the current element.
--
-- Complexity: O(log n)
delAllR :: ZAVL e -> ZAVL e
delAllR (ZAVL p l hl e r hr) =
 let hE = case COMPAREUINT hl hr of -- Calculate relative offset and use this as height of empty tree
          LT -> SUBINT(hl,height l)
          EQ -> SUBINT(hl,height l)
          GT -> SUBINT(hr,height r)
     p_ = noLP p -- remove left paths (current element becomes rightmost)
 in p_ `seq` ZAVL p_ l hl e E hE

-- | Similar to 'delAllL', in that all elements to the left of the current element are deleted,
-- but this function also closes the tree in the process.
--
-- Complexity: O(log n)
delAllCloseL :: ZAVL e -> AVL e
delAllCloseL (ZAVL p _ _ e r hr) = case pushHL e r hr of UBT2(t,ht) -> closeNoRP p t ht

-- | Similar to 'delAllR', in that all elements to the right of the current element are deleted,
-- but this function also closes the tree in the process.
--
-- Complexity: O(log n)
delAllCloseR :: ZAVL e -> AVL e
delAllCloseR (ZAVL p l hl e _ _) = case pushHR l hl e of UBT2(t,ht) -> closeNoLP p t ht

-- | Similar to 'delAllCloseL', but in this case the current element and all
-- those to the left of the current element are deleted.
--
-- Complexity: O(log n)
delAllIncCloseL :: ZAVL e -> AVL e
delAllIncCloseL (ZAVL p _ _ _ r hr) = closeNoRP p r hr

-- | Similar to 'delAllCloseR', but in this case the current element and all
-- those to the right of the current element are deleted.
--
-- Complexity: O(log n)
delAllIncCloseR :: ZAVL e -> AVL e
delAllIncCloseR (ZAVL p l hl _ _ _) = closeNoLP p l hl

-- | Counts the number of elements to the left of the current element
-- (this does not include the current element).
--
-- Complexity: O(n), where n is the count result.
sizeL :: ZAVL e -> Int
sizeL (ZAVL p l _ _ _ _) = addSizeRP (size l) p

-- | Counts the number of elements to the right of the current element
-- (this does not include the current element).
--
-- Complexity: O(n), where n is the count result.
sizeR :: ZAVL e -> Int
sizeR (ZAVL p _ _ _ r _) = addSizeLP (size r) p

-- | Counts the total number of elements in a ZAVL.
--
-- Complexity: O(n)
sizeZAVL :: ZAVL e -> Int
sizeZAVL (ZAVL p l _ _ r _) = addSizeP (addSize (addSize 1 l) r) p


{-------------------- BAVL stuff below ----------------------------------}

-- | A 'BAVL' is like a pointer reference to somewhere inside an 'AVL' tree. It may be either \"full\"
-- (meaning it points to an actual tree node containing an element), or \"empty\" (meaning it
-- points to the position in a tree where an element was expected but wasn\'t found).
data BAVL e = BAVL (AVL e) (BinPath e)

-- | Search for an element in a /sorted/ 'AVL' tree using the supplied selector.
-- Returns a \"full\" 'BAVL' if a matching element was found, otherwise returns an \"empty\" 'BAVL'.
--
-- Complexity: O(log n)
openBAVL :: (e -> Ordering) -> AVL e -> BAVL e
{-# INLINE openBAVL #-}
openBAVL c t = bp `seq` BAVL t bp
 where bp = openPath c t

-- | Returns the original tree, extracted from the 'BAVL'. Typically you will not need this, as
-- the original tree will still be in scope in most cases.
--
-- Complexity: O(1)
closeBAVL :: BAVL e -> AVL e
{-# INLINE closeBAVL #-}
closeBAVL (BAVL t _) = t

-- | Returns 'True' if the 'BAVL' is \"full\" (a corresponding element was found).
--
-- Complexity: O(1)
fullBAVL :: BAVL e -> Bool
{-# INLINE fullBAVL #-}
fullBAVL (BAVL _ (FullBP  _ _)) = True
fullBAVL (BAVL _ (EmptyBP _  )) = False

-- | Returns 'True' if the 'BAVL' is \"empty\" (no corresponding element was found).
--
-- Complexity: O(1)
emptyBAVL :: BAVL e -> Bool
{-# INLINE emptyBAVL #-}
emptyBAVL (BAVL _ (FullBP  _ _)) = False
emptyBAVL (BAVL _ (EmptyBP _  )) = True

-- | Read the element value from a \"full\" 'BAVL'.
-- This function returns 'Nothing' if applied to an \"empty\" 'BAVL'.
--
-- Complexity: O(1)
tryReadBAVL :: BAVL e -> Maybe e
{-# INLINE tryReadBAVL #-}
tryReadBAVL (BAVL _ (FullBP  _ e)) = Just e
tryReadBAVL (BAVL _ (EmptyBP _  )) = Nothing

-- | Read the element value from a \"full\" 'BAVL'.
-- This function raises an error if applied to an \"empty\" 'BAVL'.
--
-- Complexity: O(1)
readFullBAVL :: BAVL e -> e
{-# INLINE readFullBAVL #-}
readFullBAVL (BAVL _ (FullBP  _ e)) = e
readFullBAVL (BAVL _ (EmptyBP _  )) = error "readFullBAVL: Empty BAVL."

-- | If the 'BAVL' is \"full\", this function returns the original tree with the corresponding
-- element replaced by the new element (first argument). If it\'s \"empty\" the original tree is returned
-- with the new element inserted.
--
-- Complexity: O(log n)
pushBAVL :: e -> BAVL e -> AVL e
{-# INLINE pushBAVL #-}
pushBAVL e (BAVL t (FullBP  p _)) = writePath  p e t
pushBAVL e (BAVL t (EmptyBP p  )) = insertPath p e t

-- | If the 'BAVL' is \"full\", this function returns the original tree with the corresponding
-- element deleted. If it\'s \"empty\" the original tree is returned unmodified.
--
-- Complexity: O(log n) (or O(1) for an empty 'BAVL')
deleteBAVL :: BAVL e -> AVL e
{-# INLINE deleteBAVL #-}
deleteBAVL (BAVL t (FullBP  p _)) = deletePath p t
deleteBAVL (BAVL t (EmptyBP _  )) = t

-- | Converts a \"full\" 'BAVL' as a 'ZAVL'. Raises an error if applied to an \"empty\" 'BAVL'.
--
-- Complexity: O(log n)
fullBAVLtoZAVL :: BAVL e -> ZAVL e
fullBAVLtoZAVL (BAVL t (FullBP  i _)) = openFull i EP L(0) t -- Relative heights !!
fullBAVLtoZAVL (BAVL _ (EmptyBP _  )) = error "fullBAVLtoZAVL: Empty BAVL."
-- Local Utility
openFull :: UINT -> (Path e) -> UINT -> AVL e -> ZAVL e
openFull _ _ _  E        = error "openFull: Bug0."
openFull i p h (N l e r) = case sel i of
                           LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` openFull (goL i) p_ DECINT2(h) l
                           EQ -> ZAVL p l DECINT2(h) e r DECINT1(h)
                           GT -> let p_ = RP p e l DECINT2(h) in p_ `seq` openFull (goR i) p_ DECINT1(h) r
openFull i p h (Z l e r) = case sel i of
                           LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` openFull (goL i) p_ DECINT1(h) l
                           EQ -> ZAVL p l DECINT1(h) e r DECINT1(h)
                           GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` openFull (goR i) p_ DECINT1(h) r
openFull i p h (P l e r) = case sel i of
                           LT -> let p_ = LP p e r DECINT2(h) in p_ `seq` openFull (goL i) p_ DECINT1(h) l
                           EQ -> ZAVL p l DECINT1(h) e r DECINT2(h)
                           GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` openFull (goR i) p_ DECINT2(h) r

-- | Converts an \"empty\" 'BAVL' as a 'PAVL'. Raises an error if applied to a \"full\" 'BAVL'.
--
-- Complexity: O(log n)
emptyBAVLtoPAVL :: BAVL e -> PAVL e
emptyBAVLtoPAVL (BAVL _ (FullBP  _ _)) = error "emptyBAVLtoPAVL: Full BAVL."
emptyBAVLtoPAVL (BAVL t (EmptyBP i  )) = openEmpty i EP L(0) t -- Relative heights !!
-- Local Utility
openEmpty :: UINT -> (Path e) -> UINT -> AVL e -> PAVL e
openEmpty _ p h  E        = PAVL p h -- Test for i==0 ??
openEmpty i p h (N l e r) = case sel i of
                            LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` openEmpty (goL i) p_ DECINT2(h) l
                            EQ -> error "openEmpty: Bug0"
                            GT -> let p_ = RP p e l DECINT2(h) in p_ `seq` openEmpty (goR i) p_ DECINT1(h) r
openEmpty i p h (Z l e r) = case sel i of
                            LT -> let p_ = LP p e r DECINT1(h) in p_ `seq` openEmpty (goL i) p_ DECINT1(h) l
                            EQ -> error "openEmpty: Bug1"
                            GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` openEmpty (goR i) p_ DECINT1(h) r
openEmpty i p h (P l e r) = case sel i of
                            LT -> let p_ = LP p e r DECINT2(h) in p_ `seq` openEmpty (goL i) p_ DECINT1(h) l
                            EQ -> error "openEmpty: Bug2"
                            GT -> let p_ = RP p e l DECINT1(h) in p_ `seq` openEmpty (goR i) p_ DECINT2(h) r


-- | Converts a 'BAVL' to either a 'PAVL' or 'ZAVL' (depending on whether it is \"empty\" or \"full\").
--
-- Complexity: O(log n)
anyBAVLtoEither :: BAVL e -> Either (PAVL e) (ZAVL e)
anyBAVLtoEither (BAVL t (FullBP  i _)) = Right (openFull  i EP L(0) t) -- Relative heights !!
anyBAVLtoEither (BAVL t (EmptyBP i  )) = Left  (openEmpty i EP L(0) t) -- Relative heights !!
