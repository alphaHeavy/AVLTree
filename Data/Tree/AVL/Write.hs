-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Write
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.Write
(-- * Writing to AVL trees
 -- | These functions alter the content of a tree (values of tree elements) but not the structure
 -- of a tree.

 -- ** Writing to extreme left or right
 -- | I'm not sure these are likely to be much use in practice, but they're
 -- simple enough to implement so are included for the sake of completeness.
 writeL,tryWriteL,writeR,tryWriteR,

 -- ** Writing to /sorted/ trees
 write,writeFast,tryWrite,writeMaybe,tryWriteMaybe
) where

import Prelude -- so haddock finds the symbols there

import Data.COrdering
import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.BinPath(BinPath(..),openPathWith,writePath)

---------------------------------------------------------------------------
--                       writeL, tryWriteL                               --
---------------------------------------------------------------------------
-- | Replace the left most element of a tree with the supplied new element.
-- This function raises an error if applied to an empty tree.
--
-- Complexity: O(log n)
writeL :: e -> AVL e -> AVL e
writeL _   E        = error "writeL: Empty Tree"
writeL e' (N l e r) = writeLN e' l e r
writeL e' (Z l e r) = writeLZ e' l e r
writeL e' (P l e r) = writeLP e' l e r

-- | Similar to 'writeL', but returns 'Nothing' if applied to an empty tree.
--
-- Complexity: O(log n)
tryWriteL :: e -> AVL e -> Maybe (AVL e)
tryWriteL _   E        = Nothing
tryWriteL e' (N l e r) = Just $! writeLN e' l e r
tryWriteL e' (Z l e r) = Just $! writeLZ e' l e r
tryWriteL e' (P l e r) = Just $! writeLP e' l e r

-- This version of writeL is for trees which are known to be non-empty.
writeL' :: e -> AVL e -> AVL e
writeL' _   E        = error "writeL': Bug0"
writeL' e' (N l e r) = writeLN e' l e r -- l may be empty
writeL' e' (Z l e r) = writeLZ e' l e r -- l may be empty
writeL' e' (P l e r) = writeLP e' l e r -- l can't be empty

-- Write to left sub-tree of N l e r, or here if l is empty
writeLN :: e -> AVL e -> e -> AVL e -> AVL e
writeLN e'  E           _ r = N E e' r
writeLN e' (N ll le lr) e r = let l' = writeLN e' ll le lr in l' `seq` N l' e r
writeLN e' (Z ll le lr) e r = let l' = writeLZ e' ll le lr in l' `seq` N l' e r
writeLN e' (P ll le lr) e r = let l' = writeLP e' ll le lr in l' `seq` N l' e r

-- Write to left sub-tree of Z l e r, or here if l is empty
writeLZ :: e -> AVL e -> e -> AVL e -> AVL e
writeLZ e'  E           _ r = Z E e' r -- r must be E too!
writeLZ e' (N ll le lr) e r = let l' = writeLN e' ll le lr in l' `seq` Z l' e r
writeLZ e' (Z ll le lr) e r = let l' = writeLZ e' ll le lr in l' `seq` Z l' e r
writeLZ e' (P ll le lr) e r = let l' = writeLP e' ll le lr in l' `seq` Z l' e r

-- Write to left sub-tree of P l e r (l can't be empty)
{-# INLINE writeLP #-}
writeLP ::  e -> AVL e -> e -> AVL e -> AVL e
writeLP e'  l           e r = let l' = writeL' e' l in l' `seq` P l' e r
---------------------------------------------------------------------------
--                       writeL, tryWriteL end here                      --
---------------------------------------------------------------------------


---------------------------------------------------------------------------
--                       writeR, tryWriteR                               --
---------------------------------------------------------------------------
-- | Replace the right most element of a tree with the supplied new element.
-- This function raises an error if applied to an empty tree.
--
-- Complexity: O(log n)
writeR :: AVL e -> e -> AVL e
writeR  E        _  = error "writeR: Empty Tree"
writeR (N l e r) e' = writeRN l e r e'
writeR (Z l e r) e' = writeRZ l e r e'
writeR (P l e r) e' = writeRP l e r e'

-- | Similar to 'writeR', but returns 'Nothing' if applied to an empty tree.
--
-- Complexity: O(log n)
tryWriteR :: AVL e -> e -> Maybe (AVL e)
tryWriteR  E        _  = Nothing
tryWriteR (N l e r) e' = Just $! writeRN l e r e'
tryWriteR (Z l e r) e' = Just $! writeRZ l e r e'
tryWriteR (P l e r) e' = Just $! writeRP l e r e'

-- This version of writeR is for trees which are known to be non-empty.
writeR' :: AVL e -> e -> AVL e
writeR'  E        _  = error "writeR': Bug0"
writeR' (N l e r) e' = writeRN l e r e' -- r can't be empty
writeR' (Z l e r) e' = writeRZ l e r e' -- r may be empty
writeR' (P l e r) e' = writeRP l e r e' -- r may be empty

-- Write to right sub-tree of N l e r (r can't be empty)
{-# INLINE writeRN #-}
writeRN ::  AVL e -> e -> AVL e -> e -> AVL e
writeRN l e  r           e' = let r' = writeR' r e' in r' `seq` N l e r'

-- Write to right sub-tree of Z l e r, or here if r is empty
writeRZ :: AVL e -> e -> AVL e -> e -> AVL e
writeRZ l _  E           e' = Z l e' E -- l must be E too!
writeRZ l e (N rl re rr) e' = let r' = writeRN rl re rr e' in r' `seq` Z l e r'
writeRZ l e (Z rl re rr) e' = let r' = writeRZ rl re rr e' in r' `seq` Z l e r'
writeRZ l e (P rl re rr) e' = let r' = writeRP rl re rr e' in r' `seq` Z l e r'

-- Write to right sub-tree of P l e r, or here if r is empty
writeRP :: AVL e -> e -> AVL e -> e -> AVL e
writeRP l _  E           e' = P l e' E
writeRP l e (N rl re rr) e' = let r' = writeRN rl re rr e' in r' `seq` P l e r'
writeRP l e (Z rl re rr) e' = let r' = writeRZ rl re rr e' in r' `seq` P l e r'
writeRP l e (P rl re rr) e' = let r' = writeRP rl re rr e' in r' `seq` P l e r'
---------------------------------------------------------------------------
--                       writeR, tryWriteR end here                      --
---------------------------------------------------------------------------


-- | A general purpose function to perform a search of a tree, using the supplied selector.
-- If the search succeeds the found element is replaced by the value (@e@) of the @('Eq' e)@
-- constructor returned by the selector. If the search fails this function returns the original tree.
--
-- Complexity: O(log n)
write :: (e -> COrdering e) -> AVL e -> AVL e
write c t = case openPathWith c t of
            FullBP pth e -> writePath pth e t
            _            -> t

-- | Functionally identical to 'write', but returns an identical tree (one with all the nodes on
-- the path duplicated) if the search fails. This should probably only be used if you know the
-- search will succeed and will return an element which is different from that already present.
--
-- Complexity: O(log n)
writeFast :: (e -> COrdering e) -> AVL e -> AVL e
writeFast c = w where
 w   E        = E
 w  (N l e r) = case c e of
                Lt   -> let l' = w l in l' `seq` N l' e r
                Eq v -> N l v r
                Gt   -> let r' = w r in r' `seq` N l  e r'
 w  (Z l e r) = case c e of
                Lt   -> let l' = w l in l' `seq` Z l' e r
                Eq v -> Z l v r
                Gt   -> let r' = w r in r' `seq` Z l  e r'
 w  (P l e r) = case c e of
                Lt   -> let l' = w l in l' `seq` P l' e r
                Eq v -> P l v r
                Gt   -> let r' = w r in r' `seq` P l  e r'

-- | A general purpose function to perform a search of a tree, using the supplied selector.
-- The found element is replaced by the value (@e@) of the @('Eq' e)@ constructor returned by
-- the selector. This function returns 'Nothing' if the search failed.
--
-- Complexity: O(log n)
tryWrite :: (e -> COrdering e) -> AVL e -> Maybe (AVL e)
tryWrite c t = case openPathWith c t of
               FullBP pth e -> Just $! writePath pth e t
               _            -> Nothing

-- | Similar to 'write', but also returns the original tree if the search succeeds but
-- the selector returns @('Eq' 'Nothing')@. (This version is intended to help reduce heap burn
-- rate if it\'s likely that no modification of the value is needed.)
--
-- Complexity: O(log n)
writeMaybe :: (e -> COrdering (Maybe e)) -> AVL e -> AVL e
writeMaybe c t = case openPathWith c t of
                 FullBP pth (Just e) -> writePath pth e t
                 _                   -> t

-- | Similar to 'tryWrite', but also returns the original tree if the search succeeds but
-- the selector returns @('Eq' 'Nothing')@. (This version is intended to help reduce heap burn
-- rate if it\'s likely that no modification of the value is needed.)
--
-- Complexity: O(log n)
tryWriteMaybe :: (e -> COrdering (Maybe e)) -> AVL e -> Maybe (AVL e)
tryWriteMaybe c t = case openPathWith c t of
                    FullBP pth (Just e) -> Just $! writePath pth e t
                    FullBP _   Nothing  -> Just t
                    _                   -> Nothing


