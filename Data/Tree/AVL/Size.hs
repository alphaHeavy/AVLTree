{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Size
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- AVL Tree size related utilities.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Size
        (-- * AVL tree size utilities.
         size,addSize,clipSize,

#ifdef __GLASGOW_HASKELL__
         -- ** (GHC Only)
         addSize#,size#,
#endif
        ) where

import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.Height(addHeight)

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#include "ghcdefs.h"

-- | A convenience wrapper for 'addSize#'.
size :: AVL e -> Int
size t = ASINT(addSize# L(0) t)
{-# INLINE size #-}

-- | A convenience wrapper for 'addSize#'.
size# :: AVL e -> UINT
size# t = addSize# L(0) t
{-# INLINE size# #-}

-- | See 'addSize#'.
addSize :: Int -> AVL e -> Int
addSize ASINT(n) t = ASINT(addSize# n t)
{-# INLINE addSize #-}

#define AddSize addSize#
#else
#include "h98defs.h"

-- | A convenience wrapper for 'addSize'.
size :: AVL e -> Int
size t = addSize 0 t
{-# INLINE size #-}

#define AddSize addSize
#endif

{-----------------------------------------
Notes for fast size calculation.
 case (h,avl)
      (0,_      ) -> 0            -- Must be E
      (1,_      ) -> 1            -- Must be (Z  E        _  E       )
      (2,N _ _ _) -> 2            -- Must be (N  E        _ (Z E _ E))
      (2,Z _ _ _) -> 3            -- Must be (Z (Z E _ E) _ (Z E _ E))
      (2,P _ _ _) -> 2            -- Must be (P (Z E _ E) _  E       )
      (3,N _ _ r) -> 2 + size 2 r -- Must be (N (Z E _ E) _  r       )
      (3,P l _ _) -> 2 + size 2 l -- Must be (P  l        _ (Z E _ E))
------------------------------------------}

-- | Fast algorithm to add the size of a tree to the first argument. This avoids visiting about 50% of tree nodes
-- by using fact that trees with small heights can only have particular shapes.
-- So it's still O(n), but with substantial saving in constant factors.
--
-- Complexity: O(n)
AddSize :: UINT -> AVL e -> UINT
AddSize n E         = n
AddSize n (N l _ r) = case addHeight L(2) l of
                      L(2) -> INCINT2(n)
                      L(3) -> fas2 INCINT2(n) r
                      h    -> fasNP n h l r
AddSize n (Z l _ r) = case addHeight L(1) l of
                      L(1) -> INCINT1(n)
                      L(2) -> INCINT3(n)
                      L(3) -> fas2 (fas2 INCINT1(n) l) r
                      h    -> fasZ n h l r
AddSize n (P l _ r) = case addHeight L(2) r of
                      L(2) -> INCINT2(n)
                      L(3) -> fas2 INCINT2(n) l
                      h    -> fasNP n h r l
-- Parent Height (h) >= 4 !!
fasNP,fasZ :: UINT -> UINT -> AVL e -> AVL e -> UINT
fasNP n h l r = fasG3 (fasG2 INCINT1(n) DECINT2(h) l) DECINT1(h) r
fasZ  n h l r = fasG3 (fasG3 INCINT1(n) DECINT1(h) l) DECINT1(h) r
-- h>=2 !!
fasG2 :: UINT -> UINT -> AVL e -> UINT
fasG2 n L(2)  t        = fas2  n   t
fasG2 n h     t        = fasG3 n h t
{-# INLINE fasG2 #-}
-- h>=3 !!
fasG3 :: UINT -> UINT -> AVL e -> UINT
fasG3 n L(3) (N _ _ r) = fas2 INCINT2(n) r
fasG3 n L(3) (Z l _ r) = fas2 (fas2 INCINT1(n) l) r
fasG3 n L(3) (P l _ _) = fas2 INCINT2(n) l
fasG3 n h    (N l _ r) = fasNP n h l r -- h>=4
fasG3 n h    (Z l _ r) = fasZ  n h l r -- h>=4
fasG3 n h    (P l _ r) = fasNP n h r l -- h>=4
fasG3 _ _     E        = error "AddSize: Bad Tree." -- impossible
-- h=2 !!
fas2 :: UINT -> AVL e -> UINT
fas2 n (N _ _ _) = INCINT2(n)
fas2 n (Z _ _ _) = INCINT3(n)
fas2 n (P _ _ _) = INCINT2(n)
fas2 _  E        = error "AddSize: Bad Tree." -- impossible
{-# INLINE fas2 #-}
-----------------------------------------------------------------------
----------------------- fastAddSize Ends Here -------------------------
-----------------------------------------------------------------------

-- | Returns the exact tree size in the form @('Just' n)@ if this is less than or
-- equal to the input clip value. Returns @'Nothing'@ of the size is greater than
-- the clip value. This function exploits the same optimisation as 'addSize'.
--
-- Complexity: O(min n c) where n is tree size and c is clip value.
clipSize ::  Int -> AVL e -> Maybe Int
clipSize ASINT(c) t = let c_ = cSzh c t in if IS_TRUE(c_ LTN L(0))
                                           then Nothing
                                           else Just ASINT(SUBINT(c,c_))
-- First entry calculates initial height
cSzh :: UINT -> AVL e -> UINT
cSzh c  E        = c
cSzh c (N l _ r) = case addHeight L(2) l of
                   L(2) -> DECINT2(c)
                   L(3) -> cSzNP3 c     r
                   h    -> cSzNP  c h l r
cSzh c (Z l _ r) = case addHeight L(1) l of
                   L(1) -> DECINT1(c)
                   L(2) -> DECINT3(c)
                   L(3) -> cSzZ3 c   l r
                   h    -> cSzZ  c h l r
cSzh c (P l _ r) = case addHeight L(2) r of
                   L(2) -> DECINT2(c)
                   L(3) -> cSzNP3 c     l
                   h    -> cSzNP  c h r l
-- Parent Height = 3 !!
cSzNP3 :: UINT -> AVL e -> UINT
cSzNP3 c t = if IS_TRUE(c LTN L(4))
                             then L(-1)
                             else cSz2 DECINT2(c) t

cSzZ3  :: UINT -> AVL e -> AVL e -> UINT
cSzZ3  c l r = if IS_TRUE(c LTN L(5))
                              then L(-1)
                              else let c_ = cSz2 DECINT1(c) l
                                   in if IS_TRUE(c_ LTN L(2))
                                         then L(-1)
                                         else cSz2 c_ r
-- Parent Height (h) >= 4 !!
cSzNP,cSzZ :: UINT -> UINT -> AVL e -> AVL e -> UINT
cSzNP c h l r = if IS_TRUE(c LTN L(7))
                              then L(-1)
                              else let c_ = cSzG2 DECINT1(c) DECINT2(h) l       -- (h-2) >= 2
                                   in if IS_TRUE(c_ LTN L(4))
                                                     then L(-1)
                                                     else cSzG3 c_ DECINT1(h) r -- (h-1) >= 3
cSzZ c h l r = if IS_TRUE(c LTN L(9))
                             then L(-1)
                             else let c_ = cSzG3 DECINT1(c) DECINT1(h) l        -- (h-1) >= 3
                                  in if IS_TRUE(c_ LTN L(4))
                                                    then L(-1)
                                                    else cSzG3 c_ DECINT1(h) r  -- (h-1) >= 3
-- h>=2 !!
cSzG2 :: UINT -> UINT -> AVL e -> UINT
cSzG2 c L(2)  t        = cSz2  c   t
cSzG2 c h     t        = cSzG3 c h t
{-# INLINE cSzG2 #-}
-- h>=3 !!
cSzG3 :: UINT -> UINT -> AVL e -> UINT
cSzG3 c L(3) (N _ _ r) = cSzNP3 c   r
cSzG3 c L(3) (Z l _ r) = cSzZ3  c l r
cSzG3 c L(3) (P l _ _) = cSzNP3 c l
cSzG3 c h    (N l _ r) = cSzNP c h l r -- h>=4
cSzG3 c h    (Z l _ r) = cSzZ  c h l r -- h>=4
cSzG3 c h    (P l _ r) = cSzNP c h r l -- h>=4
cSzG3 _ _     E        = error "clipSize: Bad Tree." -- impossible
-- h=2 !!
cSz2 :: UINT -> AVL e -> UINT
cSz2 c (N _ _ _) = DECINT2(c)
cSz2 c (Z _ _ _) = DECINT3(c)
cSz2 c (P _ _ _) = DECINT2(c)
cSz2 _  E        = error "clipSize: Bad Tree." -- impossible
{-# INLINE cSz2 #-}
-----------------------------------------------------------------------
------------------------- clipSize Ends Here --------------------------
-----------------------------------------------------------------------

