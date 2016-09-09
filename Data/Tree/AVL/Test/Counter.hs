-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Test.Counter
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines the 'XInt' type which is a specialised instance of 'Ord' which allows
-- the number of comparisons performed to be counted. This may be used evaluate various
-- algorithms. The functions defined here are not exported by the main "Data.Tree.AVL"
-- module. You need to import this module explicitly if you want to use any of them.
-----------------------------------------------------------------------------
module Data.Tree.AVL.Test.Counter
        (XInt(..),
         getCount,resetCount,
        ) where

import System.IO.Unsafe(unsafePerformIO)
import Data.IORef(IORef,newIORef,readIORef,writeIORef)

{-# NOINLINE count #-}
count :: IORef Int
count = unsafePerformIO $ newIORef 0

-- Increment the counter.
incCount :: IO ()
incCount = do c <- readIORef count
              let c' = c+1 in c' `seq` writeIORef count c'

-- | Read the current comparison counter.
getCount :: IO Int
getCount = readIORef count

-- | Reset the comparison counter to zero.
resetCount :: IO ()
resetCount = writeIORef count 0

-- | Basic data type.
newtype XInt =  XInt Int deriving (Eq,Show,Read)

-- | A side effecting instance of Ord.
instance Ord XInt where
 compare (XInt x) (XInt y) = unsafePerformIO $ do incCount
                                                  return $! compare x y


