{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Deprecated
-- Copyright   :  (c) Adrian Hey 2004,2008
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  unstable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.Deprecated
(-- * Deprecated

 -- ** Deprecated names
 -- | These functions are all still available, but with more sensible names.
 -- They will dissapear on the next major version so you should amend your code
 -- accordingly soon.

 genUnion,genUnionMaybe,genDisjointUnion,genUnions,
 genDifference,genDifferenceMaybe,genSymDifference,
 genIntersection,genIntersectionMaybe,
 genIntersectionToListL,genIntersectionAsListL,
 genIntersectionMaybeToListL,genIntersectionMaybeAsListL,
 genVenn,genVennMaybe,
 genVennToList,genVennAsList,
 genVennMaybeToList,genVennMaybeAsList,
 genIsSubsetOf,genIsSubsetOfBy,

 genAssertRead,genTryRead,genTryReadMaybe,genDefaultRead,genContains,

 genWrite,genWriteFast,genTryWrite,genWriteMaybe,genTryWriteMaybe,

 genDel,genDelFast,genDelIf,genDelMaybe,
 genAssertPop,genTryPop,genAssertPopMaybe,genTryPopMaybe,genAssertPopIf,genTryPopIf,

 genPush,genPush',genPushMaybe,genPushMaybe',

 genAsTree,

 genForkL,genForkR,genFork,
 genTakeLE,genDropGT,
 genTakeLT,genDropGE,
 genTakeGT,genDropLE,
 genTakeGE,genDropLT,

 genAssertOpen,genTryOpen,
 genTryOpenGE,genTryOpenLE,
 genOpenEither,
 genOpenBAVL,

 genFindPath,genOpenPath,genOpenPathWith,

 fastAddSize,

 reverseAVL,mapAVL,mapAVL',
 mapAccumLAVL  ,mapAccumRAVL  ,
 mapAccumLAVL' ,mapAccumRAVL' ,
#ifdef __GLASGOW_HASKELL__
 mapAccumLAVL'',mapAccumRAVL'',
#endif
 replicateAVL,
 filterAVL,mapMaybeAVL,
 partitionAVL,
 foldrAVL,foldrAVL',foldr1AVL,foldr1AVL',foldr2AVL,foldr2AVL',
 foldlAVL,foldlAVL',foldl1AVL,foldl1AVL',foldl2AVL,foldl2AVL',
 foldrAVL_UINT,

 findPath,

{-
 -- ** Deprecated functions
 -- | Any functions listed here are deprecated, with no direct replacement.
 -- They will continue to live \"forever\" here, but should not be used
 -- (ideally).
-}

) where

import Prelude hiding (reverse,map,replicate,filter,foldr,foldr1,foldl,foldl1) -- so haddock finds the symbols there

import Data.COrdering(COrdering)
import Data.Tree.AVL.Types(AVL)
import Data.Tree.AVL.Set
import Data.Tree.AVL.Read
import Data.Tree.AVL.Write
import Data.Tree.AVL.Delete
import Data.Tree.AVL.Push
import Data.Tree.AVL.Split
import Data.Tree.AVL.List
import Data.Tree.AVL.Zipper
import Data.Tree.AVL.BinPath
import Data.Tree.AVL.Size

#ifdef __GLASGOW_HASKELL__
import GHC.Base(Int#)
#include "ghcdefs.h"
#else
#include "h98defs.h"
#endif

{-# DEPRECATED genUnion "This is now called union." #-}
-- | This name is /deprecated/. Instead use 'union'.
genUnion :: (e -> e -> COrdering e) -> AVL e -> AVL e -> AVL e
genUnion = union
{-# INLINE genUnion #-}

{-# DEPRECATED genUnionMaybe "This is now called unionMaybe." #-}
-- | This name is /deprecated/. Instead use 'unionMaybe'.
genUnionMaybe :: (e -> e -> COrdering (Maybe e)) -> AVL e -> AVL e -> AVL e
genUnionMaybe = unionMaybe
{-# INLINE genUnionMaybe #-}

{-# DEPRECATED genDisjointUnion "This is now called disjointUnion." #-}
-- | This name is /deprecated/. Instead use 'disjointUnion'.
genDisjointUnion :: (e -> e -> Ordering) -> AVL e -> AVL e -> AVL e
genDisjointUnion = disjointUnion
{-# INLINE genDisjointUnion #-}

{-# DEPRECATED genUnions "This is now called unions." #-}
-- | This name is /deprecated/. Instead use 'unions'.
genUnions :: (e -> e -> COrdering e) -> [AVL e] -> AVL e
genUnions = unions
{-# INLINE genUnions #-}

{-# DEPRECATED genDifference "This is now called difference." #-}
-- | This name is /deprecated/. Instead use 'difference'.
genDifference :: (a -> b -> Ordering) -> AVL a -> AVL b -> AVL a
genDifference = difference
{-# INLINE genDifference #-}

{-# DEPRECATED genDifferenceMaybe "This is now called differenceMaybe." #-}
-- | This name is /deprecated/. Instead use 'differenceMaybe'.
genDifferenceMaybe :: (a -> b -> COrdering (Maybe a)) -> AVL a -> AVL b -> AVL a
genDifferenceMaybe = differenceMaybe
{-# INLINE genDifferenceMaybe #-}

{-# DEPRECATED genSymDifference "This is now called symDifference." #-}
-- | This name is /deprecated/. Instead use 'symDifference'.
genSymDifference :: (e -> e -> Ordering) -> AVL e -> AVL e -> AVL e
genSymDifference = symDifference
{-# INLINE genSymDifference #-}

{-# DEPRECATED genIntersection "This is now called intersection." #-}
-- | This name is /deprecated/. Instead use 'intersection'.
genIntersection :: (a -> b -> COrdering c) -> AVL a -> AVL b -> AVL c
genIntersection = intersection
{-# INLINE genIntersection #-}

{-# DEPRECATED genIntersectionMaybe "This is now called intersectionMaybe." #-}
-- | This name is /deprecated/. Instead use 'intersectionMaybe'.
genIntersectionMaybe :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> AVL c
genIntersectionMaybe = intersectionMaybe
{-# INLINE genIntersectionMaybe #-}

{-# DEPRECATED genIntersectionToListL "This is now called intersectionToList." #-}
-- | This name is /deprecated/. Instead use 'intersectionToList'.
genIntersectionToListL :: (a -> b -> COrdering c) -> AVL a -> AVL b -> [c] -> [c]
genIntersectionToListL = intersectionToList
{-# INLINE genIntersectionToListL #-}

{-# DEPRECATED genIntersectionAsListL "This is now called intersectionAsList." #-}
-- | This name is /deprecated/. Instead use 'intersectionAsList'.
genIntersectionAsListL :: (a -> b -> COrdering c) -> AVL a -> AVL b -> [c]
genIntersectionAsListL = intersectionAsList
{-# INLINE genIntersectionAsListL #-}

{-# DEPRECATED genIntersectionMaybeToListL "This is now called intersectionMaybeToList." #-}
-- | This name is /deprecated/. Instead use 'intersectionMaybeToList'.
genIntersectionMaybeToListL :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> [c] -> [c]
genIntersectionMaybeToListL = intersectionMaybeToList
{-# INLINE genIntersectionMaybeToListL #-}

{-# DEPRECATED genIntersectionMaybeAsListL "This is now called intersectionMaybeAsList." #-}
-- | This name is /deprecated/. Instead use 'intersectionMaybeAsList'.
genIntersectionMaybeAsListL :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> [c]
genIntersectionMaybeAsListL = intersectionMaybeAsList
{-# INLINE genIntersectionMaybeAsListL #-}

{-# DEPRECATED genVenn "This is now called venn." #-}
-- | This name is /deprecated/. Instead use 'venn'.
genVenn :: (a -> b -> COrdering c) -> AVL a -> AVL b -> (AVL a, AVL c, AVL b)
genVenn = venn
{-# INLINE genVenn #-}

{-# DEPRECATED genVennMaybe "This is now called vennMaybe." #-}
-- | This name is /deprecated/. Instead use 'vennMaybe'.
genVennMaybe :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> (AVL a, AVL c, AVL b)
genVennMaybe = vennMaybe
{-# INLINE genVennMaybe #-}

{-# DEPRECATED genVennToList "This is now called vennToList." #-}
-- | This name is /deprecated/. Instead use 'vennToList'.
genVennToList :: (a -> b -> COrdering c) -> [c] -> AVL a -> AVL b -> (AVL a, [c], AVL b)
genVennToList = vennToList
{-# INLINE genVennToList #-}

{-# DEPRECATED genVennAsList "This is now called vennAsList." #-}
-- | This name is /deprecated/. Instead use 'vennAsList'.
genVennAsList :: (a -> b -> COrdering c) -> AVL a -> AVL b -> (AVL a, [c], AVL b)
genVennAsList = vennAsList
{-# INLINE genVennAsList #-}

{-# DEPRECATED genVennMaybeToList "This is now called vennMaybeToList." #-}
-- | This name is /deprecated/. Instead use 'vennMaybeToList'.
genVennMaybeToList  :: (a -> b -> COrdering (Maybe c)) -> [c] -> AVL a -> AVL b -> (AVL a, [c], AVL b)
genVennMaybeToList = vennMaybeToList
{-# INLINE genVennMaybeToList #-}

{-# DEPRECATED genVennMaybeAsList "This is now called vennMaybeAsList." #-}
-- | This name is /deprecated/. Instead use 'vennMaybeAsList'.
genVennMaybeAsList  :: (a -> b -> COrdering (Maybe c)) -> AVL a -> AVL b -> (AVL a, [c], AVL b)
genVennMaybeAsList = vennMaybeAsList
{-# INLINE genVennMaybeAsList #-}

{-# DEPRECATED genIsSubsetOf "This is now called isSubsetOf." #-}
-- | This name is /deprecated/. Instead use 'isSubsetOf'.
genIsSubsetOf :: (a -> b -> Ordering) -> AVL a -> AVL b -> Bool
genIsSubsetOf = isSubsetOf
{-# INLINE genIsSubsetOf #-}

{-# DEPRECATED genIsSubsetOfBy "This is now called isSubsetOfBy." #-}
-- | This name is /deprecated/. Instead use 'isSubsetOfBy'.
genIsSubsetOfBy :: (a -> b -> COrdering Bool) -> AVL a -> AVL b -> Bool
genIsSubsetOfBy = isSubsetOfBy
{-# INLINE genIsSubsetOfBy #-}

{-# DEPRECATED genAssertRead "This is now called assertRead." #-}
-- | This name is /deprecated/. Instead use 'assertRead'.
genAssertRead :: AVL e -> (e -> COrdering a) -> a
genAssertRead = assertRead
{-# INLINE genAssertRead #-}

{-# DEPRECATED genTryRead "This is now called tryRead." #-}
-- | This name is /deprecated/. Instead use 'tryRead'.
genTryRead :: AVL e -> (e -> COrdering a) ->  Maybe a
genTryRead = tryRead
{-# INLINE genTryRead #-}

{-# DEPRECATED genTryReadMaybe "This is now called tryReadMaybe." #-}
-- | This name is /deprecated/. Instead use 'tryReadMaybe'.
genTryReadMaybe :: AVL e -> (e -> COrdering (Maybe a)) ->  Maybe a
genTryReadMaybe = tryReadMaybe
{-# INLINE genTryReadMaybe #-}

{-# DEPRECATED genDefaultRead "This is now called defaultRead." #-}
-- | This name is /deprecated/. Instead use 'defaultRead'.
genDefaultRead :: a -> AVL e -> (e -> COrdering a) -> a
genDefaultRead = defaultRead
{-# INLINE genDefaultRead #-}

{-# DEPRECATED genContains "This is now called contains." #-}
-- | This name is /deprecated/. Instead use 'contains'.
genContains :: AVL e -> (e -> Ordering) -> Bool
genContains = contains
{-# INLINE genContains #-}

{-# DEPRECATED genWrite "This is now called write." #-}
-- | This name is /deprecated/. Instead use 'write'.
genWrite :: (e -> COrdering e) -> AVL e -> AVL e
genWrite = write
{-# INLINE genWrite #-}

{-# DEPRECATED genWriteFast "This is now called writeFast." #-}
-- | This name is /deprecated/. Instead use 'writeFast'.
genWriteFast :: (e -> COrdering e) -> AVL e -> AVL e
genWriteFast = writeFast
{-# INLINE genWriteFast #-}

{-# DEPRECATED genTryWrite "This is now called tryWrite." #-}
-- | This name is /deprecated/. Instead use 'tryWrite'.
genTryWrite :: (e -> COrdering e) -> AVL e -> Maybe (AVL e)
genTryWrite = tryWrite
{-# INLINE genTryWrite #-}

{-# DEPRECATED genWriteMaybe "This is now called writeMaybe." #-}
-- | This name is /deprecated/. Instead use 'writeMaybe'.
genWriteMaybe :: (e -> COrdering (Maybe e)) -> AVL e -> AVL e
genWriteMaybe = writeMaybe
{-# INLINE genWriteMaybe #-}

{-# DEPRECATED genTryWriteMaybe "This is now called tryWriteMaybe." #-}
-- | This name is /deprecated/. Instead use 'tryWriteMaybe'.
genTryWriteMaybe :: (e -> COrdering (Maybe e)) -> AVL e -> Maybe (AVL e)
genTryWriteMaybe = tryWriteMaybe
{-# INLINE genTryWriteMaybe #-}

{-# DEPRECATED genDel "This is now called delete." #-}
-- | This name is /deprecated/. Instead use 'delete'.
genDel :: (e -> Ordering) -> AVL e -> AVL e
genDel = delete
{-# INLINE genDel #-}

{-# DEPRECATED genDelFast "This is now called deleteFast." #-}
-- | This name is /deprecated/. Instead use 'deleteFast'.
genDelFast :: (e -> Ordering) -> AVL e -> AVL e
genDelFast = deleteFast
{-# INLINE genDelFast #-}

{-# DEPRECATED genDelIf "This is now called deleteIf." #-}
-- | This name is /deprecated/. Instead use 'deleteIf'.
genDelIf :: (e -> COrdering Bool) -> AVL e -> AVL e
genDelIf = deleteIf
{-# INLINE genDelIf #-}

{-# DEPRECATED genDelMaybe "This is now called deleteMaybe." #-}
-- | This name is /deprecated/. Instead use 'deleteMaybe'.
genDelMaybe :: (e -> COrdering (Maybe e)) -> AVL e -> AVL e
genDelMaybe = deleteMaybe
{-# INLINE genDelMaybe #-}

{-# DEPRECATED genAssertPop "This is now called assertPop." #-}
-- | This name is /deprecated/. Instead use 'assertPop'.
genAssertPop :: (e -> COrdering a) -> AVL e -> (a,AVL e)
genAssertPop = assertPop
{-# INLINE genAssertPop #-}

{-# DEPRECATED genTryPop "This is now called tryPop." #-}
-- | This name is /deprecated/. Instead use 'tryPop'.
genTryPop :: (e -> COrdering a) -> AVL e -> Maybe (a,AVL e)
genTryPop = tryPop
{-# INLINE genTryPop #-}

{-# DEPRECATED genAssertPopMaybe "This is now called assertPopMaybe." #-}
-- | This name is /deprecated/. Instead use 'assertPopMaybe'.
genAssertPopMaybe :: (e -> COrdering (a,Maybe e)) -> AVL e -> (a,AVL e)
genAssertPopMaybe = assertPopMaybe
{-# INLINE genAssertPopMaybe #-}

{-# DEPRECATED genTryPopMaybe "This is now called tryPopMaybe." #-}
-- | This name is /deprecated/. Instead use 'tryPopMaybe'.
genTryPopMaybe :: (e -> COrdering (a,Maybe e)) -> AVL e -> Maybe (a,AVL e)
genTryPopMaybe = tryPopMaybe
{-# INLINE genTryPopMaybe #-}

{-# DEPRECATED genAssertPopIf "This is now called assertPopIf." #-}
-- | This name is /deprecated/. Instead use 'assertPopIf'.
genAssertPopIf :: (e -> COrdering (a,Bool)) -> AVL e -> (a,AVL e)
genAssertPopIf = assertPopIf
{-# INLINE genAssertPopIf #-}

{-# DEPRECATED genTryPopIf "This is now called tryPopIf." #-}
-- | This name is /deprecated/. Instead use 'tryPopIf'.
genTryPopIf :: (e -> COrdering (a,Bool)) -> AVL e -> Maybe (a,AVL e)
genTryPopIf = tryPopIf
{-# INLINE genTryPopIf #-}

{-# DEPRECATED genPush "This is now called push." #-}
-- | This name is /deprecated/. Instead use 'push'.
genPush :: (e -> COrdering e) -> e -> AVL e -> AVL e
genPush = push
{-# INLINE genPush #-}

{-# DEPRECATED genPush' "This is now called  push'." #-}
-- | This name is /deprecated/. Instead use ' push''.
genPush' :: (e -> COrdering e) -> e -> AVL e -> AVL e
genPush' = push'
{-# INLINE genPush' #-}

{-# DEPRECATED genPushMaybe "This is now called pushMaybe." #-}
-- | This name is /deprecated/. Instead use 'pushMaybe'.
genPushMaybe :: (e -> COrdering (Maybe e)) -> e -> AVL e -> AVL e
genPushMaybe = pushMaybe
{-# INLINE genPushMaybe #-}

{-# DEPRECATED genPushMaybe' "This is now called pushMaybe'." #-}
-- | This name is /deprecated/. Instead use 'pushMaybe''.
genPushMaybe' :: (e -> COrdering (Maybe e)) -> e -> AVL e -> AVL e
genPushMaybe' = pushMaybe'
{-# INLINE genPushMaybe' #-}

{-# DEPRECATED genAsTree "This is now called asTree." #-}
-- | This name is /deprecated/. Instead use 'asTree'.
genAsTree :: (e -> e -> COrdering e) -> [e] -> AVL e
genAsTree = asTree
{-# INLINE genAsTree #-}

{-# DEPRECATED genForkL "This is now called forkL." #-}
-- | This name is /deprecated/. Instead use 'forkL'.
genForkL :: (e -> Ordering) -> AVL e -> (AVL e, AVL e)
genForkL = forkL
{-# INLINE genForkL #-}

{-# DEPRECATED genForkR "This is now called forkR." #-}
-- | This name is /deprecated/. Instead use 'forkR'.
genForkR :: (e -> Ordering) -> AVL e -> (AVL e, AVL e)
genForkR = forkR
{-# INLINE genForkR #-}

{-# DEPRECATED genFork "This is now called fork." #-}
-- | This name is /deprecated/. Instead use 'fork'.
genFork :: (e -> COrdering a) -> AVL e -> (AVL e, Maybe a, AVL e)
genFork = fork
{-# INLINE genFork #-}

{-# DEPRECATED genTakeLE "This is now called takeLE." #-}
-- | This name is /deprecated/. Instead use 'takeLE'.
genTakeLE :: (e -> Ordering) -> AVL e -> AVL e
genTakeLE = takeLE
{-# INLINE genTakeLE #-}

{-# DEPRECATED genDropGT "This is now called dropGT." #-}
-- | This name is /deprecated/. Instead use 'dropGT'.
genDropGT :: (e -> Ordering) -> AVL e -> AVL e
genDropGT = dropGT
{-# INLINE genDropGT #-}

{-# DEPRECATED genTakeLT "This is now called takeLT." #-}
-- | This name is /deprecated/. Instead use 'takeLT'.
genTakeLT :: (e -> Ordering) -> AVL e -> AVL e
genTakeLT = takeLT
{-# INLINE genTakeLT #-}

{-# DEPRECATED genDropGE "This is now called dropGE." #-}
-- | This name is /deprecated/. Instead use 'dropGE'.
genDropGE :: (e -> Ordering) -> AVL e -> AVL e
genDropGE = dropGE
{-# INLINE genDropGE #-}

{-# DEPRECATED genTakeGT "This is now called takeGT." #-}
-- | This name is /deprecated/. Instead use 'takeGT'.
genTakeGT :: (e -> Ordering) -> AVL e -> AVL e
genTakeGT = takeGT
{-# INLINE genTakeGT #-}

{-# DEPRECATED genDropLE "This is now called dropLE." #-}
-- | This name is /deprecated/. Instead use 'dropLE'.
genDropLE :: (e -> Ordering) -> AVL e -> AVL e
genDropLE = dropLE
{-# INLINE genDropLE #-}

{-# DEPRECATED genTakeGE "This is now called takeGE." #-}
-- | This name is /deprecated/. Instead use 'takeGE'.
genTakeGE :: (e -> Ordering) -> AVL e -> AVL e
genTakeGE = takeGE
{-# INLINE genTakeGE #-}

{-# DEPRECATED genDropLT "This is now called dropLT." #-}
-- | This name is /deprecated/. Instead use 'dropLT'.
genDropLT :: (e -> Ordering) -> AVL e -> AVL e
genDropLT = dropLT
{-# INLINE genDropLT #-}

{-# DEPRECATED genAssertOpen "This is now called assertOpen." #-}
-- | This name is /deprecated/. Instead use 'assertOpen'.
genAssertOpen :: (e -> Ordering) -> AVL e -> ZAVL e
genAssertOpen = assertOpen
{-# INLINE genAssertOpen #-}

{-# DEPRECATED genTryOpen "This is now called tryOpen." #-}
-- | This name is /deprecated/. Instead use 'tryOpen'.
genTryOpen :: (e -> Ordering) -> AVL e -> Maybe (ZAVL e)
genTryOpen = tryOpen
{-# INLINE genTryOpen #-}

{-# DEPRECATED genTryOpenGE "This is now called tryOpenGE." #-}
-- | This name is /deprecated/. Instead use 'tryOpenGE'.
genTryOpenGE :: (e -> Ordering) -> AVL e -> Maybe (ZAVL e)
genTryOpenGE = tryOpenGE
{-# INLINE genTryOpenGE #-}

{-# DEPRECATED genTryOpenLE "This is now called tryOpenLE." #-}
-- | This name is /deprecated/. Instead use 'tryOpenLE'.
genTryOpenLE :: (e -> Ordering) -> AVL e -> Maybe (ZAVL e)
genTryOpenLE = tryOpenLE
{-# INLINE genTryOpenLE #-}

{-# DEPRECATED genOpenEither "This is now called openEither." #-}
-- | This name is /deprecated/. Instead use 'openEither'.
genOpenEither :: (e -> Ordering) -> AVL e -> Either (PAVL e) (ZAVL e)
genOpenEither = openEither
{-# INLINE genOpenEither #-}

{-# DEPRECATED genOpenBAVL "This is now called openBAVL." #-}
-- | This name is /deprecated/. Instead use 'openBAVL'.
genOpenBAVL :: (e -> Ordering) -> AVL e -> BAVL e
genOpenBAVL = openBAVL
{-# INLINE genOpenBAVL #-}

{-# DEPRECATED genFindPath "This is now called findPath." #-}
-- | This name is /deprecated/. Instead use 'findPath'.
genFindPath :: (e -> Ordering) -> AVL e -> UINT
genFindPath = findPath
{-# INLINE genFindPath #-}

{-# DEPRECATED genOpenPath "This is now called openPath." #-}
-- | This name is /deprecated/. Instead use 'openPath'.
genOpenPath :: (e -> Ordering) -> AVL e -> BinPath e
genOpenPath = openPath
{-# INLINE genOpenPath #-}

{-# DEPRECATED genOpenPathWith "This is now called openPathWith." #-}
-- | This name is /deprecated/. Instead use 'openPathWith'.
genOpenPathWith :: (e -> COrdering a) -> AVL e -> BinPath a
genOpenPathWith = openPathWith
{-# INLINE genOpenPathWith #-}

{-# DEPRECATED fastAddSize "Use addSize or addSize#." #-}
-- | This name is /deprecated/. Instead use 'addSize' or 'addSize#'.
fastAddSize :: UINT -> AVL e -> UINT
#ifdef __GLASGOW_HASKELL__
fastAddSize = addSize#
#else
fastAddSize = addSize
#endif
{-# INLINE fastAddSize #-}



{-# DEPRECATED reverseAVL "This is now called reverse." #-}
-- | This name is /deprecated/. Instead use 'reverse'.
reverseAVL :: AVL e -> AVL e
reverseAVL = reverse
{-# INLINE reverseAVL #-}

{-# DEPRECATED mapAVL "This is now called map." #-}
-- | This name is /deprecated/. Instead use 'map'.
mapAVL :: (a -> b) -> AVL a -> AVL b
mapAVL = map
{-# INLINE mapAVL #-}

{-# DEPRECATED mapAVL' "This is now called map'." #-}
-- | This name is /deprecated/. Instead use 'map''.
mapAVL' :: (a -> b) -> AVL a -> AVL b
mapAVL' = map'
{-# INLINE mapAVL' #-}

{-# DEPRECATED mapAccumLAVL "This is now called mapAccumL." #-}
-- | This name is /deprecated/. Instead use 'mapAccumL'.
mapAccumLAVL :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumLAVL = mapAccumL
{-# INLINE mapAccumLAVL #-}

{-# DEPRECATED mapAccumRAVL "This is now called mapAccumR." #-}
-- | This name is /deprecated/. Instead use 'mapAccumR'.
mapAccumRAVL :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumRAVL = mapAccumR
{-# INLINE mapAccumRAVL #-}

{-# DEPRECATED mapAccumLAVL' "This is now called mapAccumL'." #-}
-- | This name is /deprecated/. Instead use 'mapAccumL''.
mapAccumLAVL' :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumLAVL' = mapAccumL'
{-# INLINE mapAccumLAVL' #-}

{-# DEPRECATED mapAccumRAVL' "This is now called mapAccumR'." #-}
-- | This name is /deprecated/. Instead use 'mapAccumR''.
mapAccumRAVL' :: (z -> a -> (z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumRAVL' = mapAccumR'
{-# INLINE mapAccumRAVL' #-}

#ifdef __GLASGOW_HASKELL__
{-# DEPRECATED mapAccumLAVL'' "This is now called mapAccumL''." #-}
-- | This name is /deprecated/. Instead use 'mapAccumL'''.
mapAccumLAVL''
               :: (z -> a -> UBT2(z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumLAVL'' = mapAccumL''
{-# INLINE mapAccumLAVL'' #-}

{-# DEPRECATED mapAccumRAVL'' "This is now called mapAccumR''." #-}
-- | This name is /deprecated/. Instead use 'mapAccumR'''.
mapAccumRAVL''
               :: (z -> a -> UBT2(z, b)) -> z -> AVL a -> (z, AVL b)
mapAccumRAVL'' = mapAccumR''
{-# INLINE mapAccumRAVL'' #-}

{-# DEPRECATED foldrAVL_UINT "This is now called foldrInt#." #-}
-- | This name is /deprecated/. Instead use 'foldrInt#'.
foldrAVL_UINT :: (e -> UINT -> UINT) -> UINT -> AVL e -> UINT
foldrAVL_UINT = foldrInt#
{-# INLINE foldrAVL_UINT #-}

#else

{-# DEPRECATED foldrAVL_UINT "This is deprecated, use foldr'." #-}
-- | This name is /deprecated/. Instead use 'foldr''.
foldrAVL_UINT :: (e -> UINT -> UINT) -> UINT -> AVL e -> UINT
foldrAVL_UINT = foldr'
{-# INLINE foldrAVL_UINT #-}

#endif

{-# DEPRECATED replicateAVL "This is now called replicate." #-}
-- | This name is /deprecated/. Instead use 'replicate'.
replicateAVL :: Int -> e -> AVL e
replicateAVL = replicate
{-# INLINE replicateAVL #-}

{-# DEPRECATED filterAVL "This is now called filter." #-}
-- | This name is /deprecated/. Instead use 'filter'.
filterAVL :: (e -> Bool) -> AVL e -> AVL e
filterAVL = filter
{-# INLINE filterAVL #-}

{-# DEPRECATED mapMaybeAVL "This is now called mapMaybe." #-}
-- | This name is /deprecated/. Instead use 'mapMaybe'.
mapMaybeAVL :: (a -> Maybe b) -> AVL a -> AVL b
mapMaybeAVL = mapMaybe
{-# INLINE mapMaybeAVL #-}

{-# DEPRECATED partitionAVL "This is now called partition." #-}
-- | This name is /deprecated/. Instead use 'partition'.
partitionAVL :: (e -> Bool) -> AVL e -> (AVL e, AVL e)
partitionAVL = partition
{-# INLINE partitionAVL #-}

{-# DEPRECATED foldrAVL "This is now called foldr." #-}
-- | This name is /deprecated/. Instead use 'foldr'.
foldrAVL :: (e -> a -> a) -> a -> AVL e -> a
foldrAVL = foldr
{-# INLINE foldrAVL #-}

{-# DEPRECATED foldrAVL' "This is now called foldr'." #-}
-- | This name is /deprecated/. Instead use 'foldr''.
foldrAVL' :: (e -> a -> a) -> a -> AVL e -> a
foldrAVL' = foldr'
{-# INLINE foldrAVL' #-}

{-# DEPRECATED foldr1AVL "This is now called foldr1." #-}
-- | This name is /deprecated/. Instead use 'foldr1'.
foldr1AVL :: (e -> e -> e) -> AVL e -> e
foldr1AVL = foldr1
{-# INLINE foldr1AVL #-}

{-# DEPRECATED foldr1AVL' "This is now called foldr1'." #-}
-- | This name is /deprecated/. Instead use 'foldr1''.
foldr1AVL' :: (e -> e -> e) -> AVL e -> e
foldr1AVL' = foldr1'
{-# INLINE foldr1AVL' #-}

{-# DEPRECATED foldr2AVL "This is now called foldr2." #-}
-- | This name is /deprecated/. Instead use 'foldr2'.
foldr2AVL :: (e -> a -> a) -> (e -> a) -> AVL e -> a
foldr2AVL = foldr2
{-# INLINE foldr2AVL #-}

{-# DEPRECATED foldr2AVL' "This is now called foldr2'." #-}
-- | This name is /deprecated/. Instead use 'foldr2''.
foldr2AVL' :: (e -> a -> a) -> (e -> a) -> AVL e -> a
foldr2AVL' = foldr2'
{-# INLINE foldr2AVL' #-}

{-# DEPRECATED foldlAVL "This is now called foldl." #-}
-- | This name is /deprecated/. Instead use 'foldl'.
foldlAVL :: (a -> e -> a) -> a -> AVL e -> a
foldlAVL = foldl
{-# INLINE foldlAVL #-}

{-# DEPRECATED foldlAVL' "This is now called foldl'." #-}
-- | This name is /deprecated/. Instead use 'foldl''.
foldlAVL' :: (a -> e -> a) -> a -> AVL e -> a
foldlAVL' = foldl'
{-# INLINE foldlAVL' #-}

{-# DEPRECATED foldl1AVL "This is now called foldl1." #-}
-- | This name is /deprecated/. Instead use 'foldl1'.
foldl1AVL :: (e -> e -> e) -> AVL e -> e
foldl1AVL = foldl1
{-# INLINE foldl1AVL #-}

{-# DEPRECATED foldl1AVL' "This is now called foldl1'." #-}
-- | This name is /deprecated/. Instead use 'foldl1''.
foldl1AVL' :: (e -> e -> e) -> AVL e -> e
foldl1AVL' = foldl1'
{-# INLINE foldl1AVL' #-}

{-# DEPRECATED foldl2AVL "This is now called foldl2." #-}
-- | This name is /deprecated/. Instead use 'foldl2'.
foldl2AVL :: (a -> e -> a) -> (e -> a) -> AVL e -> a
foldl2AVL = foldl2
{-# INLINE foldl2AVL #-}

{-# DEPRECATED foldl2AVL' "This is now called foldl2'." #-}
-- | This name is /deprecated/. Instead use 'foldl2''.
foldl2AVL' :: (a -> e -> a) -> (e -> a) -> AVL e -> a
foldl2AVL' = foldl2'
{-# INLINE foldl2AVL' #-}

{-# DEPRECATED findPath "This is now called findFullPath." #-}
-- | This name is /deprecated/. Instead use 'findFullPath'.
findPath :: (e -> Ordering) -> AVL e -> UINT
findPath = findFullPath
{-# INLINE findPath #-}
