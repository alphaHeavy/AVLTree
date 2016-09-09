-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.AVL.Push
-- Copyright   :  (c) Adrian Hey 2004,2005
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.Tree.AVL.Push
(-- * \"Pushing\" new elements into AVL trees
 -- | \"Pushing\" is another word for insertion. (c.f \"Popping\".)

 -- ** Pushing on extreme left or right
 pushL,pushR,

 -- ** Pushing on /sorted/ AVL trees
 push,push',pushMaybe,pushMaybe',
) where

import Prelude -- so haddock finds the symbols there

import Data.COrdering
import Data.Tree.AVL.Types(AVL(..))
import Data.Tree.AVL.BinPath(BinPath(..),openPathWith,writePath,insertPath)

{------------------------------------------------------------------------------------------------------------------------------
 -------------------------------------- Notes about Insertion and Rebalancing -------------------------------------------------
 ------------------------------------------------------------------------------------------------------------------------------
   If we forget about tree rebalancing, and consider what changes in BF tell us about changes in H
   under ordinary circumstances, we can make the following observations:

   (1) Insertion can never reduce the height of a (sub)tree.
   (2) Insertion can only change the height of a (sub)tree by +1 at most. Therefore the BF of the
       root can change by +/- 1 most.
   (2) If insertion changes the BF from 0 -> +/- 1, then this must be because either the left or
       right subtrees has grown in height by 1. Since they were equal before (BF=0), the overall
       height of the root must also have grown by 1.
   (3) If insertion changes the BF from +/-1 -> 0, then this must be because one either the left
       or right subtree has grown by 1 so that it is now equal in height to the opposing subtree.
       Since height of the root is determined by the maximum height of the subtrees, it is left
       unchanged.
   (4) If insertion leaves the BF unchanged, then this must be because the height of neither
       subtree has changed. Therefore the height of the root is left unchanged.
   (5) It follows from (2) and (3), that changes in height, and hence BF can (and will) propogate
       up the tree (along the insertion path) as far as the first node with non-zero BF, and no further.
   (6) If insertion changes the BF from +/-1 -> +/-2 then we have a problem. This is dealt with by
       one of four possible rebalancing 'rotations' (there are two possiblities for each of the left
       and right subtrees). However, it's appropriate to mention an important property of the rotations
       now. The net effect of unbalancing and rebalancing is to give the root BF=0 and leave the height
       unchanged. So the combined effect of the unbalance-rebalance operation appears like a special
       case of (3). Another important property of rebalancing is that it /preserves/ the tree sorting.
   (7) It follows from (6) and (5) any single insertion will cause most one unbalance-rebalance operation.

   So in summary we have a set of rules to enable us to infer changes in height of a subtree (if any) from
   changes in the BF of the subtree, and hence the changes (if any) in the BF of the root. The rules are:
      BF    0 -> +/-1, height increased by 1
      BF +/-1 ->    0, height unchanged.
      BF unchanged   , height unchanged.
      BF +/-1 -> -/+1, NEVER OCCURS

   It should also be observed that these observations and rules apply to INSERTION only (not deletion).

Rebalancing: CASE RR
--------------------
   Consider inserting into the right subtree of the right subtree (RR subtree). From the obsevations above we can
   say this is only going to unbalance the root if:
           The height of the RR subtree is increased by 1 (we determine this from looking at changes in it's BF)
   ..and.. The right subtree has BF=0 prior to insertion (observation 5)
   ..and.. THe root has BF=-1 prior to insertion (observation 2)

   In pictures..

             -----                                       -----                                            -----
            |  B  |                                     |  B  |                                          |  D  |
            |H=h+2|                                     |H=h+3|                                          |H=h+2| <- Note
            |BF=-1|                                     |BF=-2| <-- Unbalanced!                          |BF= 0| <- Note
            /-----\                                     /-----\                                          /-----\
           /       \                                   /       \                                        /       \
          /         \                                 /         \                                      /         \
    -----/           \-----                     -----/           \-----                          -----/           \-----
   |  A  |           |  D  |       E grows     |  A  |           |  D  |        Rebalance       |  B  |           |  E  |
   | H=h |           |H=h+1|       by 1        | H=h |           |H=h+2|        -------->       |H=h+1|           |H=h+1|
   |     |           |BF= 0|       ------>     |     |           |BF=-1|                        |BF= 0|           |     |
    -----            /-----\       h -> h+1     -----            /-----\                        /-----\            -----
                    /       \                                   /       \                      /       \
                   /         \                                 /         \                    /         \
             -----/           \-----                     -----/           \-----        -----/           \-----
            |  C  |           |  E  |                   |  C  |           |  E  |      |  A  |           |  C  |
            | H=h |           | H=h |                   | H=h |           |H=h+1|      | H=h |           | H=h |
            |     |           |     |                   |     |           |     |      |     |           |     |
             -----             -----                     -----             -----        -----             -----

  Unfortunately, if you try this for insertion into the right left subtree (C) it doesn't work. To deal with
  this case we need a more complicated re-balancing rotation involving 3 nodes. There are 2 distinct cases, which
  both use the same rotation, but details re. BF and H are different.

Rebalancing: CASE RL(1)
-----------------------

             -----                                       -----                                         -----
            |  B  |                                     |  B  |                                       |  D  |
            |H=h+3|                                     |H=h+4|                                       |H=h+3| <- Note
            |BF=-1|                                     |BF=-2| <-- Unbalanced!                       |BF= 0| <- Note
            /-----\                                     /-----\                                       /-----\
           /       \                                   /       \                                     /       \
          /         \                                 /         \                                   /         \
    -----/           \-----                     -----/           \-----                            /           \
   |  A  |           |  F  |       E grows     |  A  |           |  F  |       Rebalance     -----/             \-----
   |H=h+1|           |H=h+2|       by 1        |H=h+1|           |H=h+3|       -------->    |  B  |             |  F  |
   |     |           |BF= 0|       ------>     |     |           |BF=+1|                    |H=h+2|             |H=h+2|
    -----            /-----\       h -> h+1     -----            /-----\                    |BF=+1|             |BF= 0|
                    /       \                                   /       \              -----/-----\-----   -----/-----\-----
                   /         \                                 /         \            |  A  |     |  C  | |  E  |     |  G  |
             -----/           \-----                     -----/           \-----      |H=h+1|     | H=h | |H=h+1|     |H=h+1|
            |  D  |           |  G  |                   |  D  |           |  G  |     |     |     |     | |     |     |     |
            |H=h+1|           |H=h+1|                   |H=h+2|           |H=h+1|      -----       -----   -----       -----
            |BF= 0|           |     |                   |BF=-1|           |     |
            /-----\            -----                    /-----\            -----
           /       \                                   /       \
          /         \                                 /         \
    -----/           \-----                     -----/           \-----
   |  C  |           |  E  |                   |  C  |           |  E  |
   | H=h |           | H=h |                   | H=h |           |H=h+1|
   |     |           |     |                   |     |           |     |
    -----             -----                     -----             -----

Rebalancing: CASE RL(2)
-----------------------

             -----                                       -----                                         -----
            |  B  |                                     |  B  |                                       |  D  |
            |H=h+3|                                     |H=h+4|                                       |H=h+3| <- Note
            |BF=-1|                                     |BF=-2| <-- Unbalanced!                       |BF= 0| <- Note
            /-----\                                     /-----\                                       /-----\
           /       \                                   /       \                                     /       \
          /         \                                 /         \                                   /         \
    -----/           \-----                     -----/           \-----                            /           \
   |  A  |           |  F  |       C grows     |  A  |           |  F  |       Rebalance     -----/             \-----
   |H=h+1|           |H=h+2|       by 1        |H=h+1|           |H=h+3|       -------->    |  B  |             |  F  |
   |     |           |BF= 0|       ------>     |     |           |BF=+1|                    |H=h+2|             |H=h+2|
    -----            /-----\       h -> h+1     -----            /-----\                    |BF= 0|             |BF=-1|
                    /       \                                   /       \              -----/-----\-----   -----/-----\-----
                   /         \                                 /         \            |  A  |     |  C  | |  E  |     |  G  |
             -----/           \-----                     -----/           \-----      |H=h+1|     |H=h+1| | H=h |     |H=h+1|
            |  D  |           |  G  |                   |  D  |           |  G  |     |     |     |     | |     |     |     |
            |H=h+1|           |H=h+1|                   |H=h+2|           |H=h+1|      -----       -----   -----       -----
            |BF= 0|           |     |                   |BF=+1|           |     |
            /-----\            -----                    /-----\            -----
           /       \                                   /       \
          /         \                                 /         \
    -----/           \-----                     -----/           \-----
   |  C  |           |  E  |                   |  C  |           |  E  |
   | H=h |           | H=h |                   |H=h+1|           | H=h |
   |     |           |     |                   |     |           |     |
    -----             -----                     -----             -----
-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------}

-- | General push. This function searches the AVL tree using the supplied selector. If a matching element
-- is found it's replaced by the value (@e@) returned in the @('Eq' e)@ constructor returned by the selector.
-- If no match is found then the default element value is added at in the appropriate position in the tree.
--
-- Note that for this to work properly requires that the selector behave as if it were comparing the
-- (potentially) new default element with existing tree elements, even if it isn't.
--
-- Note also that this function is /non-strict/ in it\'s second argument (the default value which
-- is inserted if the search fails or is discarded if the search succeeds). If you want
-- to force evaluation, but only if it\'s actually incorprated in the tree, then use 'push''
--
-- Complexity: O(log n)
push :: (e -> COrdering e) -> e -> AVL e -> AVL e
push c e0 = put where -- there now follows a huge collection of functions requiring
                         -- pattern matching from hell in which c and e0 are free variables
-- This may look longwinded, it's been done this way to..
--  * Avoid doing case analysis on the same node more than once.
--  * Minimise heap burn rate (by avoiding explicit rebalancing operations).
 ----------------------------- LEVEL 0 ---------------------------------
 --                              put                                  --
 -----------------------------------------------------------------------
 put  E        = Z    E e0 E
 put (N l e r) = putN l e  r
 put (Z l e r) = putZ l e  r
 put (P l e r) = putP l e  r

 ----------------------------- LEVEL 1 ---------------------------------
 --                       putN, putZ, putP                            --
 -----------------------------------------------------------------------

 -- Put in (N l e r), BF=-1  , (never returns P)
 putN l e r = case c e of
              Lt    -> putNL l e  r  -- <e, so put in L subtree
              Eq e' -> N     l e' r  -- =e, so update existing
              Gt    -> putNR l e  r  -- >e, so put in R subtree

 -- Put in (Z l e r), BF= 0
 putZ l e r = case c e of
              Lt    -> putZL l e  r  -- <e, so put in L subtree
              Eq e' -> Z     l e' r  -- =e, so update existing
              Gt    -> putZR l e  r  -- >e, so put in R subtree

 -- Put in (P l e r), BF=+1 , (never returns N)
 putP l e r = case c e of
              Lt    -> putPL l e  r  -- <e, so put in L subtree
              Eq e' -> P     l e' r  -- =e, so update existing
              Gt    -> putPR l e  r  -- >e, so put in R subtree

 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNL, putZL, putPL                          --
 --                      putNR, putZR, putPR                          --
 -----------------------------------------------------------------------

 -- (putNL l e r): Put in L subtree of (N l e r), BF=-1 (Never requires rebalancing) , (never returns P)
 {-# INLINE putNL #-}
 putNL  E           e r = Z (Z    E  e0 E ) e r       -- L subtree empty, H:0->1, parent BF:-1-> 0
 putNL (N ll le lr) e r = let l' = putN ll le lr      -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (P ll le lr) e r = let l' = putP ll le lr      -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (Z ll le lr) e r = let l' = putZ ll le lr      -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          E       -> error "push: Bug0" -- impossible
                          Z _ _ _ -> N l' e r         -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                          _       -> Z l' e r         -- L subtree BF:0->+/-1, H:h->h+1, parent BF:-1-> 0

 -- (putZL l e r): Put in L subtree of (Z l e r), BF= 0  (Never requires rebalancing) , (never returns N)
 {-# INLINE putZL #-}
 putZL  E           e r = P (Z    E  e0 E ) e r       -- L subtree        H:0->1, parent BF: 0->+1
 putZL (N ll le lr) e r = let l' = putN ll le lr      -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (P ll le lr) e r = let l' = putP ll le lr      -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (Z ll le lr) e r = let l' = putZ ll le lr      -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          E       -> error "push: Bug1" -- impossible
                          Z _ _ _ -> Z l' e r         -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          _       -> P l' e r         -- L subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->+1

 -- (putZR l e r): Put in R subtree of (Z l e r), BF= 0 (Never requires rebalancing) , (never returns P)
 {-# INLINE putZR #-}
 putZR l e E            = N l e (Z    E  e0 E )       -- R subtree        H:0->1, parent BF: 0->-1
 putZR l e (N rl re rr) = let r' = putN rl re rr      -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (P rl re rr) = let r' = putP rl re rr      -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (Z rl re rr) = let r' = putZ rl re rr      -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          E       -> error "push: Bug2" -- impossible
                          Z _ _ _ -> Z l e r'         -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          _       -> N l e r'         -- R subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->-1

 -- (putPR l e r): Put in R subtree of (P l e r), BF=+1 (Never requires rebalancing) , (never returns N)
 {-# INLINE putPR #-}
 putPR l e  E           = Z l e (Z    E  e0 E )       -- R subtree empty, H:0->1,     parent BF:+1-> 0
 putPR l e (N rl re rr) = let r' = putN rl re rr      -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (P rl re rr) = let r' = putP rl re rr      -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (Z rl re rr) = let r' = putZ rl re rr      -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          E       -> error "push: Bug3" -- impossible
                          Z _ _ _ -> P l e r'         -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                          _       -> Z l e r'         -- R subtree BF:0->+/-1, H:h->h+1, parent BF:+1-> 0

      -------- These 2 cases (NR and PL) may need rebalancing if they go to LEVEL 3 ---------

 -- (putNR l e r): Put in R subtree of (N l e r), BF=-1 , (never returns P)
 {-# INLINE putNR #-}
 putNR _ _ E            = error "push: Bug4"               -- impossible if BF=-1
 putNR l e (N rl re rr) = let r' = putN rl re rr              -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (P rl re rr) = let r' = putP rl re rr              -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (Z rl re rr) = case c re of                        -- determine if RR or RL
                          Lt     -> putNRL l e    rl re  rr   -- RL (never returns P)
                          Eq re' ->    N   l e (Z rl re' rr)  -- new re
                          Gt     -> putNRR l e    rl re  rr   -- RR (never returns P)

 -- (putPL l e r): Put in L subtree of (P l e r), BF=+1 , (never returns N)
 {-# INLINE putPL #-}
 putPL  E           _ _ = error "push: Bug5"               -- impossible if BF=+1
 putPL (N ll le lr) e r = let l' = putN ll le lr              -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (P ll le lr) e r = let l' = putP ll le lr              -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (Z ll le lr) e r = case c le of                        -- determine if LL or LR
                          Lt     -> putPLL  ll le  lr  e r    -- LL (never returns N)
                          Eq le' ->    P (Z ll le' lr) e r    -- new le
                          Gt     -> putPLR  ll le  lr  e r    -- LR (never returns N)

 ----------------------------- LEVEL 3 ---------------------------------
 --                        putNRR, putPLL                             --
 --                        putNRL, putPLR                             --
 -----------------------------------------------------------------------

 -- (putNRR l e rl re rr): Put in RR subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRR #-}
 putNRR l e rl re  E              = Z (Z l e rl) re (Z E e0 E)         -- l and rl must also be E, special CASE RR!!
 putNRR l e rl re (N rrl rre rrr) = let rr' = putN rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (P rrl rre rrr) = let rr' = putP rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (Z rrl rre rrr) = let rr' = putZ rrl rre rrr         -- RR subtree BF= 0, so need to look for changes
                                    in case rr' of
                                    E       -> error "push: Bug6"   -- impossible
                                    Z _ _ _ -> N l e (Z rl re rr')     -- RR subtree BF: 0-> 0, H:h->h, so no change
                                    _       -> Z (Z l e rl) re rr'     -- RR subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE RR !!

 -- (putPLL ll le lr e r): Put in LL subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLL #-}
 putPLL  E le lr e r              = Z (Z E e0 E) le (Z lr e r)         -- r and lr must also be E, special CASE LL!!
 putPLL (N lll lle llr) le lr e r = let ll' = putN lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (P lll lle llr) le lr e r = let ll' = putP lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (Z lll lle llr) le lr e r = let ll' = putZ lll lle llr         -- LL subtree BF= 0, so need to look for changes
                                    in case ll' of
                                    E       -> error "push: Bug7"   -- impossible
                                    Z _ _ _ -> P (Z ll' le lr) e r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                    _       -> Z ll' le (Z lr e r) -- LL subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE LL !!

 -- (putNRL l e rl re rr): Put in RL subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRL #-}
 putNRL l e  E              re rr = Z (Z l e E) e0 (Z E re rr)         -- l and rr must also be E, special CASE LR !!
 putNRL l e (N rll rle rlr) re rr = let rl' = putN rll rle rlr         -- RL subtree BF<>0, H:h->h, so no change
                                    in rl' `seq` N l e (Z rl' re rr)
 putNRL l e (P rll rle rlr) re rr = let rl' = putP rll rle rlr         -- RL subtree BF<>0, H:h->h, so no change
                                    in rl' `seq` N l e (Z rl' re rr)
 putNRL l e (Z rll rle rlr) re rr = let rl' = putZ rll rle rlr         -- RL subtree BF= 0, so need to look for changes
                                    in case rl' of
                                    E                -> error "push: Bug8" -- impossible
                                    Z _    _    _    -> N l e (Z rl' re rr)                -- RL subtree BF: 0-> 0, H:h->h, so no change
                                    N rll' rle' rlr' -> Z (P l e rll') rle' (Z rlr' re rr) -- RL subtree BF: 0->-1, SO.. CASE RL(1) !!
                                    P rll' rle' rlr' -> Z (Z l e rll') rle' (N rlr' re rr) -- RL subtree BF: 0->+1, SO.. CASE RL(2) !!

 -- (putPLR ll le lr e r): Put in LR subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLR #-}
 putPLR ll le  E              e r = Z (Z ll le E) e0 (Z E e r)         -- r and ll must also be E, special CASE LR !!
 putPLR ll le (N lrl lre lrr) e r = let lr' = putN lrl lre lrr         -- LR subtree BF<>0, H:h->h, so no change
                                    in lr' `seq` P (Z ll le lr') e r
 putPLR ll le (P lrl lre lrr) e r = let lr' = putP lrl lre lrr         -- LR subtree BF<>0, H:h->h, so no change
                                    in lr' `seq` P (Z ll le lr') e r
 putPLR ll le (Z lrl lre lrr) e r = let lr' = putZ lrl lre lrr         -- LR subtree BF= 0, so need to look for changes
                                    in case lr' of
                                    E                -> error "push: Bug9" -- impossible
                                    Z _    _    _    -> P (Z ll le lr') e r                -- LR subtree BF: 0-> 0, H:h->h, so no change
                                    N lrl' lre' lrr' -> Z (P ll le lrl') lre' (Z lrr' e r) -- LR subtree BF: 0->-1, SO.. CASE LR(2) !!
                                    P lrl' lre' lrr' -> Z (Z ll le lrl') lre' (N lrr' e r) -- LR subtree BF: 0->+1, SO.. CASE LR(1) !!
-----------------------------------------------------------------------
------------------------- push Ends Here ----------------------------
-----------------------------------------------------------------------

-- | Almost identical to 'push', but this version forces evaluation of the default new element
-- (second argument) if no matching element is found. Note that it does /not/ do this if
-- a matching element is found, because in this case the default new element is discarded
-- anyway. Note also that it does not force evaluation of any replacement value provided by the
-- selector (if it returns Eq). (You have to do that yourself if that\'s what you want.)
--
-- Complexity: O(log n)
push' :: (e -> COrdering e) -> e -> AVL e -> AVL e
push' c e0 = put where
 ----------------------------- LEVEL 0 ---------------------------------
 --                              put                                  --
 -----------------------------------------------------------------------
 put  E        = e0 `seq` Z E e0 E
 put (N l e r) = putN l e  r
 put (Z l e r) = putZ l e  r
 put (P l e r) = putP l e  r

 ----------------------------- LEVEL 1 ---------------------------------
 --                       putN, putZ, putP                            --
 -----------------------------------------------------------------------

 -- Put in (N l e r), BF=-1  , (never returns P)
 putN l e r = case c e of
              Lt    -> putNL l e  r  -- <e, so put in L subtree
              Eq e' -> N     l e' r  -- =e, so update existing
              Gt    -> putNR l e  r  -- >e, so put in R subtree

 -- Put in (Z l e r), BF= 0
 putZ l e r = case c e of
              Lt    -> putZL l e  r  -- <e, so put in L subtree
              Eq e' -> Z     l e' r  -- =e, so update existing
              Gt    -> putZR l e  r  -- >e, so put in R subtree

 -- Put in (P l e r), BF=+1 , (never returns N)
 putP l e r = case c e of
              Lt    -> putPL l e  r  -- <e, so put in L subtree
              Eq e' -> P     l e' r  -- =e, so update existing
              Gt    -> putPR l e  r  -- >e, so put in R subtree

 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNL, putZL, putPL                          --
 --                      putNR, putZR, putPR                          --
 -----------------------------------------------------------------------

 -- (putNL l e r): Put in L subtree of (N l e r), BF=-1 (Never requires rebalancing) , (never returns P)
 {-# INLINE putNL #-}
 putNL  E           e r = e0 `seq` Z (Z E e0 E ) e r  -- L subtree empty, H:0->1, parent BF:-1-> 0
 putNL (N ll le lr) e r = let l' = putN ll le lr      -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (P ll le lr) e r = let l' = putP ll le lr      -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (Z ll le lr) e r = let l' = putZ ll le lr      -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          E       -> error "push': Bug0" -- impossible
                          Z _ _ _ -> N l' e r         -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                          _       -> Z l' e r         -- L subtree BF:0->+/-1, H:h->h+1, parent BF:-1-> 0

 -- (putZL l e r): Put in L subtree of (Z l e r), BF= 0  (Never requires rebalancing) , (never returns N)
 {-# INLINE putZL #-}
 putZL  E           e r = e0 `seq` P (Z E e0 E ) e r  -- L subtree        H:0->1, parent BF: 0->+1
 putZL (N ll le lr) e r = let l' = putN ll le lr      -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (P ll le lr) e r = let l' = putP ll le lr      -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (Z ll le lr) e r = let l' = putZ ll le lr      -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          E       -> error "push': Bug1" -- impossible
                          Z _ _ _ -> Z l' e r         -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          _       -> P l' e r         -- L subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->+1

 -- (putZR l e r): Put in R subtree of (Z l e r), BF= 0 (Never requires rebalancing) , (never returns P)
 {-# INLINE putZR #-}
 putZR l e E            = e0 `seq` N l e (Z E e0 E)   -- R subtree        H:0->1, parent BF: 0->-1
 putZR l e (N rl re rr) = let r' = putN rl re rr      -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (P rl re rr) = let r' = putP rl re rr      -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (Z rl re rr) = let r' = putZ rl re rr      -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          E       -> error "push': Bug2" -- impossible
                          Z _ _ _ -> Z l e r'         -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          _       -> N l e r'         -- R subtree BF: 0->+/-1, H:h->h+1, parent BF: 0->-1

 -- (putPR l e r): Put in R subtree of (P l e r), BF=+1 (Never requires rebalancing) , (never returns N)
 {-# INLINE putPR #-}
 putPR l e  E           = e0 `seq` Z l e (Z E e0 E)   -- R subtree empty, H:0->1,     parent BF:+1-> 0
 putPR l e (N rl re rr) = let r' = putN rl re rr      -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (P rl re rr) = let r' = putP rl re rr      -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (Z rl re rr) = let r' = putZ rl re rr      -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          E       -> error "push': Bug3" -- impossible
                          Z _ _ _ -> P l e r'         -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                          _       -> Z l e r'         -- R subtree BF:0->+/-1, H:h->h+1, parent BF:+1-> 0

      -------- These 2 cases (NR and PL) may need rebalancing if they go to LEVEL 3 ---------

 -- (putNR l e r): Put in R subtree of (N l e r), BF=-1 , (never returns P)
 {-# INLINE putNR #-}
 putNR _ _ E            = error "push': Bug4"              -- impossible if BF=-1
 putNR l e (N rl re rr) = let r' = putN rl re rr              -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (P rl re rr) = let r' = putP rl re rr              -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (Z rl re rr) = case c re of                        -- determine if RR or RL
                          Lt     -> putNRL l e    rl re  rr   -- RL (never returns P)
                          Eq re' ->    N   l e (Z rl re' rr)  -- new re
                          Gt     -> putNRR l e    rl re  rr   -- RR (never returns P)

 -- (putPL l e r): Put in L subtree of (P l e r), BF=+1 , (never returns N)
 {-# INLINE putPL #-}
 putPL  E           _ _ = error "push': Bug5"              -- impossible if BF=+1
 putPL (N ll le lr) e r = let l' = putN ll le lr              -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (P ll le lr) e r = let l' = putP ll le lr              -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (Z ll le lr) e r = case c le of                        -- determine if LL or LR
                          Lt     -> putPLL  ll le  lr  e r    -- LL (never returns N)
                          Eq le' ->    P (Z ll le' lr) e r    -- new le
                          Gt     -> putPLR  ll le  lr  e r    -- LR (never returns N)

 ----------------------------- LEVEL 3 ---------------------------------
 --                        putNRR, putPLL                             --
 --                        putNRL, putPLR                             --
 -----------------------------------------------------------------------

 -- (putNRR l e rl re rr): Put in RR subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRR #-}
 putNRR l e rl re  E              = e0 `seq` Z (Z l e rl) re (Z E e0 E) -- l and rl must also be E, special CASE RR!!
 putNRR l e rl re (N rrl rre rrr) = let rr' = putN rrl rre rrr          -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (P rrl rre rrr) = let rr' = putP rrl rre rrr          -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (Z rrl rre rrr) = let rr' = putZ rrl rre rrr          -- RR subtree BF= 0, so need to look for changes
                                    in case rr' of
                                    E       -> error "push': Bug6"   -- impossible
                                    Z _ _ _ -> N l e (Z rl re rr')      -- RR subtree BF: 0-> 0, H:h->h, so no change
                                    _       -> Z (Z l e rl) re rr'      -- RR subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE RR !!

 -- (putPLL ll le lr e r): Put in LL subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLL #-}
 putPLL  E le lr e r              = e0 `seq` Z (Z E e0 E) le (Z lr e r) -- r and lr must also be E, special CASE LL!!
 putPLL (N lll lle llr) le lr e r = let ll' = putN lll lle llr          -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (P lll lle llr) le lr e r = let ll' = putP lll lle llr          -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (Z lll lle llr) le lr e r = let ll' = putZ lll lle llr          -- LL subtree BF= 0, so need to look for changes
                                    in case ll' of
                                    E       -> error "push': Bug7"   -- impossible
                                    Z _ _ _ -> P (Z ll' le lr) e r      -- LL subtree BF: 0-> 0, H:h->h, so no change
                                    _       -> Z ll' le (Z lr e r)      -- LL subtree BF: 0->+/-1, H:h->h+1, parent BF:-1->-2, CASE LL !!

 -- (putNRL l e rl re rr): Put in RL subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRL #-}
 putNRL l e  E              re rr = e0 `seq` Z (Z l e E) e0 (Z E re rr) -- l and rr must also be E, special CASE LR !!
 putNRL l e (N rll rle rlr) re rr = let rl' = putN rll rle rlr          -- RL subtree BF<>0, H:h->h, so no change
                                    in rl' `seq` N l e (Z rl' re rr)
 putNRL l e (P rll rle rlr) re rr = let rl' = putP rll rle rlr          -- RL subtree BF<>0, H:h->h, so no change
                                    in rl' `seq` N l e (Z rl' re rr)
 putNRL l e (Z rll rle rlr) re rr = let rl' = putZ rll rle rlr          -- RL subtree BF= 0, so need to look for changes
                                    in case rl' of
                                    E                -> error "push': Bug8" -- impossible
                                    Z _    _    _    -> N l e (Z rl' re rr)                -- RL subtree BF: 0-> 0, H:h->h, so no change
                                    N rll' rle' rlr' -> Z (P l e rll') rle' (Z rlr' re rr) -- RL subtree BF: 0->-1, SO.. CASE RL(1) !!
                                    P rll' rle' rlr' -> Z (Z l e rll') rle' (N rlr' re rr) -- RL subtree BF: 0->+1, SO.. CASE RL(2) !!

 -- (putPLR ll le lr e r): Put in LR subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLR #-}
 putPLR ll le  E              e r = e0 `seq` Z (Z ll le E) e0 (Z E e r) -- r and ll must also be E, special CASE LR !!
 putPLR ll le (N lrl lre lrr) e r = let lr' = putN lrl lre lrr          -- LR subtree BF<>0, H:h->h, so no change
                                    in lr' `seq` P (Z ll le lr') e r
 putPLR ll le (P lrl lre lrr) e r = let lr' = putP lrl lre lrr          -- LR subtree BF<>0, H:h->h, so no change
                                    in lr' `seq` P (Z ll le lr') e r
 putPLR ll le (Z lrl lre lrr) e r = let lr' = putZ lrl lre lrr          -- LR subtree BF= 0, so need to look for changes
                                    in case lr' of
                                    E                -> error "push': Bug9" -- impossible
                                    Z _    _    _    -> P (Z ll le lr') e r                -- LR subtree BF: 0-> 0, H:h->h, so no change
                                    N lrl' lre' lrr' -> Z (P ll le lrl') lre' (Z lrr' e r) -- LR subtree BF: 0->-1, SO.. CASE LR(2) !!
                                    P lrl' lre' lrr' -> Z (Z ll le lrl') lre' (N lrr' e r) -- LR subtree BF: 0->+1, SO.. CASE LR(1) !!
-----------------------------------------------------------------------
------------------------- push' Ends Here ----------------------------
-----------------------------------------------------------------------

-- | Similar to 'push', but returns the original tree if the combining comparison returns
-- @('Eq' 'Nothing')@. So this function can be used reduce heap burn rate by avoiding duplication
-- of nodes on the insertion path. But it may also be marginally slower otherwise.
--
-- Note that this function is /non-strict/ in it\'s second argument (the default value which
-- is inserted in the search fails or is discarded if the search succeeds). If you want
-- to force evaluation, but only if it\'s actually incorprated in the tree, then use 'pushMaybe''
--
-- Complexity: O(log n)
pushMaybe :: (e -> COrdering (Maybe e)) -> e -> AVL e -> AVL e
pushMaybe c e t = case openPathWith c t of
                     FullBP  _ Nothing   -> t
                     FullBP  p (Just e') -> writePath  p e' t
                     EmptyBP p           -> insertPath p e  t

-- | Almost identical to 'pushMaybe', but this version forces evaluation of the default new element
-- (second argument) if no matching element is found. Note that it does /not/ do this if
-- a matching element is found, because in this case the default new element is discarded
-- anyway.
--
-- Complexity: O(log n)
pushMaybe' :: (e -> COrdering (Maybe e)) -> e -> AVL e -> AVL e
pushMaybe' c e t = case openPathWith c t of
                      FullBP  _ Nothing   -> t
                      FullBP  p (Just e') -> writePath  p e' t
                      EmptyBP p           -> e `seq` insertPath p e  t

-- | Push a new element in the leftmost position of an AVL tree. No comparison or searching is involved.
--
-- Complexity: O(log n)
pushL :: e -> AVL e -> AVL e
pushL e0 = pushL' where  -- There now follows a cut down version of the more general put.
                         -- Insertion is always on the left subtree.
                         -- Re-Balancing cases RR,RL/LR(1/2) never occur. Only LL!
                         -- There are also more impossible cases (putZL never returns N)
 ----------------------------- LEVEL 0 ---------------------------------
 --                             pushL'                                --
 -----------------------------------------------------------------------
 pushL'  E        = Z E e0 E
 pushL' (N l e r) = putNL l e r
 pushL' (Z l e r) = putZL l e r
 pushL' (P l e r) = putPL l e r

 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNL, putZL, putPL                          --
 -----------------------------------------------------------------------

 -- (putNL l e r): Put in L subtree of (N l e r), BF=-1 (Never requires rebalancing) , (never returns P)
 putNL  E           e r = Z (Z E e0 E) e r            -- L subtree empty, H:0->1, parent BF:-1-> 0
 putNL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:-1->-1
                          in l' `seq` N l' e r
 putNL (Z ll le lr) e r = let l' = putZL ll le lr     -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          Z _ _ _ -> N l' e r         -- L subtree BF:0-> 0, H:h->h  , parent BF:-1->-1
                          P _ _ _ -> Z l' e r         -- L subtree BF:0->+1, H:h->h+1, parent BF:-1-> 0
                          _       -> error "pushL: Bug0" -- impossible

 -- (putZL l e r): Put in L subtree of (Z l e r), BF= 0  (Never requires rebalancing) , (never returns N)
 putZL  E           e r = P (Z E e0 E) e r            -- L subtree        H:0->1, parent BF: 0->+1
 putZL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in l' `seq` Z l' e r
 putZL (Z ll le lr) e r = let l' = putZL ll le lr     -- L subtree BF= 0, so need to look for changes
                          in case l' of
                          Z _ _ _ -> Z l' e r         -- L subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          N _ _ _ -> error "pushL: Bug1" -- impossible
                          _       -> P l' e r         -- L subtree BF: 0->+1, H:h->h+1, parent BF: 0->+1

      -------- This case (PL) may need rebalancing if it goes to LEVEL 3 ---------

 -- (putPL l e r): Put in L subtree of (P l e r), BF=+1 , (never returns N)
 putPL  E           _ _ = error "pushL: Bug2"         -- impossible if BF=+1
 putPL (N ll le lr) e r = let l' = putNL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (P ll le lr) e r = let l' = putPL ll le lr     -- L subtree BF<>0, H:h->h, parent BF:+1->+1
                          in l' `seq` P l' e r
 putPL (Z ll le lr) e r = putPLL ll le lr e r         -- LL (never returns N)

 ----------------------------- LEVEL 3 ---------------------------------
 --                            putPLL                                 --
 -----------------------------------------------------------------------

 -- (putPLL ll le lr e r): Put in LL subtree of (P (Z ll le lr) e r) , (never returns N)
 {-# INLINE putPLL #-}
 putPLL  E le lr e r              = Z (Z E e0 E) le (Z lr e r)          -- r and lr must also be E, special CASE LL!!
 putPLL (N lll lle llr) le lr e r = let ll' = putNL lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (P lll lle llr) le lr e r = let ll' = putPL lll lle llr         -- LL subtree BF<>0, H:h->h, so no change
                                    in ll' `seq` P (Z ll' le lr) e r
 putPLL (Z lll lle llr) le lr e r = let ll' = putZL lll lle llr         -- LL subtree BF= 0, so need to look for changes
                                    in case ll' of
                                    Z _ _ _ -> P (Z ll' le lr) e r -- LL subtree BF: 0-> 0, H:h->h, so no change
                                    N _ _ _ -> error "pushL: Bug3" -- impossible
                                    _       -> Z ll' le (Z lr e r) -- LL subtree BF: 0->+1, H:h->h+1, parent BF:-1->-2, CASE LL !!
-----------------------------------------------------------------------
--------------------------- pushL Ends Here ---------------------------
-----------------------------------------------------------------------


-- | Push a new element in the rightmost position of an AVL tree. No comparison or searching is involved.
--
-- Complexity: O(log n)
pushR :: AVL e -> e -> AVL e
pushR t e0 = pushR' t where  -- There now follows a cut down version of the more general put.
                             -- Insertion is always on the right subtree.
                             -- Re-Balancing cases LL,RL/LR(1/2) never occur. Only RR!
                             -- There are also more impossible cases (putZR never returns P)

 ----------------------------- LEVEL 0 ---------------------------------
 --                             pushR'                                --
 -----------------------------------------------------------------------
 pushR'  E        = Z E e0 E
 pushR' (N l e r) = putNR l e r
 pushR' (Z l e r) = putZR l e r
 pushR' (P l e r) = putPR l e r

 ----------------------------- LEVEL 2 ---------------------------------
 --                      putNR, putZR, putPR                          --
 -----------------------------------------------------------------------

 -- (putZR l e r): Put in R subtree of (Z l e r), BF= 0 (Never requires rebalancing) , (never returns P)
 putZR l e E            = N l e (Z E e0 E)            -- R subtree        H:0->1, parent BF: 0->-1
 putZR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h, parent BF: 0-> 0
                          in r' `seq` Z l e r'
 putZR l e (Z rl re rr) = let r' = putZR rl re rr     -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          Z _ _ _ -> Z l e r'         -- R subtree BF: 0-> 0, H:h->h  , parent BF: 0-> 0
                          N _ _ _ -> N l e r'         -- R subtree BF: 0->-1, H:h->h+1, parent BF: 0->-1
                          _       -> error "pushR: Bug0" -- impossible

 -- (putPR l e r): Put in R subtree of (P l e r), BF=+1 (Never requires rebalancing) , (never returns N)
 putPR l e  E           = Z l e (Z E e0 E)            -- R subtree empty, H:0->1,     parent BF:+1-> 0
 putPR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h,     parent BF:+1->+1
                          in r' `seq` P l e r'
 putPR l e (Z rl re rr) = let r' = putZR rl re rr     -- R subtree BF= 0, so need to look for changes
                          in case r' of
                          Z _ _ _ -> P l e r'         -- R subtree BF:0-> 0, H:h->h  , parent BF:+1->+1
                          N _ _ _ -> Z l e r'         -- R subtree BF:0->-1, H:h->h+1, parent BF:+1-> 0
                          _       -> error "pushR: Bug1" -- impossible

      -------- This case (NR) may need rebalancing if it goes to LEVEL 3 ---------

 -- (putNR l e r): Put in R subtree of (N l e r), BF=-1 , (never returns P)
 putNR _ _ E            = error "pushR: Bug2"         -- impossible if BF=-1
 putNR l e (N rl re rr) = let r' = putNR rl re rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (P rl re rr) = let r' = putPR rl re rr     -- R subtree BF<>0, H:h->h, parent BF:-1->-1
                          in r' `seq` N l e r'
 putNR l e (Z rl re rr) = putNRR l e rl re rr         -- RR (never returns P)

 ----------------------------- LEVEL 3 ---------------------------------
 --                            putNRR                                 --
 -----------------------------------------------------------------------

 -- (putNRR l e rl re rr): Put in RR subtree of (N l e (Z rl re rr)) , (never returns P)
 {-# INLINE putNRR #-}
 putNRR l e rl re  E              = Z (Z l e rl) re (Z E e0 E)          -- l and rl must also be E, special CASE RR!!
 putNRR l e rl re (N rrl rre rrr) = let rr' = putNR rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (P rrl rre rrr) = let rr' = putPR rrl rre rrr         -- RR subtree BF<>0, H:h->h, so no change
                                    in rr' `seq` N l e (Z rl re rr')
 putNRR l e rl re (Z rrl rre rrr) = let rr' = putZR rrl rre rrr         -- RR subtree BF= 0, so need to look for changes
                                    in case rr' of
                                    Z _ _ _ -> N l e (Z rl re rr')      -- RR subtree BF: 0-> 0, H:h->h, so no change
                                    N _ _ _ -> Z (Z l e rl) re rr'      -- RR subtree BF: 0->-1, H:h->h+1, parent BF:-1->-2, CASE RR !!
                                    _       -> error "pushR: Bug3"      -- impossible
-----------------------------------------------------------------------
--------------------------- pushR Ends Here ---------------------------
-----------------------------------------------------------------------


