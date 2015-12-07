-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Putzplan (Group, WeightedGroup(..), Putzplan,
                 putzplan, printPutzplan, printPutzplan',
                 GroupMap, initGroupMap) where

{-|
Module      : Putzplan
Description : Module implementing the algorithm for a shared, weighted cleaning schedule.
Copyright   : (C) 2015 Moritz Schulte
License     : BSD3
Maintainer  : Moritz Schulte <mtesseract@silverratio.net>
Stability   : experimental
Portability : POSIX
-}

{-|

A sketch of the algorithm:

In the context of this algorithm, a "time" is an integer, as we are
not interested in continuous phaenomena. Think e.g. of the time being
a "week of the calendar".

Each group g as associated to it a weight w_g, a score s_g and two
timestamps crossed_g and last_g. crossed_g denotes the time at which
the group g has crossed the threshold value one. last_g holds the most
recent timestamp at which the group g had to clean.

The groups start with some score s_g -- it doesn't really matter what
that is. I set it one.

To compute the next group which has to clean, the algorithm adds the
weight w_g to each group's score s_g. All groups g whose scores s_g
have crossed the threshold ("critical groups") have their crossed_g
timestamp set to the current time. It then picks one of the critical
groups taking into the consideration the groups scores &
timestamps. To implement this, there is an ordering defined on the
groups which does the work for us: Groups which have crossed the
threshold earlier are 'more critical' and so are groups which had been
picked longer ago. The picked group will have its last_g timestamp set
to the current time.

Time is then incremented and the next group can be computed. -MS

-}

import qualified Data.Map as M
import           Data.Maybe
import           Data.Ratio

-- | A Group is a String.
type Group = String

-- | A Weight is a rational number. Actually only rational numbers in
-- the closed unit interval are allowed.
type Weight = Rational

-- | A WeightedGroup is a Group together with a Weight.
data WeightedGroup = WeightedGroup Group Weight deriving (Show)

-- | A Score is a rational number.
type Score = Rational

-- | Time is an Integer for this program.
type Time = Integer

-- | This is the Putzplan type.
type Putzplan = [(Time, Group, GroupMap)]

-- | Associated to Groups are a (constant) Weight and a (varying)
-- Score.
data GroupMeta =
  GroupMeta { groupMetaWeight  :: Weight     -- ^ Weight of this Group
            , groupMetaCrossed :: Maybe Time -- ^ Time at which the threshold has been crossed
            , groupMetaLast    :: Maybe Time -- ^ Time at which it was this groups turn
            , groupMetaScore   :: Score      -- ^ Varying Score of this Group
            } deriving (Show, Eq)

-- | A GroupMap is a mapping from Group values to GroupMeta values.
type GroupMap = M.Map Group GroupMeta

-- | We make GroupMeta values comparable in terms of their Score and
-- their Timestamps. Bigger means 'more critical'.
instance Ord GroupMeta where
  gm1 `compare` gm2
    -- s2 has crossed the threshold, s1 has not, thus s1 < s2 ,
    -- i.e. s2 is more critical:
    | s1 <  1 && s2 >= 1 = LT
    -- s1 has crossed the threshold, s2 has not, thus s1 > s2 ,
    -- i.e. s1 is more critical:
    | s2 <  1 && s1 >= 1 = GT
    -- Neither has crossed the threshold: If their scores coincide,
    -- then the one whose last turn is longer ago, is more
    -- critical. Otherwise, the one with the higher score is more
    -- critical.
    | s1 <  1 && s2 <  1 = if s1 == s2
                              then l2 `compare` l1
                              else s1 `compare` s2
    -- Both have crossed the threshold: The one who crossed the
    -- threshold earlier is more critical. If the crossing times
    -- coincide, then the one whose turn is longer ago is considered
    -- to be more critical.
    | s1 >= 1 && s2 >= 1 = if t1 == t2
                              then l2 `compare` l1
                              else t2 `compare` t1
    | otherwise = error "compare"
    where (GroupMeta { groupMetaWeight    = _
                     , groupMetaCrossed   = t1
                     , groupMetaLast      = l1
                     , groupMetaScore     = s1 }) = gm1
          (GroupMeta { groupMetaWeight    = _
                     , groupMetaCrossed   = t2
                     , groupMetaLast      = l2
                     , groupMetaScore     = s2 }) = gm2

-- | This function initializes a GroupMap given a list of
-- WeightedGroup values.
initGroupMap :: [WeightedGroup] -> GroupMap
initGroupMap = foldl addWGroup M.empty
  where addWGroup :: GroupMap -> WeightedGroup -> GroupMap
        addWGroup gMap (WeightedGroup group weight) =
          let groupMeta = GroupMeta { groupMetaWeight  = weight
                                    , groupMetaCrossed = Nothing
                                    , groupMetaLast    = Nothing
                                    , groupMetaScore   = 1
                                    }
          in M.insert group groupMeta gMap

-- | Adds Weight to Score inside a GroupMeta value. If the Score is
-- smaller than one before the addition, increment the time.
scoreIncrease :: Time -> GroupMeta -> GroupMeta
scoreIncrease time gMeta =
  let (GroupMeta { groupMetaWeight    = w,
                   groupMetaCrossed   = t,
                   groupMetaScore     = s }) = gMeta
      s' = s + w
      t' = if isNothing t &&  s' >= 1 then Just time else t
  in gMeta { groupMetaCrossed = t', groupMetaScore = s' }

-- | Increases all Scores by their associated Weight.
increaseScores :: Time -> GroupMap -> GroupMap
increaseScores time = M.map (scoreIncrease time)

-- | Removes the Integer part of all Scores and reset the Time to
-- zero.
resetScores :: Time -> GroupMap -> GroupMap
resetScores time = M.map reset
  where reset :: GroupMeta -> GroupMeta
        reset gMeta =
          let score = groupMetaScore gMeta
              (_, frac) = properFraction score :: (Integer, Rational)
          in gMeta { groupMetaCrossed = Nothing
                   , groupMetaLast    = Just time
                   , groupMetaScore   = frac
                   }

-- | Return those Groups which have crossed the threshold (one).
thresholdGroups :: GroupMap -> GroupMap
thresholdGroups = M.filter thresholdReached 
  where thresholdReached gMeta = groupMetaScore gMeta >= 1

-- | Returns the GroupMap containing only the most critical
-- group. Returns the empty map if the provided GroupMap is empty.
mostCritical :: GroupMap -> GroupMap
mostCritical gMap =
  let maybeMap = M.foldlWithKey findMostCritical Nothing gMap
  in case maybeMap of
       Nothing -> M.empty
       Just g -> M.fromList [g]
  where findMostCritical a' k b =
          case a' of
            Nothing -> Just (k, b)
            Just (_, a) -> if a > b then a' else Just (k, b)

-- | Generates the Putzplan.
putzplan :: Time -- ^ Current Time
         -> GroupMap -> Putzplan
putzplan time gMap =
  let gMap'      = increaseScores time gMap -- Increase all Scores by
                                            -- the appropriate
                                            -- weights.
      critGroups = thresholdGroups gMap'    -- Extract the groups that
                                            -- have crossed the
                                            -- threshold and...
      critGroup  = mostCritical critGroups  -- ... try to extract the
                                            -- single, most critical
                                            -- group.
      time'      = succ time                -- This will be the new
                                            -- time.
  in case M.toList critGroup of
       []       -> (time, "", gMap') : putzplan time' gMap'
       (g, _):_ -> (time, g,  gMap') : putzplan time' gMap''
                   -- Reset Score for critGroup and update
                   -- groupMetaLast & groupMetaCrossed.
                   where gMap'' = M.union (resetScores time critGroup) gMap'

-- | Pretty printer for the Putzplan.
printPutzplan :: Putzplan -> String
printPutzplan [] = ""
printPutzplan ((t, g, _) : xs) =
  show t ++ ": " ++ g ++ "\n" ++ printPutzplan xs

-- | Pretty printer for the Putzplan. More verbose debug version.
printPutzplan' :: Putzplan -> String
printPutzplan' [] = ""
printPutzplan' ((t, group, gMap) : xs) =
  "[" ++ show t ++ "]\n" ++ printGroupMap gMap ++ "\n"
    ++ "  => " ++ group ++ "\n\n" ++ printPutzplan' xs
  where printGroupMap = M.foldlWithKey (\ a k b -> a ++ printGroupMap' k b ++ "\n") ""
        printGroupMap' g gMeta =
          "  " ++ g ++ ": Score = " ++ printRational (groupMetaScore gMeta)
            ++ "; Crossed = " ++ show (groupMetaCrossed gMeta)
            ++ "; Last = " ++ show (groupMetaLast gMeta)

-- | Pretty print a rational number.
printRational :: Rational -> String
printRational r = show (numerator r) ++ "/" ++ show (denominator r)
