-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Putzplan (Group, GroupInit(..), Putzplan, Time,
                 Weight, GroupInitMap,
                 putzplan, printPutzplan, printPutzplan',
                 PutzplanState, initPutzplanState,
                 GroupMap, initGroupMap, groupMapDump,
                 putzplanStateDump) where

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

All groups g start with some score s_g. To compute the next group
which has to clean, the algorithm adds the weight w_g to each group's
score s_g. All groups g whose scores s_g have crossed the threshold
("critical groups") have their crossed_g timestamp set to the current
time. It then picks one of the critical groups taking into the
consideration the groups scores & timestamps. To implement this, there
is an ordering defined on the groups which does the work for us:
Groups which have crossed the threshold earlier are 'more critical'
and so are groups which had been picked longer ago. The picked group
will have its last_g timestamp set to the current time and its score
s_g decreased by one.

Time is then incremented and the next group can be computed. -MS

-}

import qualified Data.Map as M
import           Data.Maybe

-- | A Group is a String.
type Group = String

-- | A Weight is a rational number. Actually only rational numbers in
-- the closed unit interval are allowed.
type Weight = Rational

data GroupInit = GroupInit { groupWeight :: Weight
                           , groupScore :: Score
                           } deriving (Show)

type GroupInitMap = M.Map Group GroupInit

-- | A WeightedGroup is a Group together with a Weight.
data WeightedGroup = WeightedGroup Group Weight deriving (Show)

-- | A Score is a rational number.
type Score = Rational

-- | Time is an Integer for this program.
type Time = Integer

-- | This is the Putzplan type.
type Putzplan = [(Group, PutzplanState)]

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

data PutzplanState = PutzplanState { putzplanTime   :: Time
                                   , putzplanGroups :: GroupMap
                                   } deriving (Show)

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
initGroupMap :: GroupInitMap -> GroupMap
initGroupMap = M.map initGroup
  where initGroup :: GroupInit -> GroupMeta
        initGroup g = GroupMeta { groupMetaWeight  = groupWeight g
                                , groupMetaCrossed = Nothing
                                , groupMetaLast    = Nothing
                                , groupMetaScore   = groupScore g }

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
          in gMeta { groupMetaCrossed = Nothing
                   , groupMetaLast    = Just time
                   , groupMetaScore   = score - 1
                   }

-- | Return those Groups which have crossed the threshold (one).
thresholdGroups :: GroupMap -> GroupMap
thresholdGroups = M.filter thresholdReached 
  where thresholdReached gMeta = groupMetaScore gMeta >= 1

-- | Returns Just the GroupMap containing only the most critical
-- group. Returns Nothing if the provided GroupMap is empty.
mostCritical :: GroupMap -> Maybe (Group, GroupMeta)
mostCritical gMap = M.foldlWithKey findMostCritical Nothing gMap
  where findMostCritical a' k b =
          case a' of
            Nothing -> Just (k, b)
            Just (_, a) -> if a > b then a' else Just (k, b)

initPutzplanState :: Time -> GroupInitMap -> PutzplanState
initPutzplanState time groupInitMap =
  PutzplanState { putzplanTime   = time
                , putzplanGroups = initGroupMap groupInitMap }

-- | Generates the Putzplan.
putzplan :: PutzplanState -> Putzplan
putzplan pps =
  let time       = putzplanTime pps
      gMap       = putzplanGroups pps
      gMap'      = increaseScores time gMap -- Increase all Scores by
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
  in case critGroup of
       Nothing -> putzplan $ pps { putzplanGroups = gMap' }
       Just (g, gMeta) -> let critGroupMap = M.fromList [(g, gMeta)]
                              gMap'' = M.union (resetScores time critGroupMap) gMap'
                              pps' = pps { putzplanTime = time', putzplanGroups = gMap'' }
                          in (g,  pps') : (putzplan pps')

-- | Pretty printer for the Putzplan.
printPutzplan :: Time -> Putzplan -> String
printPutzplan _ [] = ""
printPutzplan t ((g, _) : xs) =
  show t ++ ": " ++ g ++ "\n" ++ printPutzplan (succ t) xs

-- | Pretty printer for the Putzplan. More verbose debug version.
printPutzplan' :: Time -> Putzplan -> String
printPutzplan' _ [] = ""
printPutzplan' t ((group, pps) : xs) =
  "[" ++ show t ++ "]\n" ++ putzplanStateDump pps ++ "\n"
    ++ "  => " ++ group ++ "\n\n" ++ printPutzplan' (succ t) xs

putzplanStateDump :: PutzplanState -> String
putzplanStateDump pps =
  "PutzplanState {\n"
  ++ "  putzplanTime = " ++ show (putzplanTime pps) ++ ",\n"
  ++ "  putzplanGroups = \n"
  ++ indentLines "    " (groupMapDump (putzplanGroups pps))
  ++ "}\n"

indentLines :: String -> String -> String
indentLines indent = unlines . (map (indent ++)) . lines

groupMapDump :: GroupMap -> String
groupMapDump gMap =
  "Data.Map.fromList [\n"
  ++ concatMap (\ gM -> "  " ++ (dumpGroupMeta gM) ++ "\n") (M.toList gMap)
  ++ "]\n"
  where dumpGroupMeta (g, gMeta) =
          "(" ++ show g ++ ", " ++ show gMeta ++ ")"
