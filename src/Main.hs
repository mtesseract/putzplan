-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Main where

import Putzplan

wGroups :: [WeightedGroup]
wGroups = [WeightedGroup "Group 1" 0.3,
           WeightedGroup "Group 2" 0.4,
           WeightedGroup "Group 3" 0.1,
           WeightedGroup "Group 4" 0.2]

groupMap :: GroupMap
groupMap = initGroupMap wGroups

main :: IO ()
main = do
  let plan = putzplan 0 groupMap
  (putStr . printPutzplan . (take 110)) plan
