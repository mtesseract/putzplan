-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Main where

import Data.Ratio
import Putzplan

wGroups :: [WeightedGroup]
wGroups = [WeightedGroup "foo" (51 % 100),
           WeightedGroup "bar" (49 % 100)]

groupMap :: GroupMap
groupMap = initGroupMap wGroups

main :: IO ()
main = do
  let plan = putzplan 0 groupMap
  (putStr . printPutzplan . (take 110)) plan
