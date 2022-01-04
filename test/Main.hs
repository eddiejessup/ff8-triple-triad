module Main where

import Test.QuickCheck qualified as QC
import Automation
import Types

prop_gameValueFns :: SearchParams -> Player -> Game -> QC.Property
prop_gameValueFns a b c = gameValueSimple a b c QC.=== gameValueAlt a b c

main :: IO ()
main = do
  QC.quickCheck prop_gameValueFns
