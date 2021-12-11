module Main where

import Automation
import Data.List.NonEmpty qualified as NE
import Data.Yaml qualified as Ya
import Parse ()
import Play (PlayConfig (game))
import Types
import Prelude hiding (head)

main :: IO ()
main = do
  playConfig <- Ya.decodeFileThrow @_ @PlayConfig "config/bench.yml"
  let optGame = optimalGame (SearchParams {maxDepth = 5}) (game playConfig)
  print $ score P1 (NE.last optGame)
