module Main where

import Data.Yaml qualified as Ya
import Parse ()
import Play

-- mainOptPlay :: IO ()
-- mainOptPlay = do
--   let optGame = optimalGame (SearchParams {maxDepth = 4}) game
--   D.Cmd.mainWith (gameDiagram $ last optGame)

main :: IO ()
main = do
  playConfig <- Ya.decodeFileThrow @_ @PlayConfig "config/game.yml"
  playGame "play.svg" playConfig
  pure ()
