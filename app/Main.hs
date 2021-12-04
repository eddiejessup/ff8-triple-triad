module Main where

import Automation
import Data
import Diagrams.Backend.SVG.CmdLine qualified as D.Cmd
import Draw
import Logic
import Nice
import Play
import Types
import Prelude hiding (head)

hand1 :: Hand
hand1 = newHandFromNames ["Geezard", "Geezard", "Bite Bug", "Best", "Fasticholon-F"]

hand2 :: Hand
hand2 = newHandFromNames ["Best", "Caterchapillar", "Cockatrice", "Funguar", "Red Bat"]

game0 :: Player -> Game
game0 = Game hand1 hand2 emptyBoard

game :: Game
game =
  game0 P1
    & playCard (HandIx 0) (fromNice (RTop, CRight))
    & playCard (HandIx 0) (fromNice (RMid, CRight))

-- & playCard (HandIx 1) (fromNice (RBot, CRight))

-- & playCard (HandIx 1) (fromNice (RTop, CMid))
-- & playCard (HandIx 2) (fromNice (RMid, CMid))
-- & playCard (HandIx 2) (fromNice (RBot, CMid))

-- & playCard (HandIx 3) (fromNice (RTop, CLeft))
-- & playCard (HandIx 3) (fromNice (RMid, CLeft))

main :: IO ()
main = do
  let optGame = optimalGame game
  D.Cmd.mainWith (gamesDiagram optGame)

  hSetBuffering stdin NoBuffering
  playGame P2 game

-- let Just lastOptGame = viaNonEmpty last optGame
-- print $ score P1 lastOptGame
