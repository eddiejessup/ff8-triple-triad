module Main where

import Automation
import Data
import Diagrams.Backend.SVG.CmdLine qualified as D.Cmd
import Draw
import Play
import Types
import Prelude hiding (head)

hand1 :: Hand
hand1 = newHandFromNames ["Geezard", "Geezard", "Bite Bug", "Belhelmel", "Fastitocalon"]

hand2 :: Hand
hand2 = newHandFromNames ["Mesmerize", "Caterchipillar", "Cockatrice", "Funguar", "Red Bat"]

game0 :: Player -> Game
game0 = Game hand1 hand2 emptyBoard

game :: Game
game =
  game0 P1

mainPlay :: IO ()
mainPlay = do
  hSetBuffering stdin NoBuffering
  playGame 1 P2 game

mainOptPlay :: IO ()
mainOptPlay = do
  let optGame = optimalGame 4 game
  D.Cmd.mainWith (gameDiagram $ last optGame)

main :: IO ()
main = mainOptPlay
