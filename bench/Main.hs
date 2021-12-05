module Main where

import Automation
import Data
import Data.List.NonEmpty qualified as NE
import Logic
import Nice
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
    & playCard (Play (HandIx 0) (fromNice (RTop, CRight)))
    & playCard (Play (HandIx 0) (fromNice (RMid, CRight)))

main :: IO ()
main = do
  let optGame = optimalGame (SearchParams {maxDepth = 100}) game
  let lastOptGame = NE.last optGame
  print $ score P1 lastOptGame
