module Main where

import Automation
import Data
import Logic
import Nice
import Types
import Prelude hiding (head)
import qualified Data.List.NonEmpty as NE

hand1 :: Hand
hand1 = newHandFromNames ["Geezard", "Geezard", "Bite Bug", "Grat", "Fasticholon-F"]

hand2 :: Hand
hand2 = newHandFromNames ["Blobra", "Caterchapillar", "Cockatrice", "Funguar", "Red Bat"]

game0 :: Player -> Game
game0 = Game hand1 hand2 emptyBoard

game :: Game
game =
  game0 P1
    & playCard (HandIx 0) (fromNice (RTop, CRight))
    & playCard (HandIx 0) (fromNice (RMid, CRight))

main :: IO ()
main = do
  let optGame = optimalGame 100 game
  let lastOptGame = NE.last optGame
  print $ score P1 lastOptGame
