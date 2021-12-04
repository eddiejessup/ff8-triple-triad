module Main where

import Automation
import Data
import Logic
import Nice
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

main :: IO ()
main = do
  let optGame = optimalGame game
  let Just lastOptGame = viaNonEmpty last optGame
  print $ score P1 lastOptGame
