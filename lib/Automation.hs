module Automation where

import Data.Foldable (Foldable (minimum), maximum, maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Tree (Tree)
import Data.Tree qualified as T
import Logic
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck qualified as QC
import Types

newtype SearchParams = SearchParams {maxDepth :: Int}
  deriving (Show, Eq, Generic)

instance Arbitrary SearchParams where
  arbitrary = do
    maxDepth <- QC.chooseInt (1, 3)
    pure SearchParams {maxDepth}

possibleNextGames :: Game -> [Game]
possibleNextGames g = do
  nextPlay <- possibleNextPlays g
  pure $ playCard nextPlay g

bestNextGames :: SearchParams -> Game -> Maybe (Int, NonEmpty (Play, Game))
bestNextGames params g0 = do
  nextPlays <- nonEmpty (possibleNextPlays g0)
  let nextGames = nextPlays <&> \pl -> playCard pl g0
      nextValues = nextGames <&> gameValue params (turn g0)
      bestValue = maximum nextValues
      nexts = NE.zip (NE.zip nextPlays nextGames) nextValues
  bests <- nonEmpty $ NE.filter (\((_, _), v) -> v == bestValue) nexts
  pure (bestValue, bests <&> fst)

aBestNextGame :: SearchParams -> Game -> Maybe (Int, (Play, Game))
aBestNextGame params g0 = do
  (bestScore, bests) <- bestNextGames params g0
  pure (bestScore, last bests)

aBestNextGameFast :: SearchParams -> Game -> Maybe Game
aBestNextGameFast params g0 = do
  nextPlays <- nonEmpty $ possibleNextPlays g0
  let nextGames = nextPlays <&> \pl -> playCard pl g0
      nextValues = nextGames <&> \g -> (g, gameValue params (turn g0) g)
      best = fst $ maximumBy (\(_, a) (_, b) -> compare a b) nextValues
   in Just best

optimalGame :: SearchParams -> Game -> NonEmpty Game
optimalGame params = NE.unfoldr go
  where
    go g = (g, aBestNextGameFast params g)

gameValue :: SearchParams -> Player -> Game -> Int
gameValue SearchParams {maxDepth} evalPlayer = go 0
  where
    go curDepth g =
      case nonEmpty (possibleNextGames g) of
        Just nextGames
          | curDepth < maxDepth ->
            let agger =
                  if evalPlayer == turn g
                    then maximum
                    else minimum
             in agger (go (curDepth + 1) <$> nextGames)
        _ ->
          score evalPlayer g

gameTree :: Int -> Game -> Tree Game
gameTree maxDepth g0 = T.unfoldTree go (g0, 0)
  where
    go :: (Game, Int) -> (Game, [(Game, Int)])
    go (g, depth) =
      (g, if depth == maxDepth then [] else possibleNextGames g <&> (,depth + 1))

countGames :: (Int, Int) -> Bool -> Int
countGames (p1Cards, p2Cards) isP1 =
  let nrCardsPlayed = (5 - p1Cards) + (5 - p2Cards)

      spacesLeft = 9 - nrCardsPlayed

      nrCardOptionsNow = if isP1 then p1Cards else p2Cards

      nrPlaysNow = spacesLeft * nrCardOptionsNow

      newHands =
        if isP1
          then (p1Cards - 1, p2Cards)
          else (p1Cards, p2Cards - 1)
   in if nrPlaysNow == 0
        then 1
        else nrPlaysNow * countGames newHands (not isP1)
