module Automation where

import Data.Foldable (Foldable (minimum), maximum, maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Traversable (for)
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

bestNextGames :: SearchParams -> Game -> Maybe (Int, NonEmpty (Move, Game))
bestNextGames params g0 = do
  nextPlays <- nonEmpty (possibleMoves g0)
  let nextGames = nextPlays <&> makeMove g0
      nextValues = nextGames <&> gameValue params (turn g0)
      bestValue = maximum nextValues
      nextPGs = NE.zip (NE.zip nextPlays nextGames) nextValues
  bestPGS <- nonEmpty $ NE.filter (\((_, _), v) -> v == bestValue) nextPGs
  let bestPlays = bestPGS <&> fst
  pure (bestValue, bestPlays)

aBestNextGame :: SearchParams -> Game -> Maybe (Int, (Move, Game))
aBestNextGame params g0 = do
  (bestScore, bests) <- bestNextGames params g0
  pure (bestScore, last bests)

aBestNextGameFast :: SearchParams -> Game -> Maybe Game
aBestNextGameFast params g0 = do
  nextPlays <- nonEmpty $ possibleMoves g0
  let nextGames = nextPlays <&> makeMove g0
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

gameValueAlt :: SearchParams -> Player -> Game -> Int
gameValueAlt SearchParams {maxDepth} evalPlayer = go 0
  where
    go curDepth g =
      case nonEmpty (possibleNextGames g) of
        Just nextGames
          | curDepth < maxDepth ->
            let nextGameValues = runIdentity $
                  for nextGames $ \nextGame -> do
                    pure $ go (curDepth + 1) nextGame

                agger =
                  if evalPlayer == turn g
                    then maximum
                    else minimum
             in agger nextGameValues
        _ ->
          score evalPlayer g

prop_gameValueFns :: SearchParams -> Player -> Game -> QC.Property
prop_gameValueFns a b c = gameValue a b c QC.=== gameValueAlt a b c

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
