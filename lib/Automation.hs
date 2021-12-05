module Automation where

import Data.Foldable (maximum, maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Tree (Tree)
import Data.Tree qualified as T
import Logic
import Types

possibleHandIxs :: Hand -> [HandIx]
possibleHandIxs h = HandIx <$> Seq.findIndicesL isJust (unHand h)

possibleRelevantHandIxs :: Game -> [HandIx]
possibleRelevantHandIxs g = possibleHandIxs (relevantHand g)

possibleBoardIxs :: Board -> [BoardIx]
possibleBoardIxs b = BoardIx <$> Seq.findIndicesL isNothing (unBoard b)

possibleNextPlays :: Game -> [Play]
possibleNextPlays g = do
  nextHix <- possibleRelevantHandIxs g
  nextBix <- possibleBoardIxs $ board g
  pure (nextHix, nextBix)

possibleNextGames :: Game -> [Game]
possibleNextGames g = do
  (nextHix, nextBix) <- possibleNextPlays g
  pure $ playCard nextHix nextBix g

bestNextGames :: Int -> Game -> Maybe (Int, NonEmpty (Play, Game))
bestNextGames maxDepth g0 = do
  nextPlays <- nonEmpty (possibleNextPlays g0)
  let nextGames = nextPlays <&> \(hIx, bIx) -> playCard hIx bIx g0
      nextValues = nextGames <&> gameValue maxDepth (turn g0)
      bestValue = maximum nextValues
      nexts = NE.zip (NE.zip nextPlays nextGames) nextValues
  bests <- nonEmpty $ NE.filter (\((_, _), v) -> v == bestValue) nexts
  pure (bestValue, bests <&> fst)

aBestNextGame :: Int -> Game -> Maybe (Int, (Play, Game))
aBestNextGame maxDepth g0 = do
  (bestScore, bests) <- bestNextGames maxDepth g0
  pure (bestScore, last bests)

aBestNextGameFast :: Int -> Game -> Maybe Game
aBestNextGameFast maxDepth g0 = do
  nextPlays <- nonEmpty $ possibleNextPlays g0
  let nextGames = nextPlays <&> \(hIx, bIx) -> playCard hIx bIx g0
      nextValues = nextGames <&> \g -> (g, gameValue maxDepth (turn g0) g)
      best = fst $ maximumBy (\(_, a) (_, b) -> compare a b) nextValues
   in Just best

optimalGame :: Int -> Game -> NonEmpty Game
optimalGame maxDepth = NE.unfoldr go
  where
    go g = (g, aBestNextGameFast maxDepth g)

gameValue :: Int -> Player -> Game -> Int
gameValue maxDepth = go 0
  where
    go curDepth evalPlayer g =
      case nonEmpty (possibleNextGames g) of
        Just nextGames | curDepth < maxDepth ->
          let turnPlayer = turn g
              valueToTurnPlayer = maximum (go (curDepth + 1) turnPlayer <$> nextGames)
          in if evalPlayer == turnPlayer then valueToTurnPlayer else 10 - valueToTurnPlayer
        _ ->
          score evalPlayer g

gameTree :: Int -> Game -> Tree Game
gameTree maxDepth g0 = T.unfoldTree go (g0, 0)
  where
    go :: (Game, Int) -> (Game, [(Game, Int)])
    go (g, depth) =
      (g, if depth == maxDepth then [] else possibleNextGames g <&> (, depth + 1))

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
