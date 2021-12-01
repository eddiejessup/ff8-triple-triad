{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Automation where

import Data.Foldable (maximum, maximumBy)
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

possibleNextPlays :: Game -> [(HandIx, BoardIx)]
possibleNextPlays g = do
  nextHix <- possibleRelevantHandIxs g
  nextBix <- possibleBoardIxs $ board g
  pure (nextHix, nextBix)

possibleNextGames :: Game -> [Game]
possibleNextGames g = do
  (nextHix, nextBix) <- possibleNextPlays g
  pure $ playCard nextHix nextBix g

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

bestNextGame :: Game -> Maybe (Int, [((HandIx, BoardIx), Game)])
bestNextGame g0 =
  case possibleNextPlays g0 of
    [] -> Nothing
    nextPlays ->
      let nextGames = nextPlays <&> \(hIx, bIx) -> playCard hIx bIx g0
          nextValues = gameValue (turn g0) <$> nextGames
          bestValue = maximum nextValues

          nexts = zip3 nextPlays nextGames nextValues

          bests = filter (\(_, _, v) -> v == bestValue) nexts
       in Just (bestValue, bests <&> \(p, g, _v) -> (p, g))

anyBestNextGame :: Game -> Maybe Game
anyBestNextGame g0 =
  case possibleNextPlays g0 of
    [] -> Nothing
    nextPlays ->
      let nextGames = nextPlays <&> \(hIx, bIx) -> playCard hIx bIx g0
          nextValues = nextGames <&> \g -> (g, gameValue (turn g0) g)
          best = fst $ maximumBy (\(_, a) (_, b) -> compare a b) nextValues
       in Just best

compareGameValues :: Player -> Game -> Game -> Ordering
compareGameValues p a b = compare (gameValue p a) (gameValue p b)

gameValue :: Player -> Game -> Int
gameValue evalPlayer g =
  case possibleNextGames g of
    [] ->
      scoreAlt evalPlayer g
    nextGames ->
      let turnPlayer = turn g

          valueToTurnPlayer = maximum (gameValue turnPlayer <$> nextGames)

          valueToEvalPlayer = if evalPlayer == turnPlayer then valueToTurnPlayer else 10 - valueToTurnPlayer
       in valueToEvalPlayer

gameTree :: Int -> Game -> Tree Game
gameTree maxDepth game0 = T.unfoldTree go (game0, 0)
  where
    go :: (Game, Int) -> (Game, [(Game, Int)])
    go (g, depth) =
      (g, if depth == maxDepth then [] else toList (possibleNextGames g) <&> (,depth + 1))
