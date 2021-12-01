{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Automation where

import Data.Generics.Wrapped (_Unwrapped)
import Data.Tree (Tree)
import Data.Tree qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Logic
import Optics (to, (%), (^.))
import Types

possibleHandIxs :: Hand -> Vector HandIx
possibleHandIxs h = HandIx <$> V.findIndices isJust (h ^. _Unwrapped)

possibleRelevantHandIxs :: Game -> Vector HandIx
-- possibleRelevantHandIxs g = g ^. relevantHandL % to possibleHandIxs
possibleRelevantHandIxs g = possibleHandIxs (relevantHand g)

possibleBoardIxs :: Board -> Vector BoardIx
possibleBoardIxs b = BoardIx <$> V.findIndices isNothing (b ^. _Unwrapped)

possibleNextPlays :: Game -> Vector (HandIx, BoardIx)
possibleNextPlays g = do
  nextHix <- possibleRelevantHandIxs g
  nextBix <- possibleBoardIxs $ board g
  pure (nextHix, nextBix)

possibleNextGames :: Game -> Vector Game
possibleNextGames g = do
  (nextHix, nextBix) <- possibleNextPlays g
  pure $ playCard nextHix nextBix g

possibleNextGames' :: Vector Game -> Vector Game
possibleNextGames' = (>>= possibleNextGames)

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

bestNextGame :: Game -> Maybe (Int, Vector ((HandIx, BoardIx), Game))
bestNextGame g0 =
  case possibleNextPlays g0 of
    [] -> Nothing
    nextPlays ->
      let nextGames = nextPlays <&> \(hIx, bIx) -> playCard hIx bIx g0
          nextValues = gameValue (g0 ^. #turn) <$> nextGames
          bestValue = V.maximum nextValues

          nexts = V.zip3 nextPlays nextGames nextValues

          bests = V.filter (\(_, _, v) -> v == bestValue) nexts
       in Just (bestValue, bests <&> \(p, g, _v) -> (p, g))

anyBestNextGame :: Game -> Maybe Game
anyBestNextGame g0 =
  case possibleNextPlays g0 of
    [] -> Nothing
    nextPlays ->
      let nextGames = nextPlays <&> \(hIx, bIx) -> playCard hIx bIx g0
          nextValues = gameValue (g0 ^. #turn) <$> nextGames
       in Just (nextGames V.! V.maxIndex nextValues)

compareGameValues :: Player -> Game -> Game -> Ordering
compareGameValues p a b = compare (gameValue p a) (gameValue p b)

gameValue :: Player -> Game -> Int
gameValue evalPlayer g =
  case possibleNextGames g of
    [] ->
      scoreAlt evalPlayer g
    nextGames ->
      let turnPlayer = (g ^. #turn)

          valueToTurnPlayer = V.maximum (gameValue turnPlayer <$> nextGames)

          valueToEvalPlayer = if evalPlayer == turnPlayer then valueToTurnPlayer else 10 - valueToTurnPlayer
       in valueToEvalPlayer

gameTree :: Int -> Game -> Tree Game
gameTree maxDepth game0 = T.unfoldTree go (game0, 0)
  where
    go :: (Game, Int) -> (Game, [(Game, Int)])
    go (g, depth) =
      (g, if depth == maxDepth then [] else toList (possibleNextGames g) <&> (,depth + 1))
