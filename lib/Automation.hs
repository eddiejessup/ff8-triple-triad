module Automation where

import Data.Foldable (Foldable (minimum), maximum, maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Tree (Tree)
import Data.Tree qualified as T
import Logic
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck qualified as QC
import Types
import Control.Monad (foldM)

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
gameValue = gameValueAlt

gameValueSimple :: SearchParams -> Player -> Game -> Int
gameValueSimple SearchParams {maxDepth} evalPlayer = go 0
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

negInfGameVal :: Int
negInfGameVal = -1000

posInfGameVal :: Int
posInfGameVal = 1000

type ValFloor = Int

minOverTheirMoves :: ValFloor -> (Game -> Int) -> NonEmpty Game -> Maybe Int
minOverTheirMoves valFloor gameToVal = foldM f posInfGameVal
  where
    f :: Int -> Game -> Maybe Int
    f curMinVal ng =
      let ngVal = gameToVal ng
      in
        if ngVal < valFloor
          then Nothing
          else Just (min curMinVal ngVal)

maxOverOurMoves :: (ValFloor -> Game -> Maybe Int) -> NonEmpty Game -> Int
maxOverOurMoves gameToVal = foldl' f negInfGameVal
  where
    f :: Int -> Game -> Int
    f curMaxVal ng =
      case gameToVal curMaxVal ng of
        Nothing -> curMaxVal
        Just valAboveFloor -> max curMaxVal valAboveFloor


gameValueAlt :: SearchParams -> Player -> Game -> Int
gameValueAlt SearchParams {maxDepth} evalPlayer g0 =
  case go negInfGameVal 0 g0 of
    Nothing -> error "nooo"
    Just v -> v
  where
    go :: ValFloor -> Int -> Game -> Maybe Int
    go valFloor curDepth g =
      case nonEmpty (possibleNextGames g) of
        Just nextGames
          | curDepth < maxDepth ->
            if evalPlayer == turn g
              then
                let nGameToVal nngValFloor = go nngValFloor (curDepth + 1)
                in Just $ maxOverOurMoves nGameToVal nextGames
              else
                let
                  nGameToVal nng = case go negInfGameVal (curDepth + 1) nng of
                      Nothing -> error "impossible?"
                      Just nngVal -> nngVal
                in minOverTheirMoves valFloor nGameToVal nextGames
        _ ->
          Just $ score evalPlayer g

minOverTheirMoves2 :: ValCeil -> (Game -> Int) -> NonEmpty Game -> Maybe Int
minOverTheirMoves2 initValCeil gameToVal games = foldM f posInfGameVal
  where
    f :: Int -> Game -> Maybe Int
    f curMinVal ng =
      let ngVal = gameToVal ng
      in
        if ngVal < valFloor
          then Nothing
          else Just (min curMinVal ngVal)

maxOverOurMoves2 :: ValFloor -> (ValFloor -> Game -> Maybe Int) -> NonEmpty Game -> Int
maxOverOurMoves2 initValFloor gameToVal games = snd $ foldl' f (initValFloor, negInfGameVal) games
  where
    f :: (ValFloor, Int) -> Game -> (ValFloor, Int)
    f (curValFloor, curMaxVal) ng =
      case gameToVal curValFloor ng of
        Nothing -> (curValFloor, curMaxVal)
        Just valAboveFloor ->
          let newVal = max curMaxVal valAboveFloor
          in (max curValFloor newVal,  newVal)

gameValueAlt2 :: SearchParams -> Player -> Game -> Int
gameValueAlt2 SearchParams {maxDepth} evalPlayer g0 =
  case go negInfGameVal 0 g0 of
    Nothing -> error "nooo"
    Just v -> v
  where
    go :: ValFloor -> Int -> Int -> Game -> Maybe Int
    go valFloor valCeil curDepth g =
      case nonEmpty (possibleNextGames g) of
        Just nextGames
          | curDepth < maxDepth ->
            if evalPlayer == turn g
              then
                let nGameToVal subValFloor = go subValFloor valCeil (curDepth + 1)
                in Just $ maxOverOurMoves2 valFloor nGameToVal nextGames
              else
                let nGameToVal subValCeil = go valFloor subValCeil (curDepth + 1)
                in minOverTheirMoves2 valCeil nGameToVal nextGames
        _ ->
          Just $ score evalPlayer g

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
