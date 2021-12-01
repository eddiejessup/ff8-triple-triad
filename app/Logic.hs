{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Logic where

import Data.Generics.Wrapped (_Unwrapped)
import Data.Vector ((!), (//))
import Optics ((!~), (%!~))
import Types

invertDirection :: Direction -> Direction
invertDirection = \case
  North -> South
  South -> North
  East -> West
  West -> East

adjacents :: BoardIx -> [(BoardIx, Direction)]
adjacents ix =
  let raw = case toNice ix of
        (RTop, CLeft) -> [((RTop, CMid), East), ((RMid, CLeft), South)]
        (RTop, CMid) -> [((RTop, CLeft), West), ((RMid, CMid), South), ((RTop, CRight), East)]
        (RTop, CRight) -> [((RTop, CMid), West), ((RMid, CRight), South)]
        (RMid, CLeft) -> [((RTop, CLeft), North), ((RMid, CMid), East), ((RBot, CLeft), South)]
        (RMid, CMid) -> [((RTop, CMid), North), ((RMid, CLeft), West), ((RMid, CRight), East), ((RBot, CMid), South)]
        (RMid, CRight) -> [((RMid, CMid), West), ((RTop, CRight), North), ((RBot, CRight), South)]
        (RBot, CLeft) -> [((RMid, CLeft), North), ((RBot, CMid), East)]
        (RBot, CMid) -> [((RBot, CLeft), West), ((RBot, CRight), East), ((RMid, CMid), North)]
        (RBot, CRight) -> [((RBot, CMid), West), ((RMid, CRight), North)]
   in raw <&> first fromNice

cardPointsInDirection :: Card -> Direction -> Int
cardPointsInDirection Card {n, e, s, w} = \case
  North -> n
  East -> e
  South -> s
  West -> w

-- Low-level
boardSpaceSet :: BoardIx -> BoardCard -> Board -> Board
boardSpaceSet ix bc =
  _Unwrapped %!~ (// [(unBoardIx ix, Just bc)])

boardSpaceFlip :: Board -> BoardIx -> Board
boardSpaceFlip b ix =
  case boardAt b ix of
    Nothing -> error "Cannot flip empty space"
    Just bc ->
      let oc = ownedCard bc
          flippedCard = bc {ownedCard = oc {player = otherPlayer (player oc)}}
       in b & boardSpaceSet ix flippedCard

pluckFromHand :: Hand -> HandIx -> (Card, Hand)
pluckFromHand Hand {unHand = h} HandIx {unHandIx = hIx} =
  case h ! hIx of
    Nothing -> error "Card not present"
    Just c -> (c, Hand {unHand = h // [(hIx, Nothing)]})

-- Constructionally correct API.

addCardToBoard :: Board -> BoardIx -> OwnedCard -> Board
addCardToBoard b ix oc@OwnedCard {card, player} =
  case boardAt b ix of
    Nothing ->
      let adjs = adjacents ix

          ixsToFlip =
            catMaybes $
              adjs <&> \(adjIx, dirToAdj) -> case boardAt b adjIx of
                Nothing -> Nothing
                Just bc ->
                  let OwnedCard {card = adjCard, player = adjPlayer} = ownedCard bc
                   in if
                          | player == adjPlayer -> Nothing
                          | cardPointsInDirection card dirToAdj <= cardPointsInDirection adjCard (invertDirection dirToAdj) -> Nothing
                          | otherwise -> Just adjIx

          boardCard = BoardCard oc player (curTurnIx b)

          boardAfterAdd = boardSpaceSet ix boardCard b

          boardAfterFlippings = foldl' boardSpaceFlip boardAfterAdd ixsToFlip
       in boardAfterFlippings
    Just _ -> error "Board space already inhabited"

playCard :: HandIx -> BoardIx -> Game -> Game
playCard hIx bIx g@Game {board, turn} =
  let (playedCard, pluckedHand) = pluckFromHand (relevantHand g) hIx

      playedBoard = addCardToBoard board bIx (OwnedCard playedCard turn)
   in g
        & relevantHandL !~ pluckedHand
        & #board !~ playedBoard
        & #turn !~ otherPlayer turn
