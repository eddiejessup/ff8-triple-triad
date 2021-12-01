{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Logic where

import Data.Sequence qualified as Seq
import Optics ((!~))
import Types

invertDirection :: Direction -> Direction
invertDirection = \case
  North -> South
  South -> North
  East -> West
  West -> East

adjacents :: BoardIx -> [(BoardIx, Direction)]
adjacents ix =
  let raw = case unBoardIx ix of
        0 -> [(1, East), (3, South)]
        1 -> [(0, West), (4, South), (2, East)]
        2 -> [(1, West), (5, South)]
        3 -> [(0, North), (4, East), (6, South)]
        4 -> [(1, North), (3, West), (5, East), (7, South)]
        5 -> [(4, West), (2, North), (8, South)]
        6 -> [(3, North), (7, East)]
        7 -> [(6, West), (8, East), (4, North)]
        8 -> [(7, West), (5, North)]
        _ -> error "Impossible"
   in raw <&> first BoardIx

cardPointsInDirection :: Card -> Direction -> Int
cardPointsInDirection Card {n, e, s, w} = \case
  North -> n
  East -> e
  South -> s
  West -> w

-- Low-level
boardSpaceSet :: BoardIx -> BoardCard -> Board -> Board
boardSpaceSet ix bc (Board b) =
  Board $ Seq.update (unBoardIx ix) (Just bc) b

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
  case h `Seq.index` hIx of
    Nothing -> error "Card not present"
    Just c -> (c, Hand $ Seq.update hIx Nothing h)

-- Constructionally correct API.

addCardToBoard :: Board -> BoardIx -> OwnedCard -> Board
addCardToBoard b ix oc@OwnedCard {card, player} =
  case boardAt b ix of
    Nothing ->
      let adjs = adjacents ix

          ixsToFlip =
            catMaybes $
              adjs <&> \(adjIx, dirToAdj) -> do
                bc <- boardAt b adjIx
                let OwnedCard {card = adjCard, player = adjPlayer} = ownedCard bc
                guard (player /= adjPlayer)
                guard (cardPointsInDirection card dirToAdj > cardPointsInDirection adjCard (invertDirection dirToAdj))
                pure adjIx

          boardCard = BoardCard oc player (nrCardsOnBoard b)

          boardAfterAdd = boardSpaceSet ix boardCard b

          boardAfterFlippings = foldl' boardSpaceFlip boardAfterAdd ixsToFlip
       in boardAfterFlippings
    Just _ -> error "Board space already inhabited"

playCard :: HandIx -> BoardIx -> Game -> Game
playCard hIx bIx g@Game {board, turn} =
  let (playedCard, pluckedHand) = pluckFromHand (relevantHand g) hIx

      playedBoard = addCardToBoard board bIx (OwnedCard playedCard turn)
   in g
        & relevantHandSetter !~ pluckedHand
        & #board !~ playedBoard
        & #turn !~ otherPlayer turn
