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

flippedBoardIxs :: Board -> OwnedCard -> BoardIx -> [BoardIx]
flippedBoardIxs b OwnedCard {card = srcCard, player = srcP} ix =
  catMaybes $ case unBoardIx ix of
    0 -> [boardAtUnsafe 1 e w, boardAtUnsafe 3 s n]
    1 -> [boardAtUnsafe 0 w e, boardAtUnsafe 4 s n, boardAtUnsafe 2 e w]
    2 -> [boardAtUnsafe 1 w e, boardAtUnsafe 5 s n]
    3 -> [boardAtUnsafe 0 n s, boardAtUnsafe 4 e w, boardAtUnsafe 6 s n]
    4 -> [boardAtUnsafe 1 n s, boardAtUnsafe 3 w e, boardAtUnsafe 5 e w, boardAtUnsafe 7 s n]
    5 -> [boardAtUnsafe 4 w e, boardAtUnsafe 2 n s, boardAtUnsafe 8 s n]
    6 -> [boardAtUnsafe 3 n s, boardAtUnsafe 7 e w]
    7 -> [boardAtUnsafe 6 w e, boardAtUnsafe 8 e w, boardAtUnsafe 4 n s]
    8 -> [boardAtUnsafe 7 w e, boardAtUnsafe 5 n s]
    _ -> error "Impossible"
  where
    boardAtUnsafe :: Int -> (Card -> Int) -> (Card -> Int) -> Maybe BoardIx
    boardAtUnsafe i srcGetter tgtGetter = do
      BoardCard (OwnedCard {card = tgtCard, player = tgtP}) _ _ <- unBoard b `Seq.index` i
      guard (srcP /= tgtP && srcGetter srcCard > tgtGetter tgtCard)
      pure $ BoardIx i

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
addCardToBoard b ix oc@OwnedCard {player} =
  case boardAt b ix of
    Nothing ->
      let ixsToFlip = flippedBoardIxs b oc ix
          boardAfterAdd = boardSpaceSet ix (BoardCard oc player (nrCardsOnBoard b)) b
       in foldl' boardSpaceFlip boardAfterAdd ixsToFlip
    Just _ -> error "Board space already inhabited"

playCard :: HandIx -> BoardIx -> Game -> Game
playCard hIx bIx g@Game {board, turn} =
  let (playedCard, pluckedHand) = pluckFromHand (relevantHand g) hIx

      playedBoard = addCardToBoard board bIx (OwnedCard playedCard turn)
   in g
        & relevantHandSetter !~ pluckedHand
        & #board !~ playedBoard
        & #turn !~ otherPlayer turn
