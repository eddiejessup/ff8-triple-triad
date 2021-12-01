{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}

module Types where

import Data.Generics.Wrapped (_Unwrapped)
import Data.Maybe.Optics (_Just)
import Data.Sequence qualified as Seq
import Data.Vector (Vector)
import Optics (Fold, folded, to, (%), (^.))
import Optics qualified as O
import Optics.Lens (Lens', lens)

data Card = Card {n, e, s, w :: Int}
  deriving (Show, Eq, Generic)

data Player = P1 | P2
  deriving (Eq, Show, Generic)

otherPlayer :: Player -> Player
otherPlayer = \case
  P1 -> P2
  P2 -> P1

data OwnedCard = OwnedCard {card :: Card, player :: Player}
  deriving (Show, Generic)

data BoardCard = BoardCard {ownedCard :: OwnedCard, turnPlayer :: Player, turnIx :: Int}
  deriving (Show, Generic)

-- Representation: [first row left-to-right, second row left-to-right, third row left-to-right]
-- 0 1 2
-- 3 4 5
-- 6 7 8
newtype Board = Board {unBoard :: Seq (Maybe BoardCard)}
  deriving (Show, Generic)

emptyBoard :: Board
emptyBoard = Board $ Seq.replicate 9 Nothing

boardCardsFold :: Fold Board BoardCard
boardCardsFold = _Unwrapped % folded % _Just

newtype BoardIx = BoardIx {unBoardIx :: Int}
  deriving (Show, Generic)

data RowPos = RTop | RMid | RBot
  deriving (Show, Generic)

data ColPos = CLeft | CMid | CRight
  deriving (Show, Generic)

type NiceBoardIx = (RowPos, ColPos)

fromNice :: NiceBoardIx -> BoardIx
fromNice (r, c) = BoardIx $ 3 * rowToInt r + colToInt c
  where
    rowToInt = \case
      RTop -> 0
      RMid -> 1
      RBot -> 2

    colToInt = \case
      CLeft -> 0
      CMid -> 1
      CRight -> 2

toNice :: BoardIx -> NiceBoardIx
toNice ix = case ix ^. _Unwrapped of
  0 -> (RTop, CLeft)
  1 -> (RTop, CMid)
  2 -> (RTop, CRight)
  3 -> (RMid, CLeft)
  4 -> (RMid, CMid)
  5 -> (RMid, CRight)
  6 -> (RBot, CLeft)
  7 -> (RBot, CMid)
  8 -> (RBot, CRight)
  _ -> error "Bad index"

boardAt :: Board -> BoardIx -> Maybe BoardCard
boardAt b ix =
  unBoard b `Seq.index` unBoardIx ix

boardRow :: Board -> RowPos -> Vector (Maybe BoardCard)
boardRow b r = [CLeft, CMid, CRight] <&> \c -> boardAt b (fromNice (r, c))

boardCol :: Board -> ColPos -> Vector (Maybe BoardCard)
boardCol b c = [RTop, RMid, RBot] <&> \r -> boardAt b (fromNice (r, c))

data Direction = North | East | South | West
  deriving (Show, Generic)

curTurnIx :: Board -> Int
curTurnIx = nrCardsOnBoard

-- maybe 0 succ (O.maximumOf (boardCardsFold % #turnIx) b)

newtype Hand = Hand {unHand :: Seq (Maybe Card)}
  deriving (Show, Generic)

newHand :: Seq Card -> Hand
newHand = Hand . fmap Just

handCardsFold :: Fold Hand Card
handCardsFold = _Unwrapped % folded % _Just

newtype HandIx = HandIx {unHandIx :: Int}
  deriving (Show, Generic)

data Game = Game {p1Hand, p2Hand :: Hand, board :: Board, turn :: Player}
  deriving (Show, Generic)

relevantHand :: Game -> Hand
relevantHand Game {turn, p1Hand, p2Hand} = case turn of
  P1 -> p1Hand
  P2 -> p2Hand

setRelevantHand :: Game -> Hand -> Game
setRelevantHand g@Game {turn} h = case turn of
  P1 -> g {p1Hand = h}
  P2 -> g {p2Hand = h}

relevantHandL :: Lens' Game Hand
relevantHandL = lens relevantHand setRelevantHand

gameCardsFold :: Fold Game OwnedCard
gameCardsFold =
  let p1HandCards :: Fold Game OwnedCard
      p1HandCards = #p1Hand % handCardsFold % to (`OwnedCard` P1)

      p2HandCards :: Fold Game OwnedCard
      p2HandCards = #p2Hand % handCardsFold % to (`OwnedCard` P2)

      boardCards :: Fold Game OwnedCard
      boardCards = #board % boardCardsFold % #ownedCard
   in p1HandCards `O.summing` p2HandCards `O.summing` boardCards

score :: Player -> Game -> Int
score p =
  O.lengthOf (gameCardsFold % #player % O.filtered (p ==))

nrCardsOnBoard :: Board -> Int
nrCardsOnBoard b =
  sum $
    unBoard b <&> \case
      Nothing -> 0
      Just _ -> 1

scoreAlt :: Player -> Game -> Int
scoreAlt p g =
  let netFlipToP1 =
        sum $
          unBoard (board g) <&> \case
            Nothing -> 0
            Just bc -> flipToP1 bc
   in case p of
        P1 -> 5 + netFlipToP1
        P2 -> 5 - netFlipToP1
  where
    flipToP1 :: BoardCard -> Int
    flipToP1 bc =
      let origOwner = turnPlayer bc
       in if origOwner /= player (ownedCard bc) then (if origOwner == P2 then 1 else -1) else 0
