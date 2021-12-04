module Types where

import Data.Generics.Wrapped (_Unwrapped)
import Data.Maybe.Optics (_Just)
import Data.Sequence qualified as Seq
import Optics (Fold, Setter', folded, sets, to, (%))
import Optics qualified as O

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

boardAt :: Board -> BoardIx -> Maybe BoardCard
boardAt b ix =
  unBoard b `Seq.index` unBoardIx ix

data RowPos = RTop | RMid | RBot
  deriving (Show, Generic)

data ColPos = CLeft | CMid | CRight
  deriving (Show, Generic)

data Direction = North | East | South | West
  deriving (Show, Generic)

newtype Hand = Hand {unHand :: Seq (Maybe Card)}
  deriving (Show, Generic)

type Play = (HandIx, BoardIx)

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

relevantHandSetter :: Setter' Game Hand
relevantHandSetter = sets setRelevantHand
  where
    setRelevantHand :: (Hand -> Hand) -> Game -> Game
    setRelevantHand f g@Game {p1Hand, p2Hand, turn} = case turn of
      P1 -> g {p1Hand = f p1Hand}
      P2 -> g {p2Hand = f p2Hand}

gameCardsFold :: Fold Game OwnedCard
gameCardsFold =
  let p1HandCards :: Fold Game OwnedCard
      p1HandCards = #p1Hand % handCardsFold % to (`OwnedCard` P1)

      p2HandCards :: Fold Game OwnedCard
      p2HandCards = #p2Hand % handCardsFold % to (`OwnedCard` P2)

      boardCards :: Fold Game OwnedCard
      boardCards = #board % boardCardsFold % #ownedCard
   in p1HandCards `O.summing` p2HandCards `O.summing` boardCards

nrCardsOnBoard :: Board -> Int
nrCardsOnBoard = countWhere isJust . unBoard

{-# INLINE countWhere #-}
countWhere :: (a -> Bool) -> Seq a -> Int
countWhere f = foldl' (\n v -> if f v then n + 1 else n) 0

score :: Player -> Game -> Int
score p g =
  let netChangeToP2 =
        foldl' (\n v -> n + changeToP2 v) 0 (unBoard (board g))
   in case p of
        P1 -> 5 - netChangeToP2
        P2 -> 5 + netChangeToP2
  where
    {-# INLINE changeToP2 #-}
    changeToP2 = \case
      Just (BoardCard (OwnedCard _ P2) P1 _) -> 1
      Just (BoardCard (OwnedCard _ P1) P2 _) -> -1
      _ -> 0
