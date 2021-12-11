module Types where

import Data.Aeson qualified as Ae
import Data.Sequence qualified as Seq
import Optics (Setter', sets)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))

newtype Points = Points {unPoints :: Int}
  deriving (Show, Eq, Ord, Generic)

-- instance Enum Points where
--   toEnum p
--     | p < 0 || p > 10 = error "Points out of range"
--     | otherwise = Points p

--   fromEnum = unPoints

--   succ (Points p)
--     | p >= 10 = error "Points out of range"
--     | otherwise = Points (succ p)

--   pred (Points p)
--     | p <= 0 = error "Points out of range"
--     | otherwise = Points (pred p)

-- instance Bounded Points where
--   minBound = Points 1
--   maxBound = Points 10

instance Arbitrary Points where
  arbitrary = Points <$> QC.elements [1 .. 10]

data Card = Card {n, e, s, w :: Points}
  deriving (Show, Eq, Generic)

icard :: Int -> Int -> Int -> Int -> Card
icard n e s w = Card (Points n) (Points e) (Points s) (Points w)

instance Arbitrary Card where
  arbitrary =
    Card
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

data Player = P1 | P2
  deriving (Eq, Show, Generic)

instance Arbitrary Player where
  arbitrary = QC.elements [P1, P2]

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

nrCardsOnBoard :: Board -> Int
nrCardsOnBoard = countWhere isJust . unBoard

availableBoardIxs :: Board -> [BoardIx]
availableBoardIxs b = BoardIx <$> Seq.findIndicesL isNothing (unBoard b)

newtype BoardIx = BoardIx {unBoardIx :: Int}
  deriving (Show, Generic)

boardAt :: Board -> BoardIx -> Maybe BoardCard
boardAt b ix =
  unBoard b `Seq.index` unBoardIx ix

newtype Hand = Hand {unHand :: Seq (Maybe Card)}
  deriving (Show, Generic)

handAt :: Hand -> HandIx -> Maybe Card
handAt h ix = unHand h `Seq.index` unHandIx ix

arbitraryInitialHand :: QC.Gen Hand
arbitraryInitialHand = do
  cardList <- QC.vectorOf 5 (arbitrary @Card)
  pure $ Hand $ Seq.fromList (Just <$> cardList)

newHand :: Seq Card -> Hand
newHand = Hand . fmap Just

newtype HandIx = HandIx {unHandIx :: Int}
  deriving (Show, Generic)
  deriving newtype (Ae.FromJSON)

availableHandIxs :: Hand -> [HandIx]
availableHandIxs h = HandIx <$> Seq.findIndicesL isJust (unHand h)

data Move = Move {moveHandIx :: HandIx, moveBoardIx :: BoardIx}

data Game = Game {p1Hand, p2Hand :: Hand, board :: Board, turn :: Player}
  deriving (Show, Generic)

initialGame :: Hand -> Hand -> Player -> Game
initialGame p1Hand p2Hand turn = Game {p1Hand, p2Hand, turn, board = emptyBoard}

relevantHand :: Game -> Hand
relevantHand Game {turn, p1Hand, p2Hand} = case turn of
  P1 -> p1Hand
  P2 -> p2Hand

{-# INLINE relevantHandSetter #-}
relevantHandSetter :: Setter' Game Hand
relevantHandSetter = sets setRelevantHand
  where
    setRelevantHand :: (Hand -> Hand) -> Game -> Game
    setRelevantHand f g@Game {p1Hand, p2Hand, turn} = case turn of
      P1 -> g {p1Hand = f p1Hand}
      P2 -> g {p2Hand = f p2Hand}

possibleMoves :: Game -> [Move]
possibleMoves g = do
  moveHandIx <- availableHandIxs (relevantHand g)
  moveBoardIx <- availableBoardIxs $ board g
  pure $ Move {moveHandIx, moveBoardIx}

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

{-# INLINE countWhere #-}
countWhere :: (a -> Bool) -> Seq a -> Int
countWhere f = foldl' (\n v -> if f v then n + 1 else n) 0
