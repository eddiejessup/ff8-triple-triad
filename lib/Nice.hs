module Nice where

import Data qualified
import Data.Generics.Wrapped (_Unwrapped)
import Optics ((^.))
import Types

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

boardRow :: Board -> RowPos -> [Maybe BoardCard]
boardRow b r = [CLeft, CMid, CRight] <&> \c -> boardAt b (fromNice (r, c))

boardCol :: Board -> ColPos -> [Maybe BoardCard]
boardCol b c = [RTop, RMid, RBot] <&> \r -> boardAt b (fromNice (r, c))

playedCardName :: Game -> HandIx -> Text
playedCardName g hix = case handAt (relevantHand g) hix of
  Nothing -> error "No card found in hand!"
  Just c -> fromMaybe "[Unknown]" (Data.cardName c)

showBoardIxNice :: BoardIx -> Text
showBoardIxNice bix =
  let (bRow, bCol) = toNice bix

      rowS = case bRow of
        RTop -> "top"
        RMid -> "middle"
        RBot -> "bottom"

      colS = case bCol of
        CLeft -> "left"
        CMid -> "center"
        CRight -> "right"
   in rowS <> " " <> colS

showMove :: Game -> Move -> Text
showMove g (Move hix bix) =
  let hixS = show $ hix ^. _Unwrapped
      nameS = playedCardName g hix
      bixS = showBoardIxNice bix
   in hixS <> " " <> bixS <> " (" <> nameS <> ")"
