{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Draw where

import Data.List ((!!))
import Diagrams hiding (turn)
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Prelude qualified as D
import Types

playerColor :: Maybe Player -> D.Colour Double
playerColor = \case
  Just P1 -> D.green
  Just P2 -> D.blue
  Nothing -> D.yellow

altPlayerColor :: Maybe Player -> D.Colour Double
altPlayerColor = \case
  Just P1 -> D.white
  Just P2 -> D.orange
  Nothing -> D.purple

cardDiagram :: Maybe Player -> Card -> D.Diagram B
cardDiagram mayP Card {..} =
  mkText w (-1, 0)
    <> mkText e (1, 0)
    <> mkText s (0, -1)
    <> mkText n (0, 1)
    <> (D.square 1 # D.fc (playerColor mayP))
  where
    mkText val (x, y) = D.text (show val) # D.scale textScale # D.translate (D.r2 (x * (textPos / 2), y * (textPos / 2))) # D.fc (altPlayerColor mayP)

    textScale = 0.15

    textPos = 0.7

noCardDiagram :: Maybe Player -> D.Diagram B
noCardDiagram mayP =
  D.square 1 # D.fc (playerColor mayP) # D.opacity 0.2

ownedCardDiagram :: OwnedCard -> Diagram B
ownedCardDiagram (OwnedCard c p) = cardDiagram (Just p) c

boardCardDiagram :: BoardCard -> Diagram B
boardCardDiagram (BoardCard oc origOwner turnIx) =
  (playDiag <> (D.rect 0.4 0.4 # D.lw 0 # D.fc D.gray))
    <> (ownedCardDiagram oc # D.centerXY)
  where
    playDiag =
      D.text (show turnIx) # D.fc (playerColor (Just origOwner)) # D.scale 0.25

boardSpaceDiagram :: Maybe BoardCard -> Diagram B
boardSpaceDiagram = \case
  Nothing -> noCardDiagram Nothing
  Just bc -> boardCardDiagram bc

boardDiagram :: Board -> D.Diagram B
boardDiagram b =
  ( rowDiag (boardRow b RTop)
      === rowDiag (boardRow b RMid)
      === rowDiag (boardRow b RBot)
  )
    # centerXY
  where
    bd x = (boardSpaceDiagram x # D.scale 0.9) <> (D.square 1 # D.opacity 0)

    rowDiag row = bd (row !! 0) ||| bd (row !! 1) ||| bd (row !! 2)

handDiagram :: Player -> Hand -> Diagram B
handDiagram p Hand {unHand} =
  let d =
        vcat $
          toList $
            unHand <&> \case
              Nothing -> noCardDiagram (Just p)
              Just c -> ownedCardDiagram (OwnedCard c p)
   in d # centerXY

gameDiagram :: Game -> Diagram B
gameDiagram g@Game {p1Hand, p2Hand, board, turn} =
  (hsep 0.25 [handD P1 p1Hand, boardDiagram board, handD P2 p2Hand] # centerXY)
    === statusD
  where
    statusD = hsep 0.67 [scoreD P1, circle 0.2 # fc (playerColor (Just turn)), scoreD P2] # padY 2 # centerXY

    handD p h = bgFrame 0.1 D.gray (handDiagram p h) # scale 0.6

    scoreD p =
      D.text (show (score p g)) # D.fc (playerColor (Just p)) # D.scale 0.7

gamesDiagram :: (Functor f, Foldable f) => f Game -> Diagram B
gamesDiagram games =
  D.centerXY $ D.vsep 0.2 $ toList $ gameDiagram <$> games
