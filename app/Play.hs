module Play where

import Automation
import Data.Generics.Wrapped (_Unwrapped)
import Data.List ((!!))
import Data.Text qualified as Tx
import Data.Text.Read qualified as Tx.R
import Diagrams qualified as D
import Diagrams.Backend.SVG qualified as D.SVG
import Draw
import Logic
import Nice
import Optics ((^.))
import Types
import Prelude hiding (head)

parseHandIx :: Text -> Maybe HandIx
parseHandIx s = case Tx.R.decimal s of
  Right (v, "") -> Just $ HandIx v
  _ -> Nothing

parseBoardIxRow :: Text -> Maybe RowPos
parseBoardIxRow = \case
  "t" -> Just RTop
  "m" -> Just RMid
  "b" -> Just RBot
  _ -> Nothing

parseBoardIxCol :: Text -> Maybe ColPos
parseBoardIxCol = \case
  "l" -> Just CLeft
  "m" -> Just CMid
  "r" -> Just CRight
  _ -> Nothing

parsePlay :: Text -> Maybe (HandIx, BoardIx)
parsePlay t = do
  let parts = Tx.splitOn " " t
  guard (length parts == 3)
  playHandIx <- parseHandIx (parts !! 0)
  playBoardRow <- parseBoardIxRow (parts !! 1)
  playBoardCol <- parseBoardIxCol (parts !! 2)
  pure (playHandIx, fromNice (playBoardRow, playBoardCol))

playGame :: Player -> Game -> IO ()
playGame player = go
  where
    go g = do
      putTextLn $ "Score: P1=" <> show (score P1 g) <> ", P2=" <> show (score P2 g)

      if null (possibleNextGames g)
        then do
          putTextLn "It's over!"
          writeGame g
        else do
          newGame <-
            if g ^. #turn == player
              then do
                writeGame g
                putTextLn $ "Make your play, " <> show player <> "([0-4] [tmb] [lmr]):"

                getLine >>= \case
                  "hint" -> do
                    (bestValue, bestPlays) <- case bestNextGames g of
                      Nothing ->
                        error "end"
                      Just (bestValue, bests) -> do
                        pure (bestValue, bests <&> fst)
                    putTextLn $ "Best plays, with value=" <> show bestValue <> ": "
                    putText $ unlines $ toList $ bestPlays <&> showPlay
                    pure g
                  playRaw | Just (hix, bix) <- parsePlay playRaw -> do
                    let newGame = playCard hix bix g
                    writeGame newGame
                    putTextLn "Play made. Press enter to end turn"
                    _ <- getLine
                    pure newGame
                  _ -> do
                    putTextLn "Invalid input, go again..."
                    pure g
              else do
                putTextLn "Playing P2..."
                case aBestNextGame g of
                  Nothing ->
                    error "end"
                  Just (_bestValue, ((hix, bix), _bestGame)) -> do
                    putTextLn $ "P2 plays: " <> showPlay (hix, bix)
                    pure $ playCard hix bix g
          go newGame
    writeGame g =
      D.SVG.renderSVG "foob.svg" (D.mkSizeSpec2D (Just 400) (Just 400)) (gameDiagram g)

    showPlay (hix, bix) =
      let hixS = show $ hix ^. _Unwrapped

          (bRow, bCol) = toNice bix

          rowS = case bRow of
            RTop -> "t"
            RMid -> "m"
            RBot -> "b"

          colS = case bCol of
            CLeft -> "l"
            CMid -> "m"
            CRight -> "r"
       in hixS <> " " <> rowS <> " " <> colS
