{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Automation
import Data
import Data.Generics.Wrapped (_Unwrapped)
import Data.List (head, (!!))
import Data.Text qualified as Tx
import Data.Text.Read qualified as Tx.R
import Data.Vector qualified as V
import Diagrams qualified as D
import Diagrams.Backend.SVG qualified as D.SVG
import Diagrams.Backend.SVG.CmdLine qualified as D.Cmd
import Draw
import Logic
import Optics ((^.))
import Types
import Prelude hiding (head)

hand1 :: Hand
hand1 = newHandFromNames ["Geezard", "Geezard", "Bite Bug", "Funguar", "Fasticholon-F"]

hand2 :: Hand
hand2 = newHandFromNames ["Blood Soul", "Caterchapillar", "Cockatrice", "Funguar", "Red Bat"]

-- board1 :: Board
-- board1 = Board
--   [
--       EmptySpace,
--       FilledSpace (OwnedCard card1 P1),
--       EmptySpace,
--       FilledSpace (OwnedCard card1 P1),
--       EmptySpace,
--       FilledSpace (OwnedCard card1 P1),
--       EmptySpace,
--       FilledSpace (OwnedCard card1 P2),
--       EmptySpace
--   ]

game0 :: Player -> Game
game0 = Game hand1 hand2 emptyBoard

game :: Game
game =
  game0 P1
    & playCard (HandIx 0) (fromNice (RTop, CRight))
    & playCard (HandIx 0) (fromNice (RMid, CRight))
    & playCard (HandIx 1) (fromNice (RBot, CRight))

-- & playCard (HandIx 1) (fromNice (RTop, CMid))
-- & playCard (HandIx 2) (fromNice (RMid, CMid))
-- & playCard (HandIx 2) (fromNice (RBot, CMid))

-- & playCard (HandIx 3) (fromNice (RTop, CLeft))
-- & playCard (HandIx 3) (fromNice (RMid, CLeft))

optimalGame :: Game -> V.Vector Game
optimalGame = V.unfoldr go
  where
    go g =
      case anyBestNextGame g of
        Nothing -> Nothing
        Just best ->
          Just (best, best)

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

playGame :: Game -> IO ()
playGame g0 = do
  putTextLn $ "Score: P1=" <> show (score P1 g0) <> ", P2=" <> show (score P2 g0)

  if null (possibleNextGames g0)
    then do
      putTextLn "It's over!"
      writeGame g0
    else do
      case g0 ^. #turn of
        P1 -> do
          writeGame g0
          putTextLn "Make your play, P1 (0-4) (tmb) (lmr) > "

          newGame <-
            getLine >>= \case
              "hint" -> do
                (bestValue, bestPlays) <- case bestNextGame g0 of
                  Nothing ->
                    error "end"
                  Just (bestValue, bests) -> do
                    pure (bestValue, bests <&> fst)
                putTextLn $ "Best plays, with value=" <> show bestValue <> ": "
                putText $ unlines $ toList $ bestPlays <&> showPlay
                pure g0
              playRaw | Just (hix, bix) <- parsePlay playRaw -> do
                let newGame = playCard hix bix g0
                writeGame newGame
                putTextLn "Play made. Press enter to end turn"
                _ <- getLine
                pure newGame
              _ -> do
                putTextLn "Invalid input, go again..."
                pure g0
          playGame newGame
        P2 -> do
          putTextLn "Playing P2..."
          case bestNextGame g0 of
            Nothing ->
              error "end"
            Just (_bestValue, bests) -> do
              let ((hix, bix), _bestGame) = head bests
              putTextLn $ "P2 plays: " <> showPlay (hix, bix)
              let newGame = playCard hix bix g0
              playGame newGame
  where
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

main :: IO ()
main = do
  -- hSetBuffering stdin NoBuffering
  -- playGame game

  -- let Just bestFirstMove = anyBestNextGame game
  -- print $ score P1 bestFirstMove

  -- D.Cmd.mainWith (gameDiagram bestFirstMove)

  D.Cmd.mainWith (gamesDiagram (optimalGame game))

-- let
--   fromGame0s = possibleNextGames game0
--   fromGame1s = possibleNextGames' fromGame0s
--   fromGame2s = possibleNextGames' fromGame1s
--   fromGame3s = possibleNextGames' fromGame2s
--   fromGame4s = possibleNextGames' fromGame3s

-- print $ length fromGame0s
-- print $ length fromGame1s
-- print $ length fromGame2s
-- print $ length fromGame4s

-- let diag = D.hsep 0.3 $ D.center . D.vsep 0.2 . toList . fmap (gameDiagram) <$> [[game0], V.take 15 fromGame0s, V.take 15 fromGame1s, V.take 15 fromGame2s, V.take 15 fromGame3s]
-- D.Cmd.mainWith (gameDiagram game)
-- D.Cmd.mainWith (gamesDiagram (p1DominatesSequence game))
