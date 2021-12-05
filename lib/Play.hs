module Play where

import Automation
import Control.Monad.Except qualified as Ex
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

parsePlay :: Text -> Maybe Play
parsePlay t = do
  let parts = Tx.splitOn " " t
  guard (length parts == 3)
  playHandIx <- parseHandIx (parts !! 0)
  playBoardRow <- parseBoardIxRow (parts !! 1)
  playBoardCol <- parseBoardIxCol (parts !! 2)
  pure $ Play playHandIx (fromNice (playBoardRow, playBoardCol))

playGame :: SearchParams -> Player -> Game -> IO ()
playGame aiParams player = go
  where
    go g = do
      putTextLn $ "Score: P1=" <> show (score P1 g) <> ", P2=" <> show (score P2 g)

      if null (possibleNextGames g)
        then do
          putTextLn "It's over!"
          writeGame g
        else do
          let foo :: (Ex.MonadError Text m, MonadIO m) => m Game
              foo = do
                if g ^. #turn == player
                  then do
                    writeGame g
                    putTextLn $ "Make your play, " <> show player <> "([0-4] [tmb] [lmr]):"

                    getLine >>= \case
                      t | Just maxDepthTx <- Tx.stripPrefix "hint " t -> do
                        maxDepth <- case Tx.R.decimal maxDepthTx of
                          Right (v, "") -> pure $ SearchParams {maxDepth = v}
                          _ -> Ex.throwError "Bad depth"

                        (bestValue, bestPlays) <- case bestNextGames maxDepth g of
                          Nothing ->
                            Ex.throwError "No more games"
                          Just (bestValue, bests) -> do
                            pure (bestValue, bests <&> fst)
                        putTextLn $ "Best plays, with value=" <> show bestValue <> ": "
                        putText $ unlines $ toList $ bestPlays <&> showPlay
                        Ex.throwError "Got hint"
                      playRaw | Just pl <- parsePlay playRaw -> do
                        let newGame = playCard pl g
                        writeGame newGame
                        putTextLn "Play made. Press enter to end turn"
                        _ <- getLine
                        pure newGame
                      _ -> do
                        putTextLn "Invalid input, go again..."
                        Ex.throwError "Invalid input"
                  else do
                    putTextLn "Playing P2..."
                    case aBestNextGame aiParams g of
                      Nothing ->
                        Ex.throwError "end"
                      Just (_bestValue, (bestPlay, _bestGame)) -> do
                        putTextLn $ "P2 plays: " <> showPlay bestPlay
                        pure $ playCard bestPlay g
          errOrnewGame <- runExceptT foo

          nextGame <- case errOrnewGame of
            Left e -> do
              putTextLn $ "Not advancing game because: '" <> e <> "'"
              pure g
            Right newGame ->
              pure newGame
          go nextGame

    writeGame g =
      liftIO $ D.SVG.renderSVG "foob.svg" (D.mkSizeSpec2D (Just 400) (Just 400)) (gameDiagram g)
