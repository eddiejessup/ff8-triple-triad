module Play where

import Automation
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except qualified as Ex
import Data.List qualified as L
import Data.Text qualified as Tx
import Data.Text.Read qualified as Tx.R
import Diagrams qualified as D
import Diagrams.Backend.SVG qualified as D.SVG
import Draw
import Logic
import Nice
import Optics ((^.))
import System.Console.Haskeline qualified as Line
import Types
import Prelude hiding (head)

parseHandIx :: Text -> Maybe HandIx
parseHandIx s = case Tx.R.decimal s of
  Right (v, "") -> Just $ HandIx v
  _ -> Nothing

parseBoardIxRow :: Text -> Maybe RowPos
parseBoardIxRow = \case
  "t" -> Just RTop
  "top" -> Just RTop
  "m" -> Just RMid
  "mid" -> Just RMid
  "middle" -> Just RMid
  "b" -> Just RBot
  "bot" -> Just RBot
  "bottom" -> Just RBot
  _ -> Nothing

parseBoardIxCol :: Text -> Maybe ColPos
parseBoardIxCol = \case
  "l" -> Just CLeft
  "left" -> Just CLeft
  "m" -> Just CMid
  "mid" -> Just CMid
  "middle" -> Just CMid
  "r" -> Just CRight
  "right" -> Just CRight
  _ -> Nothing

parseNiceBoardIx :: Text -> Maybe BoardIx
parseNiceBoardIx t = do
  let parts = Tx.splitOn " " t
  (rowPart, colPart) <- case parts of
    [r, c] -> pure (r, c)
    _ -> Nothing
  row <- parseBoardIxRow rowPart
  col <- parseBoardIxCol colPart
  pure $ fromNice (row, col)

parseMove :: Text -> Maybe Move
parseMove t = do
  let (handPart, rest) = Tx.breakOn " " t
  let boardPart = Tx.drop 1 rest
  Move
    <$> parseHandIx handPart
    <*> parseNiceBoardIx boardPart

unsafeParseMove :: Text -> Move
unsafeParseMove t = case parseMove t of
  Just p -> p
  Nothing -> error "Bad text"

data PlayConfig = PlayConfig {game :: Game, p1Config :: PlayerConfig, p2Config :: PlayerConfig}
  deriving (Show)

data PlayerConfig = PlayerConfig {isBot :: Bool, searchParams :: SearchParams}
  deriving (Show)

data PlayGameError
  = RecoverablePlayGameError Text
  | UnrecoverablePlayGameError Text

playGame :: forall m. (MonadIO m, MonadMask m) => FilePath -> PlayConfig -> m ()
playGame svgPath PlayConfig {game = game0, p1Config, p2Config} =
  Line.runInputT Line.defaultSettings (go game0)
  where
    go :: Game -> Line.InputT m ()
    go g = do
      outputTxLn $ "Score: P1=" <> show (score P1 g) <> ", P2=" <> show (score P2 g)
      outputTxLn ""

      if null (possibleNextGames g)
        then do
          outputTxLn "It's over!"
          writeGame g
        else do
          nextGame <-
            runExceptT (progressGame g) >>= \case
              Left (RecoverablePlayGameError e) -> do
                outputTxLn $ "*** Redoing turn because of recoverable error: '" <> e <> "' ***"
                pure g
              Left (UnrecoverablePlayGameError e) -> do
                outputTxLn $ "*** Ending play because of unrecoverable error: '" <> e <> "' ***"
                error e
              Right nextGame -> do
                outputTxLn "==== End of turn ===\n"
                pure nextGame

          go nextGame

    progressGame :: Game -> ExceptT PlayGameError (Line.InputT m) Game
    progressGame g = do
      let turnPlayer = g ^. #turn

          turnPlayerConfig = case turnPlayer of
            P1 -> p1Config
            P2 -> p2Config

      if not (isBot turnPlayerConfig)
        then do
          writeGame g

          let possHixs = availableHandIxs (relevantHand g)
          lift $ outputTxLn "Hand-index options:"
          lift $ for_ possHixs \hix -> do
            outputTxLn $ show (unHandIx hix) <> ": " <> playedCardName g hix

          let possBixs = availableBoardIxs (board g)
          lift $ outputTxLn ""
          lift $ outputTxLn "Board-position options:"
          lift $ for_ possBixs \bix -> do
            outputTxLn $ showBoardIxNice bix

          let movePrompt = "Make your move, " <> show turnPlayer <> " (<hand-index> <[t]op|[m]iddle|[b]ottom> <[l]eft|[c]enter|[r]ight>):"
          lift $ outputTxLn ""
          promptGet movePrompt >>= \case
            -- User wants a wee hint, bless them.
            cmdTx | Just maxDepthTx <- Tx.stripPrefix "hint " cmdTx -> do
              maxDepth <- parseInt maxDepthTx
              let hintSearchParams = SearchParams {maxDepth}

              (bestValue, bestMoves) <- case bestNextGames hintSearchParams g of
                Nothing ->
                  Ex.throwError (UnrecoverablePlayGameError "No more games")
                Just (bestValue, bests) -> do
                  pure (bestValue, bests <&> fst)
              lift $ outputTxLn $ "Best moves, with value=" <> show bestValue <> ": "

              let ixedBestMoves = zip [1 ..] (toList bestMoves)
              lift $
                for_ ixedBestMoves $ \(i :: Int, mv) -> do
                  outputTxLn $ show i <> ". " <> showMove g mv

              pickedMoveNr <- promptGet "Pick hinted move to play" >>= parseInt
              case L.lookup pickedMoveNr ixedBestMoves of
                Nothing -> Ex.throwError (RecoverablePlayGameError "Invalid hint index")
                Just hintMove ->
                  makeManualMove g hintMove

            -- User wants to make a move.
            mvRaw
              | Just mv <- parseMove mvRaw ->
                makeManualMove g mv
            _ -> do
              lift $ outputTxLn "Invalid input, go again..."
              Ex.throwError (RecoverablePlayGameError "Invalid input")
        else do
          lift $ outputTxLn $ "Playing " <> show turnPlayer <> " as bot..."
          case aBestNextGame (searchParams turnPlayerConfig) g of
            Nothing ->
              Ex.throwError (UnrecoverablePlayGameError "Bot saw no options")
            Just (_bestValue, (bestMove, _bestGame)) -> do
              lift $ outputTxLn $ "P2 plays: " <> showMove g bestMove
              pure $ makeMove g bestMove

    promptGet prompt = do
      lift $ outputTxLn prompt
      lift (Line.getInputLine "> ") <&> fmap Tx.pack >>= \case
        Nothing -> throwUserExitError
        Just t -> pure t

    throwUserExitError = Ex.throwError (UnrecoverablePlayGameError "User exit")

    makeManualMove :: Game -> Move -> ExceptT PlayGameError (Line.InputT m) Game
    makeManualMove g mv = do
      let nextGame = makeMove g mv
      writeGame nextGame
      lift (Line.waitForAnyKey "Move made. Press any key to end turn") >>= \case
        False -> throwUserExitError
        True -> pure nextGame

    parseInt t = do
      case Tx.R.decimal t of
        Right (v, "") -> pure v
        _ -> Ex.throwError $ RecoverablePlayGameError "Bad int"

    writeGame :: MonadIO n => Game -> n ()
    writeGame g =
      liftIO $ D.SVG.renderSVG svgPath (D.mkSizeSpec2D (Just 400) (Just 400)) (gameDiagram g)

outputTx :: MonadIO m => Text -> Line.InputT m ()
outputTx = Line.outputStr . Tx.unpack

outputTxLn :: MonadIO m => Text -> Line.InputT m ()
outputTxLn = Line.outputStrLn . Tx.unpack
