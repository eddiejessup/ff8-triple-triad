{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Parse where

import Automation (SearchParams (..))
import Data
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson qualified as Ae
import Data.Aeson.Types (unexpected)
import Data.Scientific qualified as Sc
import Data.Sequence qualified as Seq
import Logic
import Play
import Types

instance Ae.FromJSON PlayConfig where
  parseJSON = Ae.withObject "PlayConfig" $ \v -> do
    (p1Conf, p2Conf) <-
      v .: "players" >>= Ae.withObject "PlayersConfig" \playersConfObj ->
        (,)
          <$> playersConfObj .: "p1"
          <*> playersConfObj .: "p2"
    PlayConfig
      <$> v .: "game"
      <*> pure p1Conf
      <*> pure p2Conf

instance Ae.FromJSON PlayerConfig where
  parseJSON = Ae.withObject "PlayerConfig" $ \v ->
    PlayerConfig
      <$> v .: "bot"
      <*> v .: "search-params"

instance Ae.FromJSON SearchParams where
  parseJSON = Ae.withObject "SearchParams" $ \v ->
    SearchParams
      <$> v .: "max-depth"

instance Ae.FromJSON Game where
  parseJSON = Ae.withObject "Game" $ \v -> do
    (h1, h2) <-
      v .: "hands" >>= Ae.withObject "Hands" \handsObj -> do
        (,) <$> handsObj .: "p1" <*> handsObj .: "p2"
    game0 <-
      initialGame
        h1
        h2
        <$> v .: "first-player"
    moves <- v .: "moves"
    pure $ makeMoves @[] game0 moves

instance Ae.FromJSON Hand where
  parseJSON = Ae.withArray "Hand" $ \vs -> do
    cards <- sequence (parseJSON <$> vs)
    pure $ Hand $ Seq.fromList $ toList $ cards <&> \c -> Just c

instance Ae.FromJSON Card where
  parseJSON = Ae.withText "Card" $ \v -> do
    case cardFromName v of
      Nothing -> fail "Unknown card name"
      Just c -> pure c

instance Ae.FromJSON Player where
  parseJSON = Ae.withScientific "Player" $ \case
    1 -> pure P1
    2 -> pure P2
    _ -> fail "Unknown player number"

instance Ae.FromJSON Move where
  parseJSON = \case
    Ae.Object v ->
      Move <$> v .: "card" <*> v .: "position"
    Ae.String t -> do
      case parseMove t of
        Nothing -> fail "Couldn't parse short-hand move"
        Just m -> pure m
    v ->
      unexpected v

instance Ae.FromJSON BoardIx where
  parseJSON = \case
    Ae.Number sN -> case Sc.toBoundedInteger @Int sN of
      Nothing -> fail "Got non-integer number"
      Just bixInt -> pure $ BoardIx bixInt
    Ae.String s -> case parseNiceBoardIx s of
      Nothing -> fail "Could not parse board index"
      Just bix -> pure bix
    v -> unexpected v
