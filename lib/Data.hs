{-# LANGUAGE TemplateHaskell #-}

module Data where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Tx
import Data.Text.Read qualified as Tx.R
import Types

cardsTx :: Text
cardsTx = decodeUtf8 $(makeRelativeToProject "data/cards.csv" >>= embedFile)

cardLib :: HashMap Text Card
cardLib = HM.fromList $ parseLine <$> lines cardsTx
  where
    parseLine ln =
      case Tx.splitOn "," ln of
        [name, n, s, e, w, _] ->
          (name, Card (parsePts n) (parsePts e) (parsePts s) (parsePts w))
        _ -> error $ "Bad line: " <> show ln

    parsePts t = case Tx.R.decimal t of
      Right (v, "") -> Points v
      _ -> error $ "Bad points:" <> show t

cardName :: Card -> Maybe Text
cardName c = case HM.keys (HM.filter (c ==) cardLib) of
  [] -> Nothing
  [k] -> Just k
  _ -> error "Multiple keys match"

unsafeCardName :: Card -> Text
unsafeCardName c = case cardName c of
  Nothing -> error "Unknown card"
  Just name -> name

cardFromName :: Text -> Maybe Card
cardFromName k = k `HM.lookup` cardLib

unsafeCardFromName :: Text -> Card
unsafeCardFromName k = case cardFromName k of
  Nothing -> error $ "Unknown key: " <> k
  Just c -> c

newHandFromNames :: Seq Text -> Hand
newHandFromNames ks =
  newHand (ks <&> unsafeCardFromName)
