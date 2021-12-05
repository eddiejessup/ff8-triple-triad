{-# LANGUAGE TemplateHaskell #-}

module Data where

import Data.HashMap.Strict qualified as HM
import Types
import Data.FileEmbed (makeRelativeToProject, embedFile)
import Data.Text qualified as Tx
import Data.Text.Read qualified as Tx.R

cardsTx :: Text
cardsTx = decodeUtf8 $(makeRelativeToProject "data/cards.csv" >>= embedFile)

cardLib :: HashMap Text Card
cardLib = HM.fromList (parseLine <$> lines cardsTx)
  where
    parseLine ln =
      case Tx.splitOn "," ln of
        [name, n, s, e, w, _] ->
          (name, Card (parsePts n) (parsePts e) (parsePts s) (parsePts w))
        _ -> error $ "Bad line: " <> show ln

    parsePts t = case Tx.R.decimal t of
      Right (v, "") -> v
      _ -> error $ "Bad points:" <> show t

-- cardLib :: HashMap Text Card
-- cardLib =
--   [ ("Geezard", Card 1 4 1 5),
--     ("Funguar", Card 5 1 1 3),
--     ("Bite Bug", Card 1 3 3 5),
--     ("Red Bat", Card 6 1 1 2),
--     ("Blobra", Card 2 3 1 5),
--     ("Gayla", Card 2 1 4 4),
--     ("Gesper", Card 1 5 4 1),
--     ("Fasticholon-F", Card 3 5 2 1),
--     ("Blood Soul", Card 2 1 6 1),
--     ("Caterchapillar", Card 4 2 4 3),
--     ("Cockatrice", Card 2 1 2 6),

--     ("Grat", Card 7 1 3 1),
--     ("Buel", Card 6 2 2 3),
--     ("Mesmerize", Card 5 3 3 4),

--     ("Best", cardBest)

--   ]

newHandFromNames :: Seq Text -> Hand
newHandFromNames ks =
  let cards =
        ks <&> \k -> case k `HM.lookup` cardLib of
          Nothing -> error $ "Unknown key: " <> k
          Just c -> c
   in newHand cards
