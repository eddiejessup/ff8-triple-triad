{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Data where

import Data.HashMap.Strict qualified as HM
import Data.Vector (Vector)
import Types

cardLib :: HashMap Text Card
cardLib =
  [ ("Geezard", Card 1 4 1 5),
    ("Funguar", Card 5 1 1 3),
    ("Bite Bug", Card 1 3 3 5),
    ("Red Bat", Card 6 1 1 2),
    ("Blobra", Card 2 3 1 5),
    ("Gayla", Card 2 1 4 4),
    ("Gesper", Card 1 5 4 1),
    ("Fasticholon-F", Card 3 5 2 1),
    ("Blood Soul", Card 2 1 6 1),
    ("Caterchapillar", Card 4 2 4 3),
    ("Cockatrice", Card 2 1 2 6)
  ]

newHandFromNames :: Vector Text -> Hand
newHandFromNames ks =
  let cards =
        ks <&> \k -> case k `HM.lookup` cardLib of
          Nothing -> error $ "Unknown key: " <> k
          Just c -> c
   in newHand cards

cardBest :: Card
cardBest = Card {n = 10, e = 10, s = 10, w = 10}

cardWorst :: Card
cardWorst = Card {n = 1, e = 1, s = 1, w = 1}
