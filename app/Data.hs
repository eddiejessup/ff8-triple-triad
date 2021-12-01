{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Data where

import Types

cardLib :: HashMap String Card
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
