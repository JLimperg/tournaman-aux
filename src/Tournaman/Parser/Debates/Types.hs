module Tournaman.Parser.Debates.Types where

import           Tournaman.Parser.Common.Types

data Team
    = Team
    { teamID :: !TeamID
    , rankPoints :: !Rank
    , speaks1 :: !Speaks
    , speaks2 :: !Speaks
    }
  deriving (Read, Show, Eq, Ord)

data Room
    = Room
    { roomID :: VenueID
    , teams :: [Team]
    }
  deriving (Read, Show, Eq, Ord)

data Round
    = Round
    { motion :: Motion
    , rooms :: [Room]
    , adjudicators :: [(AdjudicatorID, VenueID)]
    }
  deriving (Read, Show, Eq, Ord)
