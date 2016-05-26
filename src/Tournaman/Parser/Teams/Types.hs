module Tournaman.Parser.Teams.Types where

import           Tournaman.Parser.Common.Types

data Team
    = Team
    { id :: TeamID
    , teamName :: TeamName
    , institutions :: [Institution]
    , speaker1Name :: SpeakerName
    , speaker2Name :: SpeakerName
    }
  deriving (Read, Show, Eq, Ord)
