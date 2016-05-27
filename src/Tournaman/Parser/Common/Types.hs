module Tournaman.Parser.Common.Types where

import           Data.Text (Text)

newtype Motion = Motion { unMotion :: Text }
  deriving (Read, Show, Eq, Ord)

newtype TeamID = TeamID { unTeamID :: Int }
  deriving (Read, Show, Eq, Ord)

newtype VenueID = VenueID { unVenueID :: Int }
  deriving (Read, Show, Eq, Ord)

newtype RoundID = RoundID { unRoundID :: Int }
  deriving (Read, Show, Eq, Ord)

newtype AdjudicatorID = AdjudicatorID { unAdjudicatorID :: Int }
  deriving (Read, Show, Eq, Ord)

newtype Rank = Rank { unRank :: Int }
  deriving (Read, Show, Eq, Ord)

newtype Class = Class { unClass :: Int }
  deriving (Read, Show, Eq, Ord)

newtype Speaks = Speaks { unSpeaks :: Int }
  deriving (Read, Show, Eq, Ord)

newtype TeamName = TeamName { unTeamName :: Text }
  deriving (Read, Show, Eq, Ord)

newtype SpeakerName = SpeakerName { unSpeakerName :: Text }
  deriving (Read, Show, Eq, Ord)

newtype AdjudicatorName = AdjudicatorName { unAdjudicatorName :: Text }
  deriving (Read, Show, Eq, Ord)

newtype VenueName = VenueName { unVenueName :: Text }
  deriving (Read, Show, Eq, Ord)

newtype Institution = Institution { unInstitution :: Text }
  deriving (Read, Show, Eq, Ord)
