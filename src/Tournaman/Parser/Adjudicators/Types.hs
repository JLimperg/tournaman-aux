module Tournaman.Parser.Adjudicators.Types where

import           Tournaman.Parser.Common.Types

data Adjudicator
    = Adjudicator
    { id :: AdjudicatorID
    , name :: AdjudicatorName
    , klass :: Class
    , institutions :: [Institution]
    , teamConflicts :: [TeamID]
    }
  deriving (Read, Show, Eq, Ord)
