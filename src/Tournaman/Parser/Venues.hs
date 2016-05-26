module Tournaman.Parser.Venues
( parseVenues
) where

import           Data.List.Split (split, keepDelimsL, oneOf)
import qualified Data.Text as Text

import           Tournaman.Parser.Common (Result, ParseError(..), readResult)
import           Tournaman.Parser.Common.Types

parseVenues :: String -> Result [(VenueID, VenueName)]
parseVenues = mapM (mkAssoc . split (keepDelimsL $ oneOf " ")) . lines
  where
    mkAssoc [_]
        = Left MalformedDocument
    mkAssoc (id : names)
        =   (,)
        <$> (fmap VenueID . readResult $ id)
        <*> pure (VenueName . Text.pack . drop 1 . concat $ names)
    mkAssoc _
        = Left MalformedDocument
