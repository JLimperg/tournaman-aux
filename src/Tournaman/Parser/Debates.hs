module Tournaman.Parser.Debates
( parseDebates
) where

import           Control.Monad
import qualified Data.Text as T
import qualified Text.XML.Light as XML

import           Tournaman.Parser.Common
import           Tournaman.Parser.Debates.Types

parseDebates :: FilePath -> IO (Result Round)
parseDebates = fmap (parseRound <=< parseXMLDoc) . readFile

parseRound :: XML.Element -> Result Round
parseRound round = do
    motion <- parseMotion =<< findSingleChild "motion" round
    rooms <- mapM parseRoom =<< findChildren "debate" round
    adjudicators <- parseAdjudicators =<< findSingleChild "adjudicators" round
    return $ Round motion rooms adjudicators


parseMotion :: XML.Element -> Result Motion
parseMotion = pure . Motion . T.pack . XML.strContent


parseRoom :: XML.Element -> Result Room
parseRoom room = do
    id <- VenueID <$> findAttr "venue" room
    teams <- mapM parseTeam =<< findNChildren 4 "team" room
    return $ Room id teams

parseTeam :: XML.Element -> Result Team
parseTeam team = do
  id <- TeamID <$> findAttr "id" team
  rank <- Rank <$> findAttr "rankpts" team
  [speaker1, speaker2] <- findNChildren 2 "speaker" team
  speaks1 <- Speaks <$> findAttr "points" speaker1
  speaks2 <- Speaks <$> findAttr "points" speaker2
  return $ Team id rank speaks1 speaks2


parseAdjudicators :: XML.Element -> Result [(AdjudicatorID, VenueID)]
parseAdjudicators = mapM parseAssoc <=< findChildren "pair"
  where
    parseAssoc assoc
        =   (,)
        <$> (AdjudicatorID <$> findAttr "adj" assoc)
        <*> (VenueID <$> findAttr "venue" assoc)
