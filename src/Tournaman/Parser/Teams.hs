module Tournaman.Parser.Teams
( parseTeams
, module Tournaman.Parser.Teams.Types
) where

import           Control.Monad
import qualified Text.XML.Light as XML

import           Tournaman.Parser.Common
import           Tournaman.Parser.Teams.Types


parseTeams :: FilePath -> IO (Result [Team])
parseTeams = fmap (parseRound <=<  parseXMLDoc) . readFile


parseRound :: XML.Element -> Result [Team]
parseRound = mapM parseTeam <=< findChildren "team"


parseTeam :: XML.Element -> Result Team
parseTeam team = do
    teamName <- TeamName <$> findAttrText "name" team
    id <- TeamID <$> findAttr "ident" team
    institutions <- map (Institution . strContent) <$> findChildren "home" team
    [speaker1, speaker2] <- findNChildren 2 "member" team
    speaker1Name <- SpeakerName <$> findAttrText "name" speaker1
    speaker2Name <- SpeakerName <$> findAttrText "name" speaker2
    return $ Team id teamName institutions speaker1Name speaker2Name
