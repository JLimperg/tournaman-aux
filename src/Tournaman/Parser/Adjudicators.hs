module Tournaman.Parser.Adjudicators where

import           Control.Monad
import qualified Text.XML.Light as XML

import           Tournaman.Parser.Common
import           Tournaman.Parser.Adjudicators.Types


parseAdjudicators :: FilePath -> IO (Result [Adjudicator])
parseAdjudicators
    = fmap (mapM parseAdjudicator <=< findChildren "adjud" <=< parseXMLDoc)
    . readFile


parseAdjudicator :: XML.Element -> Result Adjudicator
parseAdjudicator adj = do
    id <- AdjudicatorID <$> findAttr "id" adj
    name <- AdjudicatorName <$> findAttrText "name" adj
    klass <- Class <$> findAttr "class" adj
    institutions <- map (Institution . strContent) <$> findChildren "home" adj
    teamConflicts <- mapM (fmap TeamID . readResult . XML.strContent)
                       =<< findChildren "teamconflict" adj
    return $ Adjudicator id name klass institutions teamConflicts
