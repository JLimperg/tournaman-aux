import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.IO as Text
import           System.Environment
import           System.IO

import           Tournaman.Parser.Common (Result, RoundID(..))
import           Tournaman.Parser.Adjudicators (parseAdjudicators)
import           Tournaman.Parser.Debates (parseDebates)
import           Tournaman.Parser.Venues (parseVenues)
import           Tournaman.Parser.Teams (parseTeams)
import           Tournaman.Report.Feedback (readTemplates, feedbackSheets)


handleErrors :: Result a -> IO a
handleErrors res
    = case res of
        Left err -> error (show err)
        Right x -> pure x


readFileLatin1 :: FilePath -> IO String
readFileLatin1 file = do
    hdl <- openFile file ReadMode
    hSetEncoding hdl latin1
    hSetNewlineMode hdl universalNewlineMode
    hGetContents hdl


(</>) :: FilePath -> FilePath -> FilePath
f </> g = f ++ "/" ++ g


main :: IO ()
main = do
    [round, dataFolder] <- getArgs

    debates <- handleErrors =<< parseDebates (dataFolder </> "debates" ++ round ++ "-main.xml")
    adjs <- handleErrors =<< parseAdjudicators (dataFolder </> "adjudicators" ++ round ++ "-main.xml")
    teams <- handleErrors =<< parseTeams (dataFolder </> "teams" ++ round ++ "-main.xml")

    venues <- handleErrors . parseVenues =<< readFileLatin1 (dataFolder </> "venues" ++ round ++ "-main.dat")

    tpls <- readTemplates

    Text.putStr . LazyText.toStrict $
      feedbackSheets tpls (RoundID $ read round) debates adjs teams venues
