{-# LANGUAGE OverloadedStrings #-}

module Tournaman.Report.Feedback
( readTemplates
, feedbackSheets
) where

import           Data.Foldable (find)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.IO as Text
import           Data.Text.Template (Template, Context, template, render)
import           Text.LaTeX.Base.Syntax (protectText)

import           Tournaman.Parser.Common.Types
import qualified Tournaman.Parser.Adjudicators.Types as Adj
import qualified Tournaman.Parser.Debates.Types as Debates
import qualified Tournaman.Parser.Teams.Types as Teams


unsafeFind :: (Foldable t) => (a -> Bool) -> t a -> a
unsafeFind p = fromJust . find p


-------------------------------------------------------------------------------
-- Data

data Room
    = Room
    { venueName :: VenueName
    , teams :: [TeamName]
    , adjudicators :: [AdjudicatorName]
    }
  deriving (Read, Show, Eq, Ord)


roomData
    :: Debates.Round
    -> [Adj.Adjudicator]
    -> [Teams.Team]
    -> [(VenueID, VenueName)]
    -> [Room]
roomData (Debates.Round _ rooms adjDistribution) adjs teams venues
    = map mkRoom rooms
  where
    mkRoom :: Debates.Room -> Room
    mkRoom (Debates.Room roomID roomTeams)
        = Room
        { venueName = lookupVenueName roomID
        , teams = map (lookupTeamName . Debates.teamID) roomTeams
        , adjudicators = adjsInVenue roomID
        }

    lookupTeamName :: TeamID -> TeamName
    lookupTeamName teamID
        = Teams.teamName $ unsafeFind ((== teamID) . Teams.id) teams

    adjsInVenue :: VenueID -> [AdjudicatorName]
    adjsInVenue roomID
        = map (lookupAdjName . fst)
        . filter ((== roomID) . snd)
        $ adjDistribution

    lookupAdjName :: AdjudicatorID -> AdjudicatorName
    lookupAdjName adjID = Adj.name $ unsafeFind ((== adjID) . Adj.id) adjs

    lookupVenueName :: VenueID -> VenueName
    lookupVenueName venueID = snd $ unsafeFind ((== venueID) . fst) venues


data RoomFeedbackSheets
    = RoomFeedbackSheets VenueName [SingleFeedbackSheet]


data Pos = OG | OO | CG | CO
  deriving (Read, Show, Eq, Ord)

showPos :: Pos -> Text
showPos OG = "ER"
showPos OO = "EO"
showPos CG = "SR"
showPos CO = "SO"


data SingleFeedbackSheet
    = TeamForChair TeamName Pos AdjudicatorName
    | ChairForWing AdjudicatorName {- chair -} AdjudicatorName {- wing -}
    | WingForChair AdjudicatorName {- wing -} AdjudicatorName {- chair -}
  deriving (Read, Show, Eq, Ord)

roomSheets :: Room -> RoomFeedbackSheets
roomSheets (Room venue teams adjs)
    = RoomFeedbackSheets venue
    $ teamForChair ++ chairForWings ++ wingsForChair
  where
    teamForChair
        = [ TeamForChair team pos (head adjs)
          | (team, pos) <- zip teams [OG, OO, CG, CO]
          ]
    chairForWings
        = [ ChairForWing (head adjs) wing | wing <- tail adjs ]
    wingsForChair
        = [ WingForChair wing (head adjs) | wing <- tail adjs ]


-------------------------------------------------------------------------------
-- Templates

data Templates
    = Templates
    { header :: Template
    , footer :: Template
    , room :: Template
    , teamForChair :: Template
    , chairForWing :: Template
    , wingForChair :: Template
    }

readTemplate :: FilePath -> IO Template
readTemplate = fmap template . Text.readFile

readTemplates :: IO Templates
readTemplates
    =   Templates
    <$> readTemplate "templates/header.tex"
    <*> readTemplate "templates/footer.tex"
    <*> readTemplate "templates/room.tex"
    <*> readTemplate "templates/team_chair.tex"
    <*> readTemplate "templates/chair_wing.tex"
    <*> readTemplate "templates/wing_chair.tex"


-------------------------------------------------------------------------------
-- Rendering

singleContext :: SingleFeedbackSheet -> Context
singleContext (TeamForChair team pos chair) key = protectText $
    case key of
      "chair" -> unAdjudicatorName chair
      "team"  -> unTeamName team
      "pos"   -> showPos pos
      _       -> error $ "unexpected template key: " ++ Text.unpack key

singleContext (ChairForWing chair wing) key =
    chairWingContext chair wing key

singleContext (WingForChair wing chair) key =
    chairWingContext chair wing key


chairWingContext
    :: AdjudicatorName {- chair -}
    -> AdjudicatorName {- wing -}
    -> Context
chairWingContext chair wing key = protectText $
    case key of
      "chair" -> unAdjudicatorName chair
      "wing"  -> unAdjudicatorName wing
      _       -> error $ "unexpected template key: " ++ Text.unpack key


roomContext
    :: Int {- round -}
    -> VenueName
    -> Context
roomContext round room key = protectText $
    case key of
      "round" -> Text.pack $ show round
      "room"  -> protectText $ unVenueName room
      _       -> error $ "unexpected template key: " ++ Text.unpack key


renderRoom :: Templates -> Int -> Room -> LazyText.Text
renderRoom templates round r
    = LazyText.unlines
    $ render (room templates) (roomContext round venue)
    : map renderSingleSheet sheets
  where
    (RoomFeedbackSheets venue sheets) = roomSheets r

    renderSingleSheet sheet = render (tpl templates) ctx
      where
        tpl = case sheet of
            TeamForChair {} -> teamForChair
            ChairForWing {} -> chairForWing
            WingForChair {} -> wingForChair
        ctx = singleContext sheet


renderRound :: Templates -> Int -> [Room] -> LazyText.Text
renderRound templates round rs
    =  LazyText.unlines
    $  [ render (header templates) id ]
    ++ map (renderRoom templates round) rs
    ++ [ render (footer templates) id ]


-------------------------------------------------------------------------------
-- Bringing it all together


feedbackSheets
    :: Templates
    -> Int
    -> Debates.Round
    -> [Adj.Adjudicator]
    -> [Teams.Team]
    -> [(VenueID, VenueName)]
    -> LazyText.Text
feedbackSheets tpls roundNum round adjs teams venues
    = renderRound tpls roundNum $ roomData round adjs teams venues
