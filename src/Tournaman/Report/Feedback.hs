{-# LANGUAGE OverloadedStrings #-}

module Tournaman.Report.Feedback
( readTemplates
, feedbackSheets
) where

import           Data.Foldable (find)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.IO as Text
import           Data.Text.Template (Template, template, render)
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


data Pos = OG | OO | CG | CO
  deriving (Read, Show, Eq, Ord)

showPos :: Pos -> Text
showPos OG = "ER"
showPos OO = "EO"
showPos CG = "SR"
showPos CO = "SO"


data SingleFeedbackSheet
    = TeamForChair VenueName TeamName Pos AdjudicatorName
    | ChairForWing VenueName AdjudicatorName {- chair -} AdjudicatorName {- wing -}
    | WingForChair VenueName AdjudicatorName {- wing -} AdjudicatorName {- chair -}
  deriving (Read, Show, Eq, Ord)

compareSheetsByType :: SingleFeedbackSheet -> SingleFeedbackSheet -> Ordering
compareSheetsByType TeamForChair {} TeamForChair {} = EQ
compareSheetsByType TeamForChair {} _               = LT
compareSheetsByType ChairForWing {} TeamForChair {} = GT
compareSheetsByType ChairForWing {} ChairForWing {} = EQ
compareSheetsByType ChairForWing {} _               = LT
compareSheetsByType WingForChair {} WingForChair {} = EQ
compareSheetsByType WingForChair {} _               = GT


roomSheets :: Room -> [SingleFeedbackSheet]
roomSheets (Room venue teams adjs)
    = teamForChair ++ chairForWings ++ wingsForChair
  where
    teamForChair
        = [ TeamForChair venue team pos (head adjs)
          | (team, pos) <- zip teams [OG, OO, CG, CO]
          ]
    chairForWings
        = [ ChairForWing venue (head adjs) wing | wing <- tail adjs ]
    wingsForChair
        = [ WingForChair venue wing (head adjs) | wing <- tail adjs ]


roundSheets :: [Room] -> [SingleFeedbackSheet]
roundSheets = sortBy compareSheets . concatMap roomSheets
  where
    compareSheets = (compare `on` venue) <> compareSheetsByType

    venue (TeamForChair v _ _ _) = v
    venue (ChairForWing v _ _) = v
    venue (WingForChair v _ _) = v


-------------------------------------------------------------------------------
-- Templates

data Templates
    = Templates
    { header :: Template
    , footer :: Template
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
    <*> readTemplate "templates/team_chair.tex"
    <*> readTemplate "templates/chair_wing.tex"
    <*> readTemplate "templates/wing_chair.tex"


-------------------------------------------------------------------------------
-- Rendering

extendContext
    :: (Text -> Maybe Text)
    -> (Text -> Maybe Text)
    -> Text -> Maybe Text
extendContext f g x
    = case f x of
        Just fx -> Just fx
        Nothing -> g x


maybeContextToContext :: (Text -> Maybe Text) -> Text -> Text
maybeContextToContext f x
    = fromMaybe (error $ "unexpected template parameter: " ++ Text.unpack x) (f x)


roomContext
    :: RoundID
    -> VenueName
    -> Text
    -> Maybe Text
roomContext round room key = protectText <$>
    case key of
      "round" -> Just . Text.pack . show . unRoundID $ round
      "room"  -> Just . protectText . unVenueName $ room
      _       -> Nothing


chairWingContext
    :: AdjudicatorName {- chair -}
    -> AdjudicatorName {- wing -}
    -> Text
    -> Maybe Text
chairWingContext chair wing key = protectText <$>
    case key of
      "chair" -> Just . unAdjudicatorName $ chair
      "wing"  -> Just . unAdjudicatorName $ wing
      _       -> Nothing


singleContext
    :: RoundID
    -> SingleFeedbackSheet
    -> Text
    -> Maybe Text
singleContext round (TeamForChair room team pos chair)
    = extendContext (roomContext round room)
    $ \key -> protectText <$>
        case key of
          "chair" -> Just . unAdjudicatorName $ chair
          "team"  -> Just . unTeamName $ team
          "pos"   -> Just . showPos $ pos
          _       -> Nothing
singleContext round (ChairForWing room chair wing)
    = extendContext (roomContext round room) $ chairWingContext chair wing
singleContext round (WingForChair room wing chair)
    = extendContext (roomContext round room) $ chairWingContext chair wing




data SheetKind = KindTeamChair | KindChairWing | KindWingChair
  deriving (Read, Show, Eq, Ord)


renderRound :: Templates -> RoundID -> [Room] -> LazyText.Text
renderRound templates round rs
    =  LazyText.unlines
    $  [ render (header templates) id ]
    ++ map renderSingle (roundSheets rs)
    ++ [ render (footer templates) id ]
  where
    renderSingle :: SingleFeedbackSheet -> LazyText.Text
    renderSingle sheet = render (tpl templates) ctx
      where
        ctx = maybeContextToContext $ singleContext round sheet
        tpl = case sheet of
            TeamForChair {} -> teamForChair
            ChairForWing {} -> chairForWing
            WingForChair {} -> wingForChair


-------------------------------------------------------------------------------
-- Bringing it all together


feedbackSheets
    :: Templates
    -> RoundID
    -> Debates.Round
    -> [Adj.Adjudicator]
    -> [Teams.Team]
    -> [(VenueID, VenueName)]
    -> LazyText.Text
feedbackSheets tpls roundNum round adjs teams venues
    = renderRound tpls roundNum $ roomData round adjs teams venues
