module Tournaman.Parser.Common
( Result
, readResult
, strContent
, ParseError(..)
, parseXMLDoc
, findChildren
, findNChildren
, findMinNChildren
, findChildrenNonEmpty
, findSingleChild
, findAttrText
, findAttr
, module Tournaman.Parser.Common.Types
) where

import           Control.Monad
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Read (readMaybe)
import qualified Text.XML.Light as XML
import qualified Text.XML.Light.Lexer as XML (XmlSource)

import           Tournaman.Parser.Common.Types


type Result a = Either ParseError a


readResult :: (Read a) => String -> Result a
readResult val = maybeToEither (UnexpectedAttrFormat val) $ readMaybe val


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a = maybe (Left a) Right


simplifyName :: XML.QName -> String
simplifyName = XML.qName

complicateName :: String -> XML.QName
complicateName name = XML.QName name Nothing Nothing


strContent :: XML.Element -> Text
strContent = Text.pack . XML.strContent


data ParseError
    = UnexpectedElement !String {- expected -} !String {- actual -}
    | UnexpectedNumberOfElements !String {- name -} !Int {- expected -} !Int {- actual -}
    | TooFewElements !String {- name -} !Int {- expected -} !Int {- actual -}
    | NoSuchAttribute !String
    | UnexpectedAttrFormat !String
    | MalformedDocument
  deriving (Read, Show, Eq, Ord)


parseXMLDoc :: (XML.XmlSource s) => s -> Result XML.Element
parseXMLDoc = maybeToEither MalformedDocument . XML.parseXMLDoc

findChildren :: String -> XML.Element -> Result [XML.Element]
findChildren name
    = pure . XML.filterChildren ((== name) . simplifyName . XML.elName)

findNChildren :: Int -> String -> XML.Element -> Result [XML.Element]
findNChildren expectedLength name
    = ensureLength <=< findChildren name
  where
    ensureLength children
        | expectedLength == actualLength = Right children
        | otherwise
        = Left $ UnexpectedNumberOfElements name expectedLength actualLength
      where
        actualLength = length children

findMinNChildren :: Int -> String -> XML.Element -> Result [XML.Element]
findMinNChildren minLength name
    = ensureLength <=< findChildren name
  where
    ensureLength children
        | minLength >= actualLength = Right children
        | otherwise
        = Left $ TooFewElements name minLength actualLength
      where
        actualLength = length children

findChildrenNonEmpty :: String -> XML.Element -> Result (NonEmpty XML.Element)
findChildrenNonEmpty name = fmap NonEmpty.fromList . findMinNChildren 1 name

findSingleChild :: String -> XML.Element -> Result XML.Element
findSingleChild name = fmap head . findNChildren 1 name


findAttrString :: String -> XML.Element -> Result String
findAttrString name
    = maybeToEither (NoSuchAttribute name) . XML.findAttr (complicateName name)

findAttrText :: String -> XML.Element -> Result Text
findAttrText name = fmap Text.pack . findAttrString name

findAttr :: (Read a) => String -> XML.Element -> Result a
findAttr name = readResult <=< findAttrString name
