{-
How to use this library
  TODO!

Decoding and encoding XML documents
    decodeDocument, encodeDocument, decodeDocumentLT, encodeDocumentLT, decodeDocumentBS, encodeDocumentBS, decodeDocumentLBS, encodeDocumentLBS,

Deriving via helpers for Read and Show
ReadShowXmlDocument, ReadShowXmlElement

Representing XML

    XML document types
    AnnotatedDocument, AnnotatedElement, AnnotatedNode, Document, Element, Node, unAnnotateDocument, emptyElement, isEmptyElement

    helpers
    renderName, stripAllWhitespaceContent, stripAllWhitespaceContent', stripAllNamespaces, stripAllNamespaces',

    Conversion to and from xml-conduit types
    fromXmlConduit, fromXmlConduitKeepWhitespaceContent, fromXmlConduitElement, toXmlConduit, toXmlConduitElement

Parsing and Unparsing XML

    newtype wrappers
    ContentElement, OrEmpty

    Unparsing

        Classes
        ToDocument, toRootElement, ToElement, ToContent, ToChoiceElement

        Constructing elements
        ConstructM, constructElement, appendContent, appendElement, appendElementOrEmpty, appendChoiceElement,addAttribute,

    Parsing

        Parser, ParserError, prettyParserError, prettyParserErrorWithCallStack

        Classes
        FromDocument, fromRootElement, FromElement, FromChoiceElement (..), FromContent, readContent

        Parsing content and attributes
        parseContentElement, parseContentElementPartially, parseContentElementKeepLeftovers,

        Parsing Ordered elements (c.f. 'sequence')
        -> Data.XML.Parse.Ordered

        Parsing Unordered elements (c.f. 'sequence')
        -> Data.XML.Parse.Unordered

-}
module Data.XML
  ( -- * Re-exports. TODO: Explicit re-exports!
    module Data.XML.Types,
    module Data.XML.Parse.Types,
    module Data.XML.Parse.Location,
    module Data.XML.Unparse,

    -- * Decoding and encoding XML documents
    decodeDocument,
    encodeDocument,
    decodeDocumentLT,
    encodeDocumentLT,
    decodeDocumentBS,
    encodeDocumentBS,
    decodeDocumentLBS,
    encodeDocumentLBS,

    -- * @deriving via@ helpers for @Read@ and @Show@ instances
    ReadShowXmlDocument (..),
    ReadShowXmlElement (..),
  )
where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy as Text.Lazy
import Data.XML.Parse.Location
import Data.XML.Parse.Types
import Data.XML.Types
import Data.XML.Unparse
import GHC.Stack
import Text.XML (def, parseLBS, parseText, renderLBS, renderText, rsPretty, rsXMLDeclaration)

decodeDocument :: (HasCallStack, FromDocument a) => Text -> Either String a
decodeDocument raw = case parseText def (Text.Lazy.fromStrict raw) of
  Left err -> Left $ show err
  Right conduitDoc ->
    first prettyParserError . fromDocument . annotateDocument $
      fromXmlConduit conduitDoc

decodeDocumentLT :: (HasCallStack, FromDocument a) => LT.Text -> Either String a
decodeDocumentLT raw = case parseText def raw of
  Left err -> Left $ show err
  Right conduitDoc ->
    first prettyParserError . fromDocument . annotateDocument $
      fromXmlConduit conduitDoc

decodeDocumentBS :: (HasCallStack, FromDocument a) => ByteString -> Either String a
decodeDocumentBS raw = case parseLBS def (ByteString.Lazy.fromStrict raw) of
  Left err -> Left $ show err
  Right conduitDoc ->
    first prettyParserError . fromDocument . annotateDocument $
      fromXmlConduit conduitDoc

decodeDocumentLBS :: (HasCallStack, FromDocument a) => BL.ByteString -> Either String a
decodeDocumentLBS raw = case parseLBS def raw of
  Left err -> Left $ show err
  Right conduitDoc ->
    first prettyParserError . fromDocument . annotateDocument $
      fromXmlConduit conduitDoc

encodeDocument :: ToDocument a => a -> Text
encodeDocument =
  Text.Lazy.toStrict
    . renderText (def {rsPretty = True})
    . toXmlConduit
    . toDocument

encodeDocumentLT :: ToDocument a => a -> LT.Text
encodeDocumentLT =
  renderText (def {rsPretty = True})
    . toXmlConduit
    . toDocument

encodeDocumentBS :: ToDocument a => a -> ByteString
encodeDocumentBS =
  ByteString.Lazy.toStrict
    . renderLBS (def {rsPretty = True})
    . toXmlConduit
    . toDocument

encodeDocumentLBS :: ToDocument a => a -> BL.ByteString
encodeDocumentLBS =
  renderLBS (def {rsPretty = True})
    . toXmlConduit
    . toDocument

newtype ReadShowXmlDocument a = ReadShowXmlDocument a

instance ToDocument a => Show (ReadShowXmlDocument a) where
  show (ReadShowXmlDocument a) =
    Text.Lazy.unpack . renderText (def {rsPretty = True}) . toXmlConduit $ toDocument a

instance FromDocument a => Read (ReadShowXmlDocument a) where
  readsPrec _ str = case decodeDocument (Text.pack str) of
    Right a -> [(ReadShowXmlDocument a, "")]
    Left err -> error err

newtype ReadShowXmlElement a = ReadShowXmlElement a

instance FromElement a => FromDocument (ReadShowXmlElement a) where
  fromDocument Document {root} = ReadShowXmlElement <$> fromElement root

deriving via ReadShowXmlDocument (ReadShowXmlElement a) instance FromElement a => Read (ReadShowXmlElement a)

instance ToElement a => Show (ReadShowXmlElement a) where
  show (ReadShowXmlElement a) =
    Text.Lazy.unpack . renderText (def {rsPretty = True, rsXMLDeclaration = False}) $
      toXmlConduit
        Document {root = toElement a, rootName = "root_element", info = ()}
