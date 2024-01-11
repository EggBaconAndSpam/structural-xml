module Data.XML.Parse.Location
  ( Location (..),
    Step (..),
    printPath,
    annotateDocument,
    annotateElement,
    annotateElementAt,
  )
where

import qualified Data.Map.Strict as Map
import Data.XML.Types
import Text.XML (Name)

data Step = EnterElement Name | SkipContent | SkipElement Name

newtype Location = Location {reversedPath :: [Step]}

annotateDocument :: Document -> AnnotatedDocument Location
annotateDocument Document {..} =
  Document
    { root = annotateElementAt (Location [EnterElement rootName]) root,
      info = Location [],
      ..
    }

annotateElement :: Element -> AnnotatedElement Location
annotateElement = annotateElementAt (Location [])

annotateElementAt :: Location -> Element -> AnnotatedElement Location
annotateElementAt here el = do
  Element
    { attributes = Map.map (\(t, _) -> (t, here)) $ attributes el,
      children = nodesWithPaths here $ children el,
      info = here
    }
  where
    nodesWithPaths :: Location -> [Node] -> [AnnotatedNode Location]
    nodesWithPaths _ [] = []
    nodesWithPaths (Location path) (node : rest) = case node of
      NodeContent text _ ->
        NodeContent text (Location path) :
        nodesWithPaths (Location $ SkipContent : path) rest
      NodeElement name child _ ->
        let childWithPaths = annotateElementAt (Location $ EnterElement name : path) child
         in NodeElement name childWithPaths (Location path) :
            nodesWithPaths (Location $ SkipElement name : path) rest

printPath :: Location -> [String]
printPath loc = map printStep . reverse $ reversedPath loc
  where
    printStep = \case
      EnterElement name -> "enter node \"" <> renderName name <> "\""
      SkipElement name -> "skip node \"" <> renderName name <> "\""
      SkipContent -> "skip content node"
