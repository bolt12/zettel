{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import Control.Exception (Exception, IOException, SomeException, catch)
import qualified Data.ByteString as B
import Data.ByteString hiding (map, pack, unpack)
import qualified Data.Text as TT (unpack)
import Data.Text (pack)
import qualified Data.Text.Lazy as TL (unpack)
import qualified Data.Time as T
import Data.Time.Format
import qualified Database.Bolt as DB
import Neo4JEffect
import Options.Generic
import PandocParse
import Polysemy
import Polysemy.Error hiding (catch)
import Polysemy.Trace
import System.Directory
import System.Environment
import Text.Editor
import Text.Pandoc hiding (trace)
import Text.Pretty.Simple (pShow)
import Types

data Options w
  = New
  | List
      { size :: w ::: Int <?> "List size"
      }
  | Find
      { tags :: w ::: [String] <?> "Tags to search for"
      }
  | Delete
      { did :: w ::: Int <?> "Zettel id to delete"
      }
  | Edit
      { eid :: w ::: Int <?> "Zettel id to edit"
      }
  deriving (Generic)

instance ParseRecord (Options Wrapped)

deriving instance Show (Options Unwrapped)

data ZettelError
  = P PandocError
  | D DependenciesFoundError
  | N NodeNotFoundError
  | C ConnectionError
  deriving (Show, Exception)

template :: ByteString
template =
  "\
  \---\n\
  \title: \"Test Zettel\"\n\
  \author: A, B\n\
  \tags: T1, T2\n\
  \connections: |\n\
  \  [\n\
  \    {\n\
  \      \"id\": -1,\n\
  \      \"reason\": \"\"\n\
  \    }\n\
  \  ]\n\
  \...\n\
  \\n\
  \## Content\n"

mainProg :: Members '[Neo4J, Error ZettelError, Trace, Embed IO] r => Sem r ()
mainProg = do
  x <- unwrapRecord "Zettelkasten processor"
  case x of
    List s -> do
      r <- listNodes s
      trace . TL.unpack . pShow $ r
    New -> do
      r <- embed $ runUserEditorDWIM markdownTemplate template
      timestamp <- embed T.getCurrentTime
      home <- embed $ getEnv "HOME"
      let formated = formatTime defaultTimeLocale "%d-%m-%YT%H:%M:%S" timestamp
          filename = formated ++ ".md"
          zettelsFile = home ++ "/.config/zettel/" ++ filename
      embed $ B.writeFile zettelsFile r
      p <- embed . runIO $ readZettel zettelsFile
      case p of
        Left e -> throw $ P e
        Right pandoc -> do
          zettel <- embed . runIO $ createZettel formated pandoc
          case zettel of
            Left e -> throw $ P e
            Right z -> createNode z
    Find t -> do
      r <- findNodes t
      trace . TL.unpack . pShow $ r
    Delete zid -> deleteNode (ZID zid)
    Edit zid -> do
      home <- embed $ getEnv "HOME"
      zt <- getNode (ZID zid)
      case zt of
        Nothing -> throw . N $ NodeNotFoundError "No Zettel with given ID found"
        (Just z) -> do
          let filename = TT.unpack (getTimestamp z) ++ ".md"
              filepath = home ++ "/.config/zettel/" ++ filename
          r <- embed $ runUserEditorDWIMFile markdownTemplate filepath
          embed $ B.writeFile filepath r
          p <- embed . runIO $ readZettel filepath
          case p of
            Left e -> throw $ P e
            Right pandoc -> do
              zettel <- embed . runIO $ createZettel (TT.unpack $ getTimestamp z) pandoc
              case zettel of
                Left e -> throw $ P e
                Right z -> editNode (z {getId = ZID zid})

runMain :: User -> Password -> IO (Either ZettelError ())
runMain user pass =
  runM
    . runError @ZettelError
    . mapError C
    . mapError D
    . neo4jToIO user pass
    . traceToIO
    $ mainProg

main :: IO ()
main = do
  home <- getEnv "HOME"
  b <- doesFileExist (home ++ "/.config/zettel/zettel-conf")
  [user, pass] <-
    map pack
      <$> if b
        then words <$> Prelude.readFile (home ++ "/.config/zettel/zettel-conf")
        else do
          createDirectoryIfMissing False (home ++ "/.config/zettel")
          Prelude.writeFile
            (home ++ "/.config/zettel/zettel-conf")
            "neo4j neo4j"
          return ["neo4j", "neo4j"]
  r <- runMain user pass
  case r of
    Left e -> error (show e)
    Right x -> return x
