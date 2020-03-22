{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import qualified Data.ByteString as B
import Data.ByteString hiding (unpack, pack, map)
import qualified Data.Time as T
import Data.Time.Format
import qualified Database.Bolt as DB
import Neo4JEffect
import Types
import Options.Generic
import PandocParse
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Trace
import System.Directory
import System.Environment
import Text.Editor
import qualified Data.Text.Lazy as TL (unpack)
import qualified Data.Text as TT (unpack)
import Data.Text (pack)
import Text.Pandoc hiding (trace)
import Text.Pretty.Simple (pShow)

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

newtype ZettelError = ZettelError (Either PandocError (Either DependenciesFoundError NodeNotFoundError))
  deriving (Show)

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
        Left e -> throw . ZettelError $ Left e
        Right pandoc -> do
          zettel <- embed . runIO $ createZettel formated pandoc
          case zettel of
            Left e -> throw . ZettelError $ Left e
            Right z -> createNode z
    Find t -> do
      r <- findNodes t
      trace . TL.unpack . pShow $ r
    Delete zid -> deleteNode (ZID zid)
    Edit zid -> do
      home <- embed $ getEnv "HOME"
      zt <- getNode (ZID zid)
      case zt of
        Nothing -> throw . ZettelError . Right . Right $ NodeNotFoundError "No Zettel with given ID found"
        (Just z) -> do
          let filename = TT.unpack (getTimestamp z) ++ ".md"
              filepath = home ++ "/.config/zettel/" ++ filename
          r <- embed $ runUserEditorDWIMFile markdownTemplate filepath
          embed $ B.writeFile filepath r
          p <- embed . runIO $ readZettel filepath
          case p of
            Left e -> throw . ZettelError $ Left e
            Right pandoc -> do
              zettel <- embed . runIO $ createZettel (TT.unpack $ getTimestamp z) pandoc
              case zettel of
                Left e -> throw . ZettelError $ Left e
                Right z -> editNode (z { getId = ZID zid })

runMain :: DB.Pipe -> IO (Either ZettelError ())
runMain pipe =
  runM
    . traceToIO
    . runInputConst pipe
    . runError @ZettelError
    . mapError (ZettelError . Right . Left)
    . neo4jToIO
    $ mainProg

main :: IO ()
main = do
  home <- getEnv "HOME"
  b <- doesFileExist (home ++ "/.config/zettel/zettel-conf")
  [user, pass] <-
    map pack <$>
    if b
      then words <$> Prelude.readFile (home ++ "/.config/zettel/zettel-conf")
      else do
        createDirectoryIfMissing False (home ++ "/.config/zettel")
        Prelude.writeFile
          (home ++ "/.config/zettel/zettel-conf")
          "neo4j neo4j"
        return ["neo4j", "neo4j"]
  pipe <- DB.connect (def {DB.user = user, DB.password = pass})
  r <- runMain pipe
  case r of
    Left e -> error (show e)
    Right x -> return x
