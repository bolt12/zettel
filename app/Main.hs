{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Polysemy
import Polysemy.Reader
import Polysemy.Error
import qualified Database.Bolt as DB
import Neo4JEffect
import PandocParse
import Text.Pandoc
import Data.Either (fromRight)

prog :: Members '[Error PandocError, Neo4J, Embed IO] r => Sem r ()
prog = do
  p <- embed . runIO $ readZettel "zettel.md"
  case p of
    Left e -> throw e
    Right pandoc -> do
      zettel <- embed . runIO $ createZettel pandoc
      case zettel of
        Left e -> throw e
        Right z -> createNode z

runProg :: DB.Pipe -> IO (Either PandocError ())
runProg pipe =
  runM
  . runReader pipe
  . runError
  . neo4jToIO
  $ prog

main :: IO ()
main = do
  pipe <- DB.connect (def { DB.user = "neo4j", DB.password = "bolt"})
  fromRight () <$> runProg pipe
