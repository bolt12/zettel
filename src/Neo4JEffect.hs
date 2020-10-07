{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Neo4JEffect where

import Control.Exception (SomeException)
import           Control.Monad
import Data.Char (toLower)
import Data.Default
import Data.Map hiding (map)
import Data.Text (Text, pack, append)
import qualified Database.Bolt as B
import Polysemy
import Polysemy.Error
import Types

type User = Text

type Password = Text

type Tags = String

data Neo4J m a where
  CreateNode :: Zettel -> Neo4J m ()
  CreateRelation :: ZettelID -> ZettelID -> Text -> Neo4J m ()
  GetNode :: ZettelID -> Neo4J m (Maybe Zettel)
  ListNodes :: Int -> Neo4J m [Zettel]
  FindNodes :: [Tags] -> Neo4J m [Zettel]
  DeleteNode :: ZettelID -> Neo4J m ()
  EditNode :: Zettel -> Neo4J m ()

makeSem ''Neo4J

newtype DependenciesFoundError = DependenciesFoundError String
  deriving (Show)

newtype NodeNotFoundError = NodeNotFoundError String
  deriving (Show)

newtype ConnectionError = ConnectionError String
  deriving (Show)

errorMsg :: String
errorMsg = "Failed to connect to DB.\n"
           ++ "Please check your credentials and make sure Neo4J is running."

neo4jToIO ::
  User ->
  Password ->
  Members '[Error DependenciesFoundError, Error ConnectionError, Embed IO] r =>
  Sem (Neo4J ': r) a ->
  Sem r a
neo4jToIO user pass =
  interpret
    ( \case
        CreateNode zettel -> do
          pipe <-
            fromExceptionVia @SomeException
              (const $ ConnectionError errorMsg)
              (B.connect (def {B.user = user, B.password = pass}))
          B.run pipe $
            B.queryP_
              "CREATE (node:Zettel { timestamp: {ts}, title: {t}, authors: {a}, zettel: {z}, tags: {tg} })"
              ( fromList
                  [ ("ts", B.T $ getTimestamp zettel),
                    ("t", B.T $ getTitle zettel),
                    ("a", B.L . Prelude.map B.T . getAuthors $ zettel),
                    ("z", B.T $ getZettel zettel),
                    ("tg", B.L . Prelude.map B.T . getTags $ zettel)
                  ]
              )
          if Prelude.null (getConnections zettel)
            then B.close pipe
            else do
              r <-
                B.run pipe $
                  B.queryP
                    "MATCH (z:Zettel) WHERE z.timestamp = {ts} return z"
                    ( fromList
                        [("ts", B.T $ getTimestamp zettel)]
                    )
              B.close pipe
              let newZettelId = getId . toZettel . (! "z") . Prelude.head $ r
              mapM_
                (\c -> neo4jToIO user pass $ createRelation newZettelId (getCID c) (getDesc c))
                (getConnections zettel)
        CreateRelation (ZID id1) (ZID id2) t -> do
          pipe <-
            fromExceptionVia @SomeException
              (const $ ConnectionError errorMsg)
              (B.connect (def {B.user = user, B.password = pass}))
          x <- B.run pipe $
            B.queryP_
              ( "MATCH (z1:Zettel) WHERE ID(z1)={id1}\n"
                  `append` "MATCH (z2:Zettel) WHERE ID(z2)={id2}\n"
                  `append` "CREATE (z1)-[:RELATES { reason: {desc} }]->(z2)"
              )
              ( fromList
                  [ ("id1", B.I id1),
                    ("id2", B.I id2),
                    ("desc", B.T t)
                  ]
              )
          B.close pipe
          return x
        GetNode (ZID i) -> do
          pipe <-
            fromExceptionVia @SomeException
              (const $ ConnectionError errorMsg)
              (B.connect (def {B.user = user, B.password = pass}))
          r <-
            B.run pipe $
              B.queryP
                "MATCH (z:Zettel) WHERE ID(z)={id} return z"
                (fromList [("id", B.I i)])
          B.close pipe
          if Prelude.null r
            then return Nothing
            else return . Just . toZettel . (! "z") . Prelude.head $ r
        ListNodes s -> do
          pipe <-
            fromExceptionVia @SomeException
              (const $ ConnectionError errorMsg)
              (B.connect (def {B.user = user, B.password = pass}))
          r <-
            B.run pipe $
              B.queryP
                "MATCH (z:Zettel) RETURN z LIMIT {size}"
                (fromList [("size", B.I s)])
          B.close pipe
          return . Prelude.map (toZettel . (! "z")) $ r
        FindNodes tags -> do
          pipe <-
            fromExceptionVia @SomeException
              (const $ ConnectionError errorMsg)
              (B.connect (def {B.user = user, B.password = pass}))
          r <-
            B.run pipe $
              B.queryP
                "MATCH (z:Zettel) WHERE size([tag IN {tags} WHERE tag IN [x IN z.tags | toLower(x)] | 1]) > 0 RETURN z"
                (fromList [("tags", B.L . Prelude.map (B.T . pack . map toLower) $ tags)])
          B.close pipe
          return . Prelude.map (toZettel . (! "z")) $ r
        DeleteNode (ZID zid) -> do
          pipe <-
            fromExceptionVia @SomeException
              (const $ ConnectionError errorMsg)
              (B.connect (def {B.user = user, B.password = pass}))
          r <-
            B.run pipe $
              B.queryP
                ( "MATCH (z:Zettel) WHERE ID(z)={zid}\n"
                    `append` "MATCH p=()-->(z) RETURN p"
                )
                (fromList [("zid", B.I zid)])
          if not (Prelude.null r)
            then B.close pipe >> throw (DependenciesFoundError "There are nodes which relate to this Zettel")
            else do
              -- Delete connections
              B.run pipe $
                B.queryP_
                  ( "MATCH (z:Zettel) WHERE ID(z)={zid}\n"
                      `append` "MATCH p=(z)-->() DELETE p"
                  )
                  (fromList [("zid", B.I zid)])
              -- Delete node
              B.run pipe $
                B.queryP_
                  "MATCH (z:Zettel) WHERE ID(z)={zid} DELETE z"
                  (fromList [("zid", B.I zid)])
              B.close pipe
        EditNode z -> do
          neo4jToIO user pass . deleteNode $ getId z
          neo4jToIO user pass (createNode z)
    )

toZettel :: B.Value -> Zettel
toZettel (B.S l) =
  let zettel = Zettel {}
   in aux (B.fields l) zettel
  where
    aux [] z = z
    aux (B.I i : t) z = aux t (z {getId = ZID i})
    aux (B.M m : t) z =
      let (B.T title) = m ! "title"
          (B.T timestamp) = m ! "timestamp"
          (B.L authors) = m ! "authors"
          (B.L tags) = m ! "tags"
          (B.T zettel) = m ! "zettel"
       in aux
            t
            ( z
                { getAuthors = Prelude.map unT authors,
                  getTags = Prelude.map unT tags,
                  getTimestamp = timestamp,
                  getTitle = title,
                  getZettel = zettel,
                  getConnections = []
                }
            )
    aux (_ : t) z = aux t z
    unT (B.T a) = a
toZettel _ = Zettel {}
