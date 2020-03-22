{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Neo4JEffect where

import Data.Map
import Data.Text
import qualified Database.Bolt as B
import Polysemy
import Polysemy.Input
import Polysemy.Error
import Types

type User = Text

type Password = Text

data Neo4J m a where
  CreateNode :: Zettel -> Neo4J m ()
  CreateRelation :: ZettelID -> ZettelID -> Text -> Neo4J m ()
  GetNode :: ZettelID -> Neo4J m Zettel
  ListNodes :: Int -> Neo4J m [Zettel]
  FindNodes :: [String] -> Neo4J m [Zettel]
  Delete :: ZettelID -> Neo4J m ()

makeSem ''Neo4J

newtype DependenciesFoundError = DependenciesFoundError String
  deriving (Show)

neo4jToIO :: Members '[Input B.Pipe, Error DependenciesFoundError, Embed IO] r => Sem (Neo4J ': r) a -> Sem r a
neo4jToIO = interpret $ \case
  CreateNode zettel -> do
    pipe <- input
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
      then return ()
      else do
        r <-
          B.run pipe $
            B.queryP
              "MATCH (z:Zettel) WHERE z.timestamp = {ts} return z"
              ( fromList
                  [("ts", B.T $ getTimestamp zettel)]
              )
        let newZettelId = getId . toZettel . (! "z") . Prelude.head $ r
        mapM_
          (\c -> neo4jToIO $ createRelation newZettelId (getCID c) (getDesc c))
          (getConnections zettel)
  CreateRelation (ZID id1) (ZID id2) t -> do
    pipe <- input
    B.run pipe $
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
  GetNode (ZID i) -> do
    pipe <- input
    r <-
      B.run pipe $
        B.queryP
          "MATCH (z:Zettel) WHERE ID(z)={id}"
          (fromList [("id", B.I i)])
    return . toZettel . (! "z") . Prelude.head $ r
  ListNodes s -> do
    pipe <- input
    r <-
      B.run pipe $
        B.queryP
          "MATCH (z:Zettel) RETURN z LIMIT {size}"
          (fromList [("size", B.I s)])
    return . Prelude.map (toZettel . (! "z")) $ r
  FindNodes tags -> do
    pipe <- input
    r <-
      B.run pipe $
        B.queryP
          "MATCH (z:Zettel) WHERE size([tag IN {tags} WHERE tag IN z.tags | 1]) > 0 RETURN z"
          (fromList [("tags", B.L . Prelude.map (B.T . pack) $ tags)])
    return . Prelude.map (toZettel . (! "z")) $ r
  Delete (ZID zid) -> do
    pipe <- input
    r <-
      B.run pipe $
        B.queryP
          ( "MATCH (z:Zettel) WHERE ID(z)={zid}\n"
            `append` "MATCH p=()-->(z) RETURN p"
          )
          (fromList [("zid", B.I zid)])
    if not (Prelude.null r)
       then throw (DependenciesFoundError "There are nodes which relate to this Zettel")
       else
         B.run pipe $
           B.queryP_
             ( "MATCH (z:Zettel) WHERE ID(z)={zid}\n"
               `append` "MATCH p=(z)-->() DELETE p, z"
             )
             (fromList [("zid", B.I zid)])

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
