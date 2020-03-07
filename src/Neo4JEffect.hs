{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Neo4JEffect where

import Data.Default
import Data.Map
import Data.Text
import qualified Database.Bolt as B
import Polysemy
import Polysemy.Reader
import Types

type User = Text

type Password = Text

data Neo4J m a where
  Connect :: User -> Password -> Neo4J m B.Pipe
  CreateNode :: Zettel -> Neo4J m ()
  CreateRelation :: ZettelID -> ZettelID -> Text -> Neo4J m ()
  GetNode :: ZettelID -> Neo4J m Zettel

makeSem ''Neo4J

neo4jToIO :: Members '[Neo4J ,Reader B.Pipe, Embed IO] r => Sem (Neo4J ': r) a -> Sem r a
neo4jToIO = interpret $ \case
  Connect user password -> B.connect (def {B.user = user, B.password = password})
  CreateNode zettel -> do
    pipe <- ask
    B.run pipe $
      B.queryP_
        "CREATE (node:Zettel { timestamp: {ts}, title: {t}, authors: {a}, zettel: {z}, tags: {tg} })"
        ( fromList
            [ ("ts", B.T $ getTimestamp zettel ),
              ("t", B.T $ getTitle zettel),
              ("a", B.T . pack . show . getAuthors $ zettel),
              ("z", B.T $ getZettel zettel),
              ("tg", B.T . pack . show . getTags $ zettel)
            ]
        )
    if Prelude.null (getConections zettel)
       then return ()
       else
        mapM_ (\c -> createRelation (getId zettel) (getCID c) (getDesc c))
              (getConections zettel)
  CreateRelation (ZID id1) (ZID id2) t -> do
    pipe <- ask
    B.run pipe $
      B.queryP_
        ( "MATCH (z1:Zettel) WHERE ID(z1)  WHERE ID(z1)={id1}\n" `append`
          "MATCH (z2:Zettel) WHERE ID(z2)  WHERE ID(z2)={id2}\n" `append`
          "CREATE (z1)-[:RELATES { reason: {desc} }]->(z2)"
        )
        ( fromList
            [ ("id1", B.T . pack $ show id1),
              ("id2", B.T . pack $ show id2),
              ("desc", B.T t)
            ]
        )
  GetNode (ZID i) -> do
    pipe <- ask
    r <- B.run pipe $ 
      B.queryP
        "MATCH (z:Zettel) WHERE ID(z)={id}"
        (fromList [ ("id", B.T . pack $ show i) ])
    return . toZettel . (! "z") . Prelude.head $ r

toZettel :: B.Value -> Zettel
toZettel (B.S l) =
  let zettel = Zettel {}
   in aux (B.fields l) zettel
  where
    aux [] z         = z
    aux (B.I i:t) z = aux t (z {getId = ZID i})
    aux (B.M m:t) z  =
      let (B.T title)     = m ! "title"
          (B.T timestamp) = m ! "timestamp"
          (B.T author)    = m ! "author"
          (B.T zettel)    = m ! "zettel"
       in aux t (z { getAuthors = read . unpack $ author,
                     getTimestamp = timestamp,
                     getTitle = title,
                     getZettel = zettel,
                     getConections = []
                   })
    aux (_:t) z = aux t z
toZettel _ = Zettel {}
