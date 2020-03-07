{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.Pandoc
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.HashMap.Strict
import Data.Aeson.Types
import Data.Aeson
import Data.Scientific
import Data.Maybe (fromJust)
import Types

readZettel :: PandocMonad m => FilePath -> m Pandoc
readZettel f = do
  content <- E.decodeUtf8 <$> readFileLazy f
  let extension = enableExtension Ext_yaml_metadata_block emptyExtensions
  readMarkdown (def {readerExtensions = extension }) (TL.toStrict content)

-- Right (
--  Pandoc (
--    Meta {
--      unMeta =
--        fromList
--          [("author",MetaList [MetaInlines [Str "Armando"]]),
--           ("tags",MetaList [MetaInlines [Str "test"]]),
--           ("title",MetaInlines [Str "Test",Space,Str "Zettel"])]
--         }) 
--         [Header 2 ("",[],[]) [Str "How",Space,Str "to",Space,Str "write",Space,Str "a",Space,Str "Zettel"]])

createZettel :: PandocMonad m => Pandoc -> m Zettel
createZettel p@(Pandoc m _) = do
  title <- writePlain def (Pandoc nullMeta [Plain (docTitle m)])
  author <- writePlain def (Pandoc nullMeta [Plain (concat $ docAuthors m)])
  tags <- writePlain def (Pandoc nullMeta [Plain (concat $ docTags m)])
  zettel <- writePlain def p
  let res = lookupMeta "connections" m
  connections <- case res of
        Nothing -> return []
        (Just (MetaBlocks r)) -> do
          str <- writePlain def (Pandoc nullMeta r)
          return $ toConnections (fromJust . decode $ E.encodeUtf8 (TL.fromStrict str))
  return (Zettel {
    getId = ZID 0,
    getTimestamp = "07/03/2020",
    getTitle = title,
    getAuthors = T.splitOn "," author,
    getZettel = zettel,
    getTags = T.splitOn "," tags,
    getConections = connections
                 })

toConnections :: Value -> [Connection]
toConnections (Array l) = V.toList $ V.map toConnection l

toConnection :: Value -> Connection
toConnection (Object m) =
  let i = fromJust . toBoundedInteger . fromNumber $ m ! "id"
      reason = fromString $ m ! "reason"
   in Connection {
    getCID = ZID i,
    getDesc = reason
                 }
  where
    fromNumber (Number n) = n
    fromString (String s) = s

docTags :: Meta -> [[Inline]]
docTags meta =
  case lookupMeta "tags" meta of
        Just (MetaString s)    -> [[Str s]]
        Just (MetaInlines ils) -> [ils]
        Just (MetaList   ms)   -> [ils | MetaInlines ils <- ms] ++
                                  [ils | MetaBlocks [Plain ils] <- ms] ++
                                  [ils | MetaBlocks [Para ils]  <- ms] ++
                                  [[Str x] | MetaString x <- ms]
        _                      -> []

main :: IO ()
main = putStrLn ("Hello, world!" :: String)
