{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.Pandoc
import qualified Data.Text.Encoding as E

readZettel :: PandocMonad m => FilePath -> m Pandoc
readZettel f = do
  content <- E.decodeUtf8 <$> readFileStrict f
  let extension = enableExtension Ext_yaml_metadata_block emptyExtensions
  readMarkdown (def {readerExtensions = extension }) content

main :: IO ()
main = putStrLn ("Hello, world!" :: String)
