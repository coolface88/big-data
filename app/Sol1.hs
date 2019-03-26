{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Text (Text)
import Data.Conduit
import Data.Conduit.Binary
import Data.CSV.Conduit
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Text.CSV.Lazy.ByteString
import System.Environment (getArgs)
import System.Directory
import System.FilePath
import Data.Int
import Control.Monad as CM
import Control.Monad.State as ST
import Conduit
import qualified Data.Text as T

listFilesInDirectory :: FilePath -> IO [FilePath]
listFilesInDirectory dir = do
    rawList <- listDirectory dir 
    let csvList = [x | x <- rawList, isExtensionOf ".csv" x]  
    filterM doesFileExist (Prelude.map (dir </>) csvList)

myCSVTransformer :: Monad m => ConduitT (Row Text) (Row Text) m ()
myCSVTransformer = awaitForever $ yield

filterAsPhoneNum fls pn = runConduitRes
                        $ (yieldMany $ fls)
                       .| awaitForever sourceFile
                       .| intoCSV defCSVSettings
                       .| myCSVTransformer
                       .| filterC (\x -> Prelude.head x == pn) 
                       .| fromCSV defCSVSettings
                       .| sinkFile "test/all-file.csv" 

main = do
  [arg,arg2] <- getArgs
  (file:fls) <- listFilesInDirectory arg
  filterAsPhoneNum (file:fls) (T.pack arg2)

