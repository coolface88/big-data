{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Main where


import Data.Conduit
import Data.Conduit.Binary
import Data.CSV.Conduit
import Data.Text (Text) 
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Text.CSV.Lazy.ByteString
import System.Environment (getArgs)
import System.Directory
import System.FilePath
import Control.Applicative
import Data.List (foldl', delete)
import Data.Int
import Control.Monad as CM
import Data.ByteString.Internal as Internal
import Control.Monad.State as ST
import Conduit
import System.FilePath (takeExtension)
import qualified Data.Text as T
import Data.Conduit.List as CL
 

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
  h <- filterAsPhoneNum (file:fls) (T.pack arg2)

  let loop2 k [] = return k
      loop2 k (b:bs) = do
        (line:lns) <- fmap BS.lines $ BS.readFile b
        let row1 = csvTable (parseCSV line)
            !phoneList = [ Prelude.map csvFieldContent x | let x = (Prelude.head row1)]
            !phoneN = Prelude.head (Prelude.head phoneList)
        let loop []  []  _ = []
            loop []  (l:ls) _  = (l:ls)  
            loop (b:c:bs) [] (n:ns) = do
              row1 <- csvTable (parseCSV b)
              let !phoneList = [ Prelude.map csvFieldContent x | let x = row1]
              let !phoneNum = Prelude.head ( Prelude.head phoneList)
              let !pnS = ST.runState get phoneNum
              let (pnVal,_) = pnS 
              row2 <- csvTable (parseCSV n)
              let !phoneL = [ Prelude.map csvFieldContent x | let x = row2]
              let !phoneN = Prelude.head ( Prelude.head phoneList)  
              let !activated  = if length (BS.unpack ( Prelude.last(  Prelude.last phoneList))) == 0
                              then do checkPhoneState (c:bs) phoneN False (n:[])
                              else do checkPhoneState (c:bs) phoneN True (n:[])
              loop bs activated ns
            loop (b:c:bs) (l:ls)  (n:ns) = do
              row1 <- csvTable (parseCSV b)
              let !phoneList = [ Prelude.map csvFieldContent x | let x = row1]
              let !phoneNum = Prelude.head ( Prelude.head phoneList)
              let !pnS = ST.runState get phoneNum
              let (pnVal,_) = pnS 
              row2 <- csvTable (parseCSV n)
              let !phoneL = [ Prelude.map csvFieldContent x | let x = row2]
              let !phoneN = Prelude.head ( Prelude.head phoneList) 
              let !activated = if length (BS.unpack ( Prelude.last ( Prelude.last phoneList))) == 0
                                 then do (l:ls) ++ checkPhoneState (c:bs) phoneN False (n:[])
                                 else do (l:ls) ++ checkPhoneState (c:bs)  phoneN True (n:[])
              loop bs activated ns
            loop (b:bs) (l:ls)  (n:ns) = do
              row1 <- csvTable (parseCSV b)
              let !phoneList = [ Prelude.map csvFieldContent x | let x = row1]
              let !phoneNum = Prelude.head ( Prelude.head phoneList)
              let !pnS = ST.runState get phoneNum
              let (pnVal,_) = pnS
              row2 <- csvTable (parseCSV n)
              let !phoneL = [ Prelude.map csvFieldContent x | let x = row2]
              let !phoneN = Prelude.head ( Prelude.head phoneList)
              let !activated = if length (BS.unpack ( Prelude.last ( Prelude.last phoneList))) == 0
                                 then do (l:ls) ++ checkPhoneState bs phoneN False (n:[])
                                 else do (l:ls) ++ checkPhoneState bs  phoneN True (n:[])
              loop bs activated ns
        let w = loop (line:lns) [] (line:lns)
        CM.mapM_ print w
        print phoneList
        loop2 (k+1) bs
        
  k <- loop2 0 (file:fls) 
  putStrLn $ "k = " ++ show (k :: Int)
  print (file:fls)

checkPhoneState :: [BS.ByteString] -> BS.ByteString -> Bool -> [BS.ByteString] -> [BS.ByteString]
checkPhoneState [] _  _ [] = [] 
checkPhoneState (l:ls) b i (z:zs) = do 
                             row <- csvTable (parseCSV l)
                             let !phoneList = [ Prelude.map csvFieldContent x | let x = row]
                             let !phoneNum = Prelude.head $ Prelude.head phoneList 
                             if  phoneNum == b && length (BS.unpack ( Prelude.last ( Prelude.last phoneList))) /= 0
                                       then do checkPhoneState ls b True (z:zs)
                                       else if phoneNum == b && i == True && length (BS.unpack ( Prelude.last ( Prelude.last phoneList))) == 0
                                            then do checkPhoneState ls b True (z:zs) 
                                            else if phoneNum == b && i == False && length (BS.unpack ( Prelude.last ( Prelude.last phoneList))) == 0
                                                    then do checkPhoneState ls  b False (z:zs) 
                                                    else do checkPhoneState ls b i (z:zs) 
checkPhoneState [] b i (z:zs) | i == True = []
                              | i == False = (z:zs)
