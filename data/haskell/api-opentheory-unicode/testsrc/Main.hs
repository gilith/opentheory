{- |
module: Main
description: Testing the Unicode character library and API
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List as List
import qualified Data.Either as Either
import qualified Data.Word as Word
import qualified System.Directory as Directory

import Unicode
import qualified OpenTheory.Unicode.UTF8 as UTF8

readTestCharFile :: Bool -> FilePath -> IO ()
readTestCharFile x f =
    do l <- decodeFile ("test/" ++ f ++ ".txt")
       let a = all Either.isRight l
       if a == x
         then return ()
         else
           if a
             then error $ "invalid file " ++ f ++ " was successfully decoded"
             else error $ "valid file " ++ f ++ " could not be decoded"

readValidCharFile :: FilePath -> IO ()
readValidCharFile = readTestCharFile True

readInvalidCharFile :: FilePath -> IO ()
readInvalidCharFile = readTestCharFile False

partitionTestCharFile :: IO ()
partitionTestCharFile =
    do skip <- Directory.doesFileExist "test/valid/test0.txt"
       if skip
         then return ()
         else
           do putStrLn "\n(splitting UTF8 test file into valid and invalid lines)"
              bytestr <- ByteString.readFile "test/test.txt"
              let bs = ByteString.unpack bytestr
              let ls = List.drop 51 (readLines [] bs)
              outputLine 0 0 ls
  where
    readLines :: [[Word.Word8]] -> [Word.Word8] -> [[Word.Word8]]
    readLines acc [] = reverse acc
    readLines acc inp =
      let (line,inp') = List.span (not . isNewline) inp in
      let inp'' = List.dropWhile isNewline inp' in
      let acc' = if null line then acc else line : acc in
      readLines acc' inp''

    isNewline :: Word.Word8 -> Bool
    isNewline b = b == 10 || b == 13

    outputLine :: Int -> Int -> [[Word.Word8]] -> IO ()
    outputLine _ _ [] = return ()
    outputLine ex err (line : rest) =
       let res = UTF8.decode line in
       let cs = Either.rights res in
       let (f,ex',err') =
             if length res == length cs
               then
                 if length cs == 79
                   then ("valid/test" ++ show ex, ex + 1, err)
                   else error $ "bad line length: " ++ show (length cs)
               else ("invalid/test" ++ show err, ex, err + 1) in
       let bs = ByteString.pack line in
       do ByteString.writeFile ("test/" ++ f ++ ".txt") bs
          outputLine ex' err' rest

main :: IO ()
main =
    do partitionTestCharFile
       readValidCharFile "demo"
       mapM_ (\i -> readValidCharFile $ "valid/test" ++ show i)
         ([0..139] :: [Int])
       mapM_ (\i -> readInvalidCharFile $ "invalid/test" ++ show i)
         ([0..70] :: [Int])
       putStrLn "ok"
       return ()
