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
import qualified OpenTheory.Unicode as Unicode
import qualified OpenTheory.Unicode.UTF8 as UTF8

demoLength :: Int
demoLength = 7621

testLength :: Int
testLength = 79

getTestFiles :: FilePath -> IO [FilePath]
getTestFiles d =
    do fs <- Directory.getDirectoryContents d
       let fs' = filter (List.isPrefixOf "test") fs
       return (map (\f -> d ++ "/" ++ f) fs')

readTestCharFile :: Maybe Int -> FilePath -> IO ()
readTestCharFile x f =
    do l <- decodeFile f
       let a = all Either.isRight l
       case x of
         Nothing ->
             if a
               then error $ "invalid file " ++ f ++ " was successfully decoded"
               else return ()
         Just n ->
             if not a
               then error $ "valid file " ++ f ++ " could not be decoded"
               else
                 if length l == n
                   then return ()
                   else
                     error $ "valid file " ++ f ++ " has bad line length: " ++
                             show (length l)

readValidCharFile :: Int -> FilePath -> IO ()
readValidCharFile n = readTestCharFile (Just n)

readInvalidCharFile :: FilePath -> IO ()
readInvalidCharFile = readTestCharFile Nothing

partitionTestCharFile :: IO ()
partitionTestCharFile =
    do skip <- Directory.doesFileExist "test/valid/test061.txt"
       if skip
         then return ()
         else
           do putStrLn "\nsplitting UTF8 test file into valid and invalid lines"
              cs <- decodeFile "test/test.txt"
              mapM_ outputLine (filter testLine (readLines 1 cs))
  where
    isNewline :: Either Word.Word8 Unicode.Unicode -> Bool
    isNewline (Left _) = False
    isNewline (Right c) = let n = Unicode.unUnicode c in n == 10 || n == 13

    chopNewline :: [Either Word.Word8 Unicode.Unicode] ->
                   [Either Word.Word8 Unicode.Unicode]
    chopNewline [] = []
    chopNewline (_ : cs) = cs

    readLines :: Int -> [Either Word.Word8 Unicode.Unicode] ->
                 [(Int,[Either Word.Word8 Unicode.Unicode])]
    readLines _ [] = []
    readLines lineno inp =
        let (line,inp') = List.span (not . isNewline) inp in
        let inp'' = chopNewline inp' in
        (lineno,line) : readLines (lineno + 1) inp''

    testLine :: (Int,[Either Word.Word8 Unicode.Unicode]) -> Bool
    testLine (lineno,line) = not (lineno < 61 || null line)

    outputLine :: (Int,[Either Word.Word8 Unicode.Unicode]) -> IO ()
    outputLine (lineno,line) =
        let valid = all Either.isRight line in
        let n = show lineno in
        let f = "test/" ++ (if valid then "valid" else "invalid") ++
                "/test" ++ replicate (3 - length n) '0' ++ n ++ ".txt" in
        reencodeFile f line

main :: IO ()
main =
    do partitionTestCharFile
       readValidCharFile demoLength "test/demo.txt"
       validTestFiles <- getTestFiles "test/valid"
       invalidTestFiles <- getTestFiles "test/invalid"
       mapM_ (readValidCharFile testLength) validTestFiles
       mapM_ readInvalidCharFile invalidTestFiles
       putStrLn "  all tests pass"
       return ()
