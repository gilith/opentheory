{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT

License-file: LICENSE
Maintainer: Joe Hurd
-}
module Main
  ( main )
where

import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Word
import qualified Debug.Trace
import qualified OpenTheory.Char
import qualified OpenTheory.Test

prop1 :: [OpenTheory.Char.Unicode] -> Bool
prop1 cs =
    case OpenTheory.Char.decode (OpenTheory.Char.encode cs) of
      Just cs' -> cs == cs'
      Nothing -> False

{- This property is violated by "overlong sequences" -}
{- We reject overlong sequences, since they're a source of covert channels -}
prop2 :: [Data.Word.Word8] -> Bool
prop2 bs =
    case OpenTheory.Char.decode bs of
      Just cs -> OpenTheory.Char.encode cs == bs
      Nothing -> True

prop3 :: [OpenTheory.Char.Unicode] -> Bool
prop3 cs = length cs <= length (OpenTheory.Char.encode cs)

readTestFile :: Bool -> FilePath -> IO ()
readTestFile x f =
    do b <- Data.ByteString.Lazy.readFile ("test/" ++ f ++ ".txt")
       let bs = Data.ByteString.Lazy.unpack b
       let a = Data.Maybe.isJust (OpenTheory.Char.decode bs)
       if a == x
         then return ()
         else
           if a
             then error $ "invalid file " ++ f ++ " was successfully decoded"
             else error $ "valid file " ++ f ++ " could not be decoded"

readExampleFile :: FilePath -> IO ()
readExampleFile = readTestFile True

readErrorFile :: FilePath -> IO ()
readErrorFile = readTestFile False

splitTestFile :: IO ()
splitTestFile =
    do bytestr <- Data.ByteString.Lazy.readFile "test/test.txt"
       let bs = Data.ByteString.Lazy.unpack bytestr
       let ls = Data.List.drop 51 (readLines [] bs)
       outputLine 0 0 ls
  where
    readLines :: [[Data.Word.Word8]] -> [Data.Word.Word8] -> [[Data.Word.Word8]]
    readLines acc [] = reverse acc
    readLines acc inp =
      let (line,inp') = Data.List.span (not . isNewline) inp in
      let inp'' = Data.List.dropWhile isNewline inp' in
      let acc' = if null line then acc else line : acc in
      readLines acc' inp''

    isNewline :: Data.Word.Word8 -> Bool
    isNewline b = b == 10 || b == 13

    outputLine :: Int -> Int -> [[Data.Word.Word8]] -> IO ()
    outputLine _ _ [] = return ()
    outputLine ex err (line : lines) =
       let res = OpenTheory.Char.decode line in
       let (f,ex',err') =
               case res of
                 Just cs ->
                     if length cs == 79
                       then ("example-" ++ show ex, ex + 1, err)
                       else error $ "bad line length: " ++ show (length cs)
                 Nothing -> ("error-" ++ show err, ex, err + 1) in
       let bs = Data.ByteString.Lazy.pack line in
       do Data.ByteString.Lazy.writeFile ("test/" ++ f ++ ".txt") bs
          outputLine ex' err' lines

main :: IO ()
main =
    do OpenTheory.Test.check "print then parse" prop1
       OpenTheory.Test.check "parse then print" prop2
       OpenTheory.Test.check "printing grows length" prop3
       readExampleFile "demo"
       --splitTestFile
       mapM_ (\i -> readExampleFile $ "example-" ++ show i) [0..139]
       mapM_ (\i -> readErrorFile $ "error-" ++ show i) [0..70]
       return ()
