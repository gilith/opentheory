{- |
module: Unicode
description: Unicode character API
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Unicode
  ( newline,
    decode,
    encode,
    reencode,
    hDecode,
    hEncode,
    hReencode,
    decodeFile,
    encodeFile,
    reencodeFile )
where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Char as Char
import qualified Data.Word as Word
import qualified System.IO as IO

import qualified OpenTheory.Unicode.UTF8 as UTF8
import OpenTheory.Unicode

instance Show Unicode where
  show = show . Char.chr . fromIntegral . unUnicode

newline :: [Unicode]
newline = [Unicode 10]

decode :: IO [Either Word.Word8 Unicode]
decode =
    do b <- ByteString.getContents
       return (UTF8.decode (ByteString.unpack b))

encode :: [Unicode] -> IO ()
encode c =
    let b = ByteString.pack (UTF8.encode c) in
    ByteString.putStr b

reencode :: [Either Word.Word8 Unicode] -> IO ()
reencode c =
    let b = ByteString.pack (UTF8.reencode c) in
    ByteString.putStr b

hDecode :: IO.Handle -> IO [Either Word.Word8 Unicode]
hDecode h =
    do b <- ByteString.hGetContents h
       return (UTF8.decode (ByteString.unpack b))

hEncode :: IO.Handle -> [Unicode] -> IO ()
hEncode h c =
    let b = ByteString.pack (UTF8.encode c) in
    ByteString.hPut h b

hReencode :: IO.Handle -> [Either Word.Word8 Unicode] -> IO ()
hReencode h c =
    let b = ByteString.pack (UTF8.reencode c) in
    ByteString.hPut h b

decodeFile :: FilePath -> IO [Either Word.Word8 Unicode]
decodeFile f =
    do b <- ByteString.readFile f
       return (UTF8.decode (ByteString.unpack b))

encodeFile :: FilePath -> [Unicode] -> IO ()
encodeFile f c =
    let b = ByteString.pack (UTF8.encode c) in
    ByteString.writeFile f b

reencodeFile :: FilePath -> [Either Word.Word8 Unicode] -> IO ()
reencodeFile f c =
    let b = ByteString.pack (UTF8.reencode c) in
    ByteString.writeFile f b
