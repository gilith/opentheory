{- |
module: Chess
description: Converting a chess position from FEN notation to text
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Chess
where

import qualified Data.Char as Char
import qualified Data.List as List

import OpenTheory.Unicode
import qualified Unicode

data Side =
    Black
  | White
  deriving (Eq,Ord,Show)

data Piece =
    King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  deriving (Eq,Ord,Show)

data Edge =
    NoEdge
  | SingleEdge
  | DoubleEdge
  deriving (Eq,Ord,Show)

newtype Board =
    Board { unBoard :: [[Maybe (Side,Piece)]] }
  deriving (Eq,Ord,Show)

emptySquareUnicode :: Unicode
emptySquareUnicode = Unicode 8729

fenToSide :: Char -> Side
fenToSide c = if Char.isLower c then Black else White

fenToPiece :: Char -> Piece
fenToPiece c =
    case Char.toLower c of
      'k' -> King
      'q' -> Queen
      'r' -> Rook
      'b' -> Bishop
      'n' -> Knight
      'p' -> Pawn
      _ -> error $ "bad FEN character: " ++ show c

fenToSidePiece :: Char -> (Side,Piece)
fenToSidePiece c = (fenToSide c, fenToPiece c)

fenToBoard :: String -> Board
fenToBoard =
    Board . uncurry (:) . foldr parse ([],[])
  where
    parse '/' (r,rs) = ([], r : rs)
    parse c (r,rs) =
        if Char.isDigit c
        then (replicate (Char.digitToInt c) Nothing ++ r, rs)
        else (Just (fenToSidePiece c) : r, rs)

-- http://en.wikipedia.org/wiki/Chess_symbols_in_Unicode
sidePieceToUnicode :: (Side,Piece) -> Unicode
sidePieceToUnicode p =
    Unicode codepoint
  where
    codepoint =
        case p of
          (White,King) -> 9812
          (White,Queen) -> 9813
          (White,Rook) -> 9814
          (White,Bishop) -> 9815
          (White,Knight) -> 9816
          (White,Pawn) -> 9817
          (Black,King) -> 9818
          (Black,Queen) -> 9819
          (Black,Rook) -> 9820
          (Black,Bishop) -> 9821
          (Black,Knight) -> 9822
          (Black,Pawn) -> 9823

squareToUnicode :: Maybe (Side,Piece) -> Unicode
squareToUnicode Nothing = emptySquareUnicode
squareToUnicode (Just p) = sidePieceToUnicode p

rankToUnicode :: [Maybe (Side,Piece)] -> [Unicode]
rankToUnicode = map squareToUnicode

boardToUnicode :: Edge -> Board -> [Unicode]
boardToUnicode e b =
    top ++ List.intercalate Unicode.newline (map rank (unBoard b)) ++ bottom
  where
    rank l = side ++ rankToUnicode l ++ side

    top = case e of
            NoEdge -> []
            SingleEdge -> singleTop
            DoubleEdge -> doubleTop

    side = case e of
             NoEdge -> []
             SingleEdge -> singleSide
             DoubleEdge -> doubleSide

    bottom = case e of
            NoEdge -> []
            SingleEdge -> singleBottom
            DoubleEdge -> doubleBottom

    singleTop = topEdge 9484 9472 9488
    singleSide = sideEdge 9474
    singleBottom = bottomEdge 9492 9472 9496

    doubleTop = topEdge 9556 9552 9559
    doubleSide = sideEdge 9553
    doubleBottom = bottomEdge 9562 9552 9565

    topEdge x y z = longEdge x y z ++ Unicode.newline
    sideEdge x = [Unicode x]
    bottomEdge x y z = Unicode.newline ++ longEdge x y z
    longEdge x y z = map Unicode ([x] ++ replicate 8 y ++ [z])

fenToUnicode :: String -> Edge -> [Unicode]
fenToUnicode f e = boardToUnicode e (fenToBoard f)
