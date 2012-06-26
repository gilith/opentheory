(* ========================================================================= *)
(* CHECKSUMS                                                                 *)
(* Copyright (c) 2010 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure Checksum :> Checksum =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Types of theory package syntax.                                           *)
(* ------------------------------------------------------------------------- *)

datatype checksum = Checksum of string;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (Checksum s1, Checksum s2) = String.compare (s1,s2);

fun equal (Checksum s1) (Checksum s2) = s1 = s2;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun toString (Checksum s) = s;

val pp = Print.ppMap toString Print.ppString;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  fun isChecksumChar c =
      Char.isDigit c orelse
      c = #"a" orelse c = #"b" orelse c = #"c" orelse
      c = #"d" orelse c = #"e" orelse c = #"f";

  val checksumCharParser = some isChecksumChar;

  val checksumStringParser = atLeastOne checksumCharParser >> String.implode;
in
  val parser = checksumStringParser >> Checksum;
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad checksum format: " ^ s);

end
