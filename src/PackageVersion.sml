(* ========================================================================= *)
(* PACKAGE VERSIONS                                                          *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageVersion :> PackageVersion =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val separatorString = ".";

(* ------------------------------------------------------------------------- *)
(* A type of theory package versions.                                        *)
(* ------------------------------------------------------------------------- *)

datatype version = Version of int * int list;

(* ------------------------------------------------------------------------- *)
(* Converting between integer lists.                                         *)
(* ------------------------------------------------------------------------- *)

fun toList (Version (i,l)) = i :: l;

fun fromList l =
    case l of
      [] => raise Bug "PackageVersion.fromList: null"
    | h :: t => Version (h,t);

(* ------------------------------------------------------------------------- *)
(* Incrementing versions.                                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun inc i l =
      case l of
        [] => (i + 1, [])
      | h :: t => (i, op:: (inc h t));
in
  fun increment (Version (i,l)) = Version (inc i l);
end;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (v1,v2) =
    let
      val Version (i1,l1) = v1
      and Version (i2,l2) = v2
    in
      case Int.compare (i1,i2) of
        LESS => LESS
      | EQUAL => lexCompare Int.compare (l1,l2)
      | GREATER => GREATER
    end;

fun equal (Version v1) (Version v2) = v1 = v2;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSeparator = Print.ppString separatorString;

fun pp (Version (i,l)) =
    Print.program
      (Print.ppInt i :: List.map (Print.sequence ppSeparator o Print.ppInt) l);

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val separatorParser = exactString separatorString;

  val zeroParser = exactChar #"0" >> K 0;

  val nonzeroParser =
      exactChar #"1" >> K 1 ||
      exactChar #"2" >> K 2 ||
      exactChar #"3" >> K 3 ||
      exactChar #"4" >> K 4 ||
      exactChar #"5" >> K 5 ||
      exactChar #"6" >> K 6 ||
      exactChar #"7" >> K 7 ||
      exactChar #"8" >> K 8 ||
      exactChar #"9" >> K 9;

  val digitParser = zeroParser || nonzeroParser;

  local
    fun mkNum acc l =
        case l of
          [] => acc
        | d :: l => mkNum (10 * acc + d) l;
  in
    val componentParser =
        zeroParser ||
        nonzeroParser ++ many digitParser >> uncurry mkNum;
  end;
in
  val parser =
      componentParser ++
      many (separatorParser ++ componentParser >> snd) >>
      Version;
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package version format: " ^ s);

end

structure PackageVersionOrdered =
struct type t = PackageVersion.version val compare = PackageVersion.compare end

structure PackageVersionMap = KeyMap (PackageVersionOrdered)

structure PackageVersionSet =
struct

  local
    structure S = ElementSet (PackageVersionMap);
  in
    open S;
  end;

  fun latestVersion set = findr (Useful.K true) set;

  val pp =
      Print.ppMap
        toList
        (Print.ppBracket "{" "}" (Print.ppOpList "," PackageVersion.pp));

end
