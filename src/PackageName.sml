(* ========================================================================= *)
(* PACKAGE NAMES                                                             *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageName :> PackageName =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val authorString = "author"
and avoidString = "a"
and descriptionString = "description"
and extraSuffixString = "file"
and gilithString = "gilith"
and installedString = "installed"
and licenseString = "license"
and mainString = "main"
and nameString = "name"
and separatorString = "-"
and showString = "show"
and versionString = "version";

(* ------------------------------------------------------------------------- *)
(* A type of theory package names.                                           *)
(* ------------------------------------------------------------------------- *)

type name = string;

(* ------------------------------------------------------------------------- *)
(* Concatenation.                                                            *)
(* ------------------------------------------------------------------------- *)

fun append b1 b2 = b1 ^ separatorString ^ b2;

fun concat bs =
    if List.null bs then raise Error "PackageName.concat"
    else String.concatWith separatorString bs;

fun destSuffix suff =
    let
      val sepSuff = separatorString ^ suff

      val sepSuffSize = size sepSuff
    in
      fn n =>
         if not (String.isSuffix sepSuff n) then NONE
         else SOME (String.substring (n, 0, size n - sepSuffSize))
    end;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = String.compare;

fun equal (b1 : name) b2 = b1 = b2;

(* ------------------------------------------------------------------------- *)
(* Generating fresh names.                                                   *)
(* ------------------------------------------------------------------------- *)

fun mkAvoid i = avoidString ^ Int.toString i;

fun variantName {avoid} n : name =
    let
      fun mkNum i =
          let
            val ni = append n (mkAvoid i)
          in
            if avoid ni then mkNum (i + 1) else ni
          end
    in
      if avoid n then mkNum 1 else n
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppString;

fun toString (b : name) = b;

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

  val componentParser =
      let
        fun isInitialChar c = Char.isLower c

        fun isSubsequentChar c = Char.isLower c orelse Char.isDigit c
      in
        (some isInitialChar ++ many (some isSubsequentChar)) >>
        (fn (c,cs) => String.implode (c :: cs))
      end;
in
  val parser =
      componentParser ++
      many (separatorParser ++ componentParser >> snd) >>
      (fn (b,l) => concat (b :: l));
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package name format: " ^ s);

(* ------------------------------------------------------------------------- *)
(* Theory block names.                                                       *)
(* ------------------------------------------------------------------------- *)

val mainTheory = mainString;

(* ------------------------------------------------------------------------- *)
(* Tag names.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Package basics *)

val authorTag = authorString;

val descriptionTag = descriptionString;

val licenseTag = licenseString;

val nameTag = nameString;

val versionTag = versionString;

(* Extra package files *)

val extraSuffixTag = extraSuffixString;

(* Shows *)

val showTag = showString;

(* ------------------------------------------------------------------------- *)
(* Directory checksums names.                                                *)
(* ------------------------------------------------------------------------- *)

val installedChecksums = installedString;

(* ------------------------------------------------------------------------- *)
(* Repo names.                                                               *)
(* ------------------------------------------------------------------------- *)

val gilithRepo = gilithString;

end

structure PackageNameOrdered =
struct type t = PackageName.name val compare = PackageName.compare end

structure PackageNameMap = KeyMap (PackageNameOrdered)

structure PackageNameSet = ElementSet (PackageNameMap)
