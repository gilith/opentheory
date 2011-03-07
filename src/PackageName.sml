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
and separatorChar = #"-"
and showString = "show"
and versionString = "version";

val separatorString = str separatorChar;

(* ------------------------------------------------------------------------- *)
(* A type of theory package names.                                           *)
(* ------------------------------------------------------------------------- *)

type name = string;

(* ------------------------------------------------------------------------- *)
(* Concatenation.                                                            *)
(* ------------------------------------------------------------------------- *)

fun append n1 n2 = n1 ^ separatorString ^ n2;

fun concat ns =
    if List.null ns then raise Error "PackageName.concat"
    else String.concatWith separatorString ns;

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

fun equal (n1 : name) n2 = n1 = n2;

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
(* Prefix names.                                                             *)
(* ------------------------------------------------------------------------- *)

fun isStrictPrefix n1 n2 =
    let
      val i1 = size n1
      and i2 = size n2
    in
      i1 < i2 andalso
      String.isPrefix n1 n2 andalso
      String.sub (n2,i1) = separatorChar
    end;

fun isPrefix n1 n2 = equal n1 n2 orelse isStrictPrefix n1 n2;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppString;

fun toString (n : name) = n;

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
