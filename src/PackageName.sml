(* ========================================================================= *)
(* PACKAGE NAMES                                                             *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageName :> PackageName =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val separatorString = "-";

(* ------------------------------------------------------------------------- *)
(* A type of theory package names.                                           *)
(* ------------------------------------------------------------------------- *)

datatype name' =
    Name' of
      {base : PackageBase.base,
       version : PackageVersion.version};

type name = name';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk n' : name = n';

fun dest n : name' = n;

fun base' (Name' {base = x, ...}) = x;

fun version' (Name' {version = x, ...}) = x;

fun base n = base' (dest n);

fun version n = version' (dest n);

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (i1,i2) =
    let
      val Name' {base = b1, version = v1} = dest i1
      and Name' {base = b2, version = v2} = dest i2
    in
      case PackageBase.compare (b1,b2) of
        LESS => LESS
      | EQUAL => PackageVersion.compare (v1,v2)
      | GREATER => GREATER
    end;

fun equal i1 i2 =
    let
      val Name' {base = b1, version = v1} = dest i1
      and Name' {base = b2, version = v2} = dest i2
    in
      PackageBase.equal b1 b2 andalso
      PackageVersion.equal v1 v2
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSeparator = Print.addString separatorString;

fun pp' n =
    let
      val Name' {base = b, version = v} = n
    in
      Print.program
        [PackageBase.pp b,
         ppSeparator,
         PackageVersion.pp v]
    end;

val pp = Print.ppMap dest pp';

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

  val parser' =
      PackageBase.parser ++
      separatorParser ++
      PackageVersion.parser >>
      (fn (b,((),v)) => Name' {base = b, version = v});
in
  val parser = parser' >> mk;
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package name format: " ^ s);

end

structure PackageNameOrdered =
struct type t = PackageName.name val compare = PackageName.compare end

structure PackageNameSet = ElementSet (PackageNameOrdered)

structure PackageNameMap = KeyMap (PackageNameOrdered)
