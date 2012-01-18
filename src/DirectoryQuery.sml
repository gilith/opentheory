(* ========================================================================= *)
(* QUERYING THEORY PACKAGE DIRECTORIES                                       *)
(* Copyright (c) 2012 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryQuery :> DirectoryQuery =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val allKeywordString = "All"
and emptyKeywordString = "Empty"
and differenceSymbolString = "-"
and identityKeywordString = "Identity"
and includedByKeywordString = "IncludedBy"
and includesKeywordString = "Includes"
and intersectSymbolString = "/\\"
and latestKeywordString = "Latest"
and optionalSymbolString = "?"
and reflexiveTransitiveSymbolString = "*"
and requiredByKeywordString = "RequiredBy"
and requiresKeywordString = "Requires"
and subtheoriesKeywordString = "Subtheories"
and subtheoryOfKeywordString = "SubtheoryOf"
and transitiveSymbolString = "+"
and unionSymbolString = "\\/";

(* ------------------------------------------------------------------------- *)
(* A type of package query.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype set =
    Name of PackageName.name
  | NameVersion of PackageNameVersion.nameVersion
  | All
  | Empty;

datatype function =
    Identity
  | Constant of set
  | Requires
  | RequiredBy
  | Includes
  | IncludedBy
  | Subtheories
  | SubtheoryOf
  | Latest
  | Union of function * function
  | Intersect of function * function
  | Difference of function * function
  | ReflexiveTransitive of function
  | Transitive of function
  | Optional of function
  | Compose of function * function;

(* ------------------------------------------------------------------------- *)
(* Does the function ignore its input?                                       *)
(* ------------------------------------------------------------------------- *)

fun isConstant func =
    case func of
      Identity => false
    | Constant _ => true
    | Requires => false
    | RequiredBy => false
    | Includes => false
    | IncludedBy => false
    | Subtheories => false
    | SubtheoryOf => false
    | Latest => false
    | Union (func1,func2) => isConstant func1 andalso isConstant func2
    | Intersect (func1,func2) => isConstant func1 andalso isConstant func2
    | Difference (func1,func2) => isConstant func1 andalso isConstant func2
    | ReflexiveTransitive _ => false
    | Transitive func => isConstant func
    | Optional _ => false
    | Compose (func1,func2) => isConstant func1 orelse isConstant func2;

(* ------------------------------------------------------------------------- *)
(* Evaluating queries.                                                       *)
(* ------------------------------------------------------------------------- *)

fun evaluateSet dir set =
    case set of
      Name n => Directory.nameVersions dir n
    | NameVersion nv => PackageNameVersionSet.singleton nv
    | All => Directory.all dir
    | Empty => PackageNameVersionSet.empty;

local
  fun rtc f set =
      let
        val set' = PackageNameVersionSet.union (f set) set

        val stable =
            PackageNameVersionSet.size set' = PackageNameVersionSet.size set
      in
        if stable then set else rtc f set
      end;
in
  fun evaluate dir func =
      case func of
        Identity => I
      | Constant set => K (evaluateSet dir set)
      | Requires => PackageNameVersionSet.lift (Directory.requires dir)
      | RequiredBy => PackageNameVersionSet.lift (Directory.requiredBy dir)
      | Includes => PackageNameVersionSet.lift (Directory.includes dir)
      | IncludedBy => PackageNameVersionSet.lift (Directory.includedBy dir)
      | Subtheories => PackageNameVersionSet.lift (Directory.subtheories dir)
      | SubtheoryOf => PackageNameVersionSet.lift (Directory.subtheoryOf dir)
      | Latest => PackageNameVersionSet.latestVersions
      | Union (func1,func2) =>
        let
          val f1 = evaluate dir func1
          and f2 = evaluate dir func2
        in
          fn s => PackageNameVersionSet.union (f1 s) (f2 s)
        end
      | Intersect (func1,func2) =>
        let
          val f1 = evaluate dir func1
          and f2 = evaluate dir func2
        in
          fn s => PackageNameVersionSet.intersect (f1 s) (f2 s)
        end
      | Difference (func1,func2) =>
        let
          val f1 = evaluate dir func1
          and f2 = evaluate dir func2
        in
          fn s => PackageNameVersionSet.difference (f1 s) (f2 s)
        end
      | ReflexiveTransitive func =>
        let
          val f = evaluate dir func
        in
          rtc f
        end
      | Transitive func =>
        let
          val f = evaluate dir func
        in
          rtc f o f
        end
      | Optional func =>
        let
          val f = evaluate dir func
        in
          fn s => PackageNameVersionSet.union s (f s)
        end
      | Compose (func1,func2) =>
        let
          val f1 = evaluate dir func1
          and f2 = evaluate dir func2
        in
          f1 o f2
        end;
end;

(***
(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : function Print.pp

val toString : function -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,function) Parse.parser

val fromString : string -> function
***)

end
