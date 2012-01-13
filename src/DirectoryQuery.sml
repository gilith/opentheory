(* ========================================================================= *)
(* QUERYING THEORY PACKAGE DIRECTORIES                                       *)
(* Copyright (c) 2012 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryQuery :> DirectoryQuery =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of query.                                                          *)
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
  | Compose of function * function;

(* ------------------------------------------------------------------------- *)
(* Evaluating queries.                                                       *)
(* ------------------------------------------------------------------------- *)

fun evaluateSet dir set =
    case set of
      Name n => Directory.nameVersions dir n
    | NameVersion nv => PackageNameVersionSet.singleton nv
    | All => Directory.all dir
    | Empty => PackageNameVersionSet.empty;

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
    | ReflexiveTransitive func1 =>
      let
        val f1 = evaluate dir func1

        fun f2 nv = f1 (PackageNameVersionSet.singleton nv)
      in
        PackageNameVersionSet.close f2
      end
    | Compose (func1,func2) =>
      let
        val f1 = evaluate dir func1
        and f2 = evaluate dir func2
      in
        f1 o f2
      end;

end
