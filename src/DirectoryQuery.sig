(* ========================================================================= *)
(* QUERYING THEORY PACKAGE DIRECTORIES                                       *)
(* Copyright (c) 2012 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryQuery =
sig

(* ------------------------------------------------------------------------- *)
(* A type of query.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype set =
    Name of PackageName.name
  | NameVersion of PackageNameVersion.nameVersion
  | All
  | Empty

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
  | Compose of function * function

(* ------------------------------------------------------------------------- *)
(* Evaluating queries.                                                       *)
(* ------------------------------------------------------------------------- *)

val evaluate :
    Directory.directory -> function -> PackageNameVersionSet.set ->
    PackageNameVersionSet.set

end
