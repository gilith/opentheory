(* ========================================================================= *)
(* QUERYING THEORY PACKAGE DIRECTORIES                                       *)
(* Copyright (c) 2012 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryQuery =
sig

(* ------------------------------------------------------------------------- *)
(* A type of package query.                                                  *)
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
  | Mine
  | ConsistentWithRepo
  | NotEarlierThanRepo
  | LaterThanRepo
  | Upgradable  (* Identity - NotEarlierThanRepo *)
  | Uploadable  (* LaterThanRepo & Mine & ConsistentWithRepo *)
  | Union of function * function
  | Intersect of function * function
  | Difference of function * function
  | ReflexiveTransitive of function
  | Transitive of function
  | Optional of function
  | Compose of function * function

(* ------------------------------------------------------------------------- *)
(* Does the function ignore its input?                                       *)
(* ------------------------------------------------------------------------- *)

val isConstant : function -> bool

(* ------------------------------------------------------------------------- *)
(* Evaluating queries.                                                       *)
(* ------------------------------------------------------------------------- *)

val evaluate :
    Directory.directory -> DirectoryRepo.repo list -> function ->
    PackageNameVersionSet.set -> PackageNameVersionSet.set

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSet : set Print.pp

val pp : function Print.pp

val toString : function -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parserSet : (char,set) Parse.parser

val parser : (char,function) Parse.parser

val fromString : string -> function

end
