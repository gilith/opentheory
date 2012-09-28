(* ========================================================================= *)
(* QUERYING THEORY PACKAGE DIRECTORIES                                       *)
(* Copyright (c) 2012 Joe Leslie-Hurd, distributed under the MIT license     *)
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
  | None

datatype predicate =
    Empty
  | Mine
  | Closed
  | Acyclic
  | WellFounded
  | OnRepo
  | IdenticalOnRepo
  | ConsistentWithRepo
  | EarlierThanRepo
  | LaterThanRepo
  | Not of predicate
  | And of predicate * predicate
  | Or of predicate * predicate

datatype function =
    Identity
  | Constant of set
  | Filter of predicate
  | Requires
  | RequiredBy
  | Includes
  | IncludedBy
  | Subtheories
  | SubtheoryOf
  | Versions
  | Latest
  | Deprecated  (* (Identity - Latest) (Requires|Includes)* *)
  | Obsolete  (* All - (Requires|Includes)* *)
  | Upgradable  (* EarlierThanRepo *)
  | Uploadable  (* Mine & (~OnRepo /\ ~EarlierThanRepo /\ ConsistentWithRepo) *)
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

val ignoresRepo : predicate -> bool

val ignoresInput : function -> bool

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

val ppPredicate : predicate Print.pp

val pp : function Print.pp

val toString : function -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parserSet : (char,set) Parse.parser

val parserPredicate : (char,predicate) Parse.parser

val parser : (char,function) Parse.parser

val fromString : string -> function

end
