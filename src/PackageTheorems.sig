(* ========================================================================= *)
(* PACKAGE THEOREMS                                                          *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageTheorems =
sig

(* ------------------------------------------------------------------------- *)
(* Package theorems filenames.                                               *)
(* ------------------------------------------------------------------------- *)

val mkFilename : PackageNameVersion.nameVersion -> {filename : string}

val destFilename : {filename : string} -> PackageNameVersion.nameVersion option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* A type of package theorems.                                               *)
(* ------------------------------------------------------------------------- *)

type theorems

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : PackageNameVersion.nameVersion -> Sequents.sequents -> theorems

val nameVersion : theorems -> PackageNameVersion.nameVersion

val theorems : theorems -> ObjectTheorems.theorems

val sequents : theorems -> Sequents.sequents

val symbol : theorems -> SymbolTable.table

val partitionUndef :
    theorems -> {undefined : SymbolTable.table, defined : SymbolTable.table}

val undefined : theorems -> SymbolTable.table

val defined : theorems -> SymbolTable.table

val allUndefined : theorems -> bool

val allDefined : theorems -> bool

val existsUndefined : theorems -> bool

val existsDefined : theorems -> bool

(* ------------------------------------------------------------------------- *)
(* Theory contexts.                                                          *)
(* ------------------------------------------------------------------------- *)

val context : Summary.summary -> theorems list -> Summary.context

val packageContext :
    PackageSummary.summary -> theorems list -> Summary.context

(* ------------------------------------------------------------------------- *)
(* Testing different versions of required theories.                          *)
(* ------------------------------------------------------------------------- *)

type versions

val mkVersions : Summary.summary -> theorems list -> versions

val addVersion : versions -> theorems -> versions

(* ------------------------------------------------------------------------- *)
(* Output formats.                                                           *)
(* ------------------------------------------------------------------------- *)

val fromTextFile :
    {package : PackageNameVersion.nameVersion,
     filename : string} ->
    theorems

val toTextFile : {theorems : theorems, filename : string} -> unit

end
