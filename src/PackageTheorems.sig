(* ========================================================================= *)
(* PACKAGE THEOREMS                                                          *)
(* Copyright (c) 2011 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

signature PackageTheorems =
sig

(* ------------------------------------------------------------------------- *)
(* Theorems filenames.                                                       *)
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

datatype theorems' =
    Theorems' of
      {package : PackageNameVersion.nameVersion,
       sequents : Sequents.sequents}

val mk : theorems' -> theorems

val dest : theorems -> theorems'

val package : theorems -> PackageNameVersion.nameVersion

val sequents : theorems -> Sequents.sequents

val symbol : theorems -> SymbolTable.table

val undefined : theorems -> SymbolTable.table

val defined : theorems -> SymbolTable.table

val allUndefined : theorems -> bool

val allDefined : theorems -> bool

val existsUndefined : theorems -> bool

val existsDefined : theorems -> bool

(* ------------------------------------------------------------------------- *)
(* Theory contexts.                                                          *)
(* ------------------------------------------------------------------------- *)

val context :
    SymbolTable.table * SequentSet.set -> theorems list -> Summary.context

val summaryContext :
    Summary.summary -> theorems list -> Summary.context

val packageSummaryContext :
    PackageSummary.summary -> theorems list -> Summary.context

(* ------------------------------------------------------------------------- *)
(* Testing different versions of required theories.                          *)
(* ------------------------------------------------------------------------- *)

type versions

val mkVersions : SequentSet.set -> theorems list -> versions

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
