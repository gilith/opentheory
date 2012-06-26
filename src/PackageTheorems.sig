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

val sequents : theorems -> Sequents.sequents

val symbol : theorems -> SymbolTable.table

val defined : theorems -> SymbolTable.table

(* ------------------------------------------------------------------------- *)
(* Unsatisfied assumptions.                                                  *)
(* ------------------------------------------------------------------------- *)

val unsatisfiedAssumptions :
    theorems list -> (SequentSet.set -> SequentSet.set) option

(* ------------------------------------------------------------------------- *)
(* Output formats.                                                           *)
(* ------------------------------------------------------------------------- *)

val fromTextFile :
    {package : PackageNameVersion.nameVersion,
     filename : string} ->
    theorems

val toTextFile : {theorems : theorems, filename : string} -> unit

end
