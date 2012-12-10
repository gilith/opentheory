(* ========================================================================= *)
(* BRANDED THEOREM OBJECTS                                                   *)
(* Copyright (c) 2012 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectTheorems =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theorems.                                                       *)
(* ------------------------------------------------------------------------- *)

type theorems

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : Name.name -> Sequents.sequents -> theorems

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
(* Output formats.                                                           *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> theorems

val toTextFile : {theorems : theorems, filename : string} -> unit

end
