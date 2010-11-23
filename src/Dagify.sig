(* ========================================================================= *)
(* COMPILING RECURSIVE THEORIES INTO DAGS                                    *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Dagify =
sig

(* ------------------------------------------------------------------------- *)
(* Linearize mutually recursive theory packages.                             *)
(* ------------------------------------------------------------------------- *)

val linearizeTheories :
    Graph.importer -> {directory : string} ->
    PackageTheory.theory list -> PackageTheory.theory list

end
