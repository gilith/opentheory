(* ========================================================================= *)
(* UNWIND MUTUALLY RECURSIVE THEORY PACKAGES                                 *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Dagify =
sig

(* ------------------------------------------------------------------------- *)
(* Remove dead theory imports and blocks.                                    *)
(* ------------------------------------------------------------------------- *)

type theories

val mk :
    {importer : Graph.importer,
     directory : string,
     theories : PackageTheory.theory list} -> theories

val theories : theories -> PackageTheory.theory list

(* ------------------------------------------------------------------------- *)
(* Unwind mutually recursive theory packages.                                *)
(* ------------------------------------------------------------------------- *)

val unwind : theories -> theories

end
