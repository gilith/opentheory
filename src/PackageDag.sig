(* ========================================================================= *)
(* UNWIND MUTUALLY RECURSIVE THEORY PACKAGES                                 *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageDag =
sig

(* ------------------------------------------------------------------------- *)
(* Remove dead theory imports and blocks.                                    *)
(* ------------------------------------------------------------------------- *)

type theories

val mk :
    {importer : TheoryGraph.importer,
     directory : string,
     theories : PackageTheory.theory list} -> theories

val theories : theories -> PackageTheory.theory list

(* ------------------------------------------------------------------------- *)
(* Unwind mutually recursive theory packages.                                *)
(* ------------------------------------------------------------------------- *)

val unwind : theories -> theories

end
