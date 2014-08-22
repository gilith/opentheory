(* ========================================================================= *)
(* UNWIND MUTUALLY RECURSIVE PACKAGE THEORY GRAPHS                           *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageTheoryGraph =
sig

(* ------------------------------------------------------------------------- *)
(* Remove dead theory imports and blocks.                                    *)
(* ------------------------------------------------------------------------- *)

type theory

val mk :
    {finder : PackageFinder.finder,
     directory : string,
     theory : PackageTheory.theory list} -> theory

val theory : theory -> PackageTheory.theory list

(* ------------------------------------------------------------------------- *)
(* Unwind mutually recursive theory packages.                                *)
(* ------------------------------------------------------------------------- *)

val unwind : theory -> theory

end
