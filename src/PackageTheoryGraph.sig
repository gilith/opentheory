(* ========================================================================= *)
(* PACKAGE THEORY GRAPHS                                                     *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageTheoryGraph =
sig

(* ------------------------------------------------------------------------- *)
(* Topological sort of theory blocks to respect union blocks.                *)
(* ------------------------------------------------------------------------- *)

val sortUnion : PackageTheory.theory list -> PackageTheory.theory list

(* ------------------------------------------------------------------------- *)
(* Topological sort of theory blocks to respect import declarations.         *)
(* ------------------------------------------------------------------------- *)

val sortImports : PackageTheory.theory list -> PackageTheory.theory list

(* ------------------------------------------------------------------------- *)
(* Remove dead theory blocks and import declarations.                        *)
(* ------------------------------------------------------------------------- *)

val removeDead :
    {finder : PackageFinder.finder,
     directory : string,
     outputWarning : bool,
     theories : PackageTheory.theory list} -> PackageTheory.theory list

(* ------------------------------------------------------------------------- *)
(* Add checksums to package include theory blocks.                           *)
(* ------------------------------------------------------------------------- *)

val addChecksums :
    {finder : PackageFinder.finder,
     theories : PackageTheory.theory list} -> PackageTheory.theory list

(* ------------------------------------------------------------------------- *)
(* Clean up theory blocks (addChecksums o removeDead o sortImports).         *)
(* ------------------------------------------------------------------------- *)

val clean :
    {finder : PackageFinder.finder,
     directory : string,
     outputWarning : bool,
     theories : PackageTheory.theory list} -> PackageTheory.theory list

end
