(* ========================================================================= *)
(* PACKAGE THEORY GRAPHS                                                     *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageTheoryGraph =
sig

(* ------------------------------------------------------------------------- *)
(* A type of package theory graphs.                                          *)
(* ------------------------------------------------------------------------- *)

type graph

val theories : graph -> PackageTheory.theory list

(* ------------------------------------------------------------------------- *)
(* The constructor removes dead theory imports and blocks.                   *)
(* ------------------------------------------------------------------------- *)

val mk :
    {finder : PackageFinder.finder,
     directory : string,
     theories : PackageTheory.theory list} -> graph

(* ------------------------------------------------------------------------- *)
(* Unwind mutually recursive package theory graphs.                          *)
(* ------------------------------------------------------------------------- *)

val unwind : graph -> graph

val unwound : graph -> bool

end
