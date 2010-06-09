(* ========================================================================= *)
(* FINDING THEORY PACKAGES                                                   *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageFinder =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package finders.                                         *)
(* ------------------------------------------------------------------------- *)

datatype package =
    Package of
      {name : PackageName.name,
       directory : string,
       filename : string}

type finder

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : (PackageName.name -> package option) -> finder

(* ------------------------------------------------------------------------- *)
(* Finding packages.                                                         *)
(* ------------------------------------------------------------------------- *)

val find : finder -> PackageName.name -> package option

val get : finder -> PackageName.name -> package

(* ------------------------------------------------------------------------- *)
(* Finder combinators.                                                       *)
(* ------------------------------------------------------------------------- *)

val useless : finder

val orelsef : finder -> finder -> finder

val first : finder list -> finder

end
