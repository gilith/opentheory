(* ========================================================================= *)
(* EXPORTING THEORY PACKAGES AS HASKELL PACKAGES                             *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Haskell =
sig

(* ------------------------------------------------------------------------- *)
(* A type of Haskell packages.                                               *)
(* ------------------------------------------------------------------------- *)

type haskell

val name : haskell -> PackageName.name

(* ------------------------------------------------------------------------- *)
(* Converting a theory package to a Haskell package.                         *)
(* ------------------------------------------------------------------------- *)

val exportable :
    Repository.repository -> PackageNameVersion.nameVersion -> bool

val fromPackage :
    Repository.repository -> PackageNameVersion.nameVersion -> haskell

(* ------------------------------------------------------------------------- *)
(* Writing a Haskell package to disk.                                        *)
(* ------------------------------------------------------------------------- *)

val writePackage :
    {reexport : bool} -> haskell ->
    PackageName.name * ({reexport : bool} * PackageVersion.version) option

(* ------------------------------------------------------------------------- *)
(* Exporting a theory package as a Haskell package.                          *)
(* ------------------------------------------------------------------------- *)

val exportPackage :
    {reexport : bool} ->
    Repository.repository -> PackageNameVersion.nameVersion ->
    PackageName.name * ({reexport : bool} * PackageVersion.version) option

end
