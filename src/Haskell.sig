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

(* ------------------------------------------------------------------------- *)
(* Converting a theory package to a Haskell package.                         *)
(* ------------------------------------------------------------------------- *)

val fromPackage :
    Repository.repository -> PackageNameVersion.nameVersion -> haskell

(* ------------------------------------------------------------------------- *)
(* Writing a Haskell package to disk.                                        *)
(* ------------------------------------------------------------------------- *)

val writePackage : haskell -> unit

(* ------------------------------------------------------------------------- *)
(* Exporting a theory package as a Haskell package.                          *)
(* ------------------------------------------------------------------------- *)

val exportPackage :
    Repository.repository -> PackageNameVersion.nameVersion -> unit

end
