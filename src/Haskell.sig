(* ========================================================================= *)
(* GENERATING HASKELL PROJECTS FROM THEORY PACKAGES                          *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Haskell =
sig

(* ------------------------------------------------------------------------- *)
(* A type of Haskell packages.                                               *)
(* ------------------------------------------------------------------------- *)

type haskell

(* ------------------------------------------------------------------------- *)
(* Converting a theory to a Haskell package.                                 *)
(* ------------------------------------------------------------------------- *)

val convert :
    Repository.repository -> Package.package -> Theory.theory -> haskell

(* ------------------------------------------------------------------------- *)
(* Writing a Haskell package to disk.                                        *)
(* ------------------------------------------------------------------------- *)

val toPackage : Repository.repository -> haskell -> unit

(* ------------------------------------------------------------------------- *)
(* Export a theory to a Haskell package.                                     *)
(* ------------------------------------------------------------------------- *)

val export : Repository.repository -> PackageNameVersion.nameVersion -> unit

end
