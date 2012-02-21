(* ========================================================================= *)
(* GENERATING HASKELL PROJECTS FROM THEORIES                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
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

val convert : Package.package -> Theory.theory -> haskell

(* ------------------------------------------------------------------------- *)
(* Writing a Haskell package to disk.                                        *)
(* ------------------------------------------------------------------------- *)

val toPackage : Directory.directory -> haskell -> unit

(* ------------------------------------------------------------------------- *)
(* Export a theory to a Haskell package.                                     *)
(* ------------------------------------------------------------------------- *)

val export : Directory.directory -> PackageNameVersion.nameVersion -> unit

end
