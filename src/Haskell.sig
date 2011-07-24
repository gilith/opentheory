(* ========================================================================= *)
(* GENERATING HASKELL PROJECTS FROM THEORIES                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Haskell =
sig

val prefix : PackageName.name

val export : Directory.directory -> PackageNameVersion.nameVersion -> unit

end
