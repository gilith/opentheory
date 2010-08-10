(* ========================================================================= *)
(* PACKAGE DIRECTORY OPERATION ERRORS                                        *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryError =
sig

(* ------------------------------------------------------------------------- *)
(* A type of directory operation errors.                                     *)
(* ------------------------------------------------------------------------- *)

datatype error =
    AlreadyInstalled
  | FilenameClash of
      {srcs : {name : string, filename : string option} list,
       dest : {filename : string}}
  | InstalledDescendent of PackageName.name
  | NotInstalled
  | UninstalledParent of PackageName.name

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val isAlreadyInstalled : error -> bool

val removeAlreadyInstalled : error list -> bool * error list

val destInstalledDescendent : error -> PackageName.name option

val isInstalledDescendent : error -> bool

val removeInstalledDescendent :
    error list -> PackageName.name list * error list

val destUninstalledParent : error -> PackageName.name option

val isUninstalledParent : error -> bool

val removeUninstalledParent :
    error list -> PackageName.name list * error list

(* ------------------------------------------------------------------------- *)
(* Fatal errors.                                                             *)
(* ------------------------------------------------------------------------- *)

val isFatal : error -> bool

val existsFatal : error list -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val toString : error -> string

val toStringList : error list -> string

end
