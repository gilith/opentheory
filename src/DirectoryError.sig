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
    AncestorNotOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.name
  | AncestorWrongChecksumOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.name
  | AlreadyInstalled of
      PackageNameVersion.nameVersion
  | AlreadyOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.name
  | AlreadyStaged of
      PackageNameVersion.nameVersion
  | FilenameClash of
      {srcs : {name : string, filename : string option} list,
       dest : {filename : string}}
  | InstalledDescendent of
      PackageNameVersion.nameVersion
  | NotInstalled of
      PackageNameVersion.nameVersion
  | NotOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.name
  | TagError of
      PackageTag.name * string
  | UninstalledParent of
      PackageNameVersion.nameVersion
  | WrongChecksumOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.name

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* AlreadyInstalled *)

val destAlreadyInstalled : error -> PackageNameVersion.nameVersion option

val isAlreadyInstalled : error -> bool

val removeAlreadyInstalled : error list -> bool * error list

(* AlreadyStaged *)

val destAlreadyStaged : error -> PackageNameVersion.nameVersion option

val isAlreadyStaged : error -> bool

val removeAlreadyStaged : error list -> bool * error list

(* InstalledDescendent *)

val destInstalledDescendent : error -> PackageNameVersion.nameVersion option

val isInstalledDescendent : error -> bool

val removeInstalledDescendent :
    error list -> PackageNameVersion.nameVersion list * error list

(* UninstalledParent *)

val destUninstalledParent : error -> PackageNameVersion.nameVersion option

val isUninstalledParent : error -> bool

val removeUninstalledParent :
    error list -> PackageNameVersion.nameVersion list * error list

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
