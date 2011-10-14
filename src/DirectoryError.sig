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
      PackageNameVersion.nameVersion * DirectoryRepo.repo
  | AncestorWrongChecksumOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.repo
  | AlreadyInstalled of
      PackageNameVersion.nameVersion
  | AlreadyOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.repo
  | AlreadyStaged of
      PackageNameVersion.nameVersion
  | FilenameClash of
      {srcs : {name : string, filename : string option} list,
       dest : {filename : string}}
  | InstalledDescendent of
      PackageNameVersion.nameVersion
  | MultipleAuthors of
      (PackageNameVersion.nameVersion * {author : string}) list
  | NotInstalled of
      PackageNameVersion.nameVersion
  | NotOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.repo
  | NotStaged of
      PackageNameVersion.nameVersion
  | NoVersionInstalled of
      PackageName.name
  | ObsoleteAuthors of
      (PackageNameVersion.nameVersion * {author : string}) list
  | TagError of
      PackageTag.name * string
  | UninstalledObsolete of
      {upload : PackageNameVersion.nameVersion,
       obsolete : PackageNameVersion.nameVersion}
  | UninstalledParent of
      PackageNameVersion.nameVersion
  | WrongChecksumObsolete of
      {upload : PackageNameVersion.nameVersion,
       obsolete : PackageNameVersion.nameVersion}
  | WrongChecksumOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.repo

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
