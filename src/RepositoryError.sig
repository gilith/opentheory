(* ========================================================================= *)
(* REPOSITORY ERRORS                                                         *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature RepositoryError =
sig

(* ------------------------------------------------------------------------- *)
(* A type of repository errors.                                              *)
(* ------------------------------------------------------------------------- *)

datatype error =
    AncestorNotOnRemote of
      PackageNameVersion.nameVersion * RepositoryRemote.remote
  | AncestorWrongChecksumOnRemote of
      PackageNameVersion.nameVersion * RepositoryRemote.remote
  | AlreadyInstalled of
      PackageNameVersion.nameVersion
  | AlreadyOnRemote of
      PackageNameVersion.nameVersion * RepositoryRemote.remote
  | AlreadyStaged of
      PackageNameVersion.nameVersion
  | FilenameClash of
      {srcs : {name : string, filename : string option} list,
       dest : {filename : string}}
  | InstalledUser of
      PackageNameVersion.nameVersion
  | MultipleAuthors of
      (PackageNameVersionSet.set * PackageAuthor.author) list
  | NotInstalled of
      PackageNameVersion.nameVersion
  | NotOnRemote of
      PackageNameVersion.nameVersion * RepositoryRemote.remote
  | NotStaged of
      PackageNameVersion.nameVersion
  | NoVersionInstalled of
      PackageName.name
  | ObsoleteAuthors of
      (PackageNameVersionSet.set * PackageAuthor.author) list
  | TagError of
      PackageTag.name * string
  | UninstalledObsolete of
      {upload : PackageNameVersion.nameVersion,
       obsolete : PackageNameVersion.nameVersion}
  | UninstalledInclude of
      PackageNameVersion.nameVersion * Checksum.checksum option
  | WrongChecksumInclude of
      PackageNameVersion.nameVersion
  | WrongChecksumObsolete of
      {upload : PackageNameVersion.nameVersion,
       obsolete : PackageNameVersion.nameVersion}
  | WrongChecksumOnRemote of
      PackageNameVersion.nameVersion * RepositoryRemote.remote

type errors

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val clean : errors

val isClean : errors -> bool

val add : errors -> error -> errors

val addList : errors -> error list -> errors

val fromList : error list -> errors

(* ------------------------------------------------------------------------- *)
(* Fatal errors.                                                             *)
(* ------------------------------------------------------------------------- *)

val isFatal : error -> bool

val fatal : errors -> bool

(* ------------------------------------------------------------------------- *)
(* AlreadyInstalled errors.                                                  *)
(* ------------------------------------------------------------------------- *)

val destAlreadyInstalled : error -> PackageNameVersion.nameVersion option

val isAlreadyInstalled : error -> bool

val removeAlreadyInstalled : errors -> bool * errors

(* ------------------------------------------------------------------------- *)
(* AlreadyStaged errors.                                                     *)
(* ------------------------------------------------------------------------- *)

val destAlreadyStaged : error -> PackageNameVersion.nameVersion option

val isAlreadyStaged : error -> bool

val removeAlreadyStaged : errors -> bool * errors

(* ------------------------------------------------------------------------- *)
(* InstalledUser errors.                                                     *)
(* ------------------------------------------------------------------------- *)

val destInstalledUser : error -> PackageNameVersion.nameVersion option

val isInstalledUser : error -> bool

val removeInstalledUser :
    errors -> PackageNameVersion.nameVersion list * errors

(* ------------------------------------------------------------------------- *)
(* UninstalledInclude errors.                                                *)
(* ------------------------------------------------------------------------- *)

val destUninstalledInclude :
    error ->
    (PackageNameVersion.nameVersion * Checksum.checksum option) option

val isUninstalledInclude : error -> bool

val removeUninstalledInclude :
    errors ->
    (PackageNameVersion.nameVersion * Checksum.checksum option) list * errors

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val toString : error -> string

val report : errors -> string

end
