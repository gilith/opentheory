(* ========================================================================= *)
(* PACKAGE DIRECTORY OPERATION ERRORS                                        *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryError :> DirectoryError =
struct

open Useful;

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
  | ObsoleteAuthors of
      (PackageNameVersion.nameVersion * {author : string}) list
  | TagError of
      PackageTag.name * string
  | UninstalledObsolete of
      {upload : PackageNameVersion.nameVersion,
       obsolete : PackageNameVersion.nameVersion}
  | UninstalledParent of
      PackageNameVersion.nameVersion
  | UninstalledUpgrade of
      PackageNameVersion.nameVersion
  | WrongChecksumObsolete of
      {upload : PackageNameVersion.nameVersion,
       obsolete : PackageNameVersion.nameVersion}
  | WrongChecksumOnRepo of
      PackageNameVersion.nameVersion * DirectoryRepo.repo;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun destAlreadyInstalled err =
    case err of
      AlreadyInstalled nv => SOME nv
    | _ => NONE;

fun isAlreadyInstalled err =
    Option.isSome (destAlreadyInstalled err);

fun removeAlreadyInstalled errs =
    let
      val (xs,errs) = List.partition isAlreadyInstalled errs

      val removed = not (List.null xs)
    in
      (removed,errs)
    end;

fun destAlreadyStaged err =
    case err of
      AlreadyStaged nv => SOME nv
    | _ => NONE;

fun isAlreadyStaged err =
    Option.isSome (destAlreadyStaged err);

fun removeAlreadyStaged errs =
    let
      val (xs,errs) = List.partition isAlreadyStaged errs

      val removed = not (List.null xs)
    in
      (removed,errs)
    end;

fun destInstalledDescendent err =
    case err of
      InstalledDescendent nv => SOME nv
    | _ => NONE;

fun isInstalledDescendent err =
    Option.isSome (destInstalledDescendent err);

val removeInstalledDescendent =
    let
      fun remove (err,(nvs,errs)) =
          case destInstalledDescendent err of
            SOME nv => (nv :: nvs, errs)
          | NONE => (nvs, err :: errs)
    in
      List.foldr remove ([],[])
    end;

fun destUninstalledParent err =
    case err of
      UninstalledParent nv => SOME nv
    | _ => NONE;

fun isUninstalledParent err =
    Option.isSome (destUninstalledParent err);

val removeUninstalledParent =
    let
      fun remove (err,(nvs,errs)) =
          case destUninstalledParent err of
            SOME nv => (nv :: nvs, errs)
          | NONE => (nvs, err :: errs)
    in
      List.foldr remove ([],[])
    end;

(* ------------------------------------------------------------------------- *)
(* Fatal errors.                                                             *)
(* ------------------------------------------------------------------------- *)

fun isFatal err =
    case err of
      AncestorNotOnRepo _ => false
    | AncestorWrongChecksumOnRepo _ => true
    | AlreadyInstalled _ => true
    | AlreadyOnRepo _ => true
    | AlreadyStaged _ => true
    | FilenameClash _ => true
    | InstalledDescendent _ => true
    | MultipleAuthors _ => true
    | ObsoleteAuthors _ => false
    | NotInstalled _ => true
    | NotOnRepo _ => true
    | NotStaged _ => true
    | TagError _ => true
    | UninstalledObsolete _ => false
    | UninstalledParent _ => true
    | UninstalledUpgrade _ => false
    | WrongChecksumObsolete _ => false
    | WrongChecksumOnRepo _ => true;

val existsFatal = List.exists isFatal;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun toStringSrc {name,filename} =
      name ^
      (case filename of
         SOME sf => ": " ^ PackageTheory.toStringFilename {filename = sf}
       | NONE => "");

  fun toStringSrcs srcs =
      join "\n  and also for " (List.map toStringSrc srcs);

  fun toStringAuthor (nv,{author}) =
      PackageNameVersion.toString nv ^ " authored by " ^ author;

  fun toStringAuthors authors =
      join "\n  " (List.map toStringAuthor authors);
in
  fun toString err =
      (if isFatal err then "Error" else "Warning") ^ ": " ^
      (case err of
         AncestorNotOnRepo (namever,repo) =>
         "depends on package " ^ PackageNameVersion.toString namever ^
         " missing on " ^ DirectoryRepo.toString repo
       | AncestorWrongChecksumOnRepo (namever,repo) =>
         "depends on package " ^ PackageNameVersion.toString namever ^
         " which has different checksum on " ^ DirectoryRepo.toString repo
       | AlreadyInstalled namever =>
         "package " ^ PackageNameVersion.toString namever ^
         " is already installed"
       | AlreadyOnRepo (namever,repo) =>
         "package " ^ PackageNameVersion.toString namever ^
         " already exists on " ^ DirectoryRepo.toString repo
       | AlreadyStaged namever =>
         "package " ^ PackageNameVersion.toString namever ^
         " is already staged for installation"
       | FilenameClash {srcs,dest} =>
         "filename clash in package directory:\n" ^
         "Package file " ^ PackageTheory.toStringFilename dest ^ "\n" ^
         " is target for " ^ toStringSrcs srcs
       | InstalledDescendent namever =>
         "in use by installed package: " ^
         PackageNameVersion.toString namever
       | MultipleAuthors auths =>
         "packages have multiple authors:\n  " ^
         toStringAuthors auths
       | NotInstalled namever =>
         "package " ^ PackageNameVersion.toString namever ^
         " is not installed"
       | NotOnRepo (namever,repo) =>
         "package " ^ PackageNameVersion.toString namever ^
         " is not on " ^ DirectoryRepo.toString repo
       | NotStaged namever =>
         "package " ^ PackageNameVersion.toString namever ^
         " is not staged for installation"
       | ObsoleteAuthors auths =>
         "obsoleting packages by other authors:\n  " ^
         toStringAuthors auths
       | TagError (tag,msg) =>
         "package " ^ PackageName.toString tag ^ " information " ^ msg
       | UninstalledObsolete {upload,obsolete} =>
         "upload package " ^ PackageNameVersion.toString upload ^
         " obsoletes package " ^ PackageNameVersion.toString obsolete ^
         ",\n  which is not installed"
       | UninstalledParent namever =>
         "depends on package " ^
         PackageNameVersion.toString namever ^
         " which is not installed"
       | UninstalledUpgrade namever =>
         "depends on package " ^
         PackageNameVersion.toString namever ^
         " which is not installed"
       | WrongChecksumObsolete {upload,obsolete} =>
         "upload package " ^ PackageNameVersion.toString upload ^
         " obsoletes package " ^ PackageNameVersion.toString obsolete ^
         ",\n  which is installed with a different checksum"
       | WrongChecksumOnRepo (namever,repo) =>
         "package " ^ PackageNameVersion.toString namever ^
         " has different checksum on " ^ DirectoryRepo.toString repo);
end;

fun toStringList errs = join "\n" (List.map toString errs);

end
