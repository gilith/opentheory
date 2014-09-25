(* ========================================================================= *)
(* REPOSITORY ERRORS                                                         *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure RepositoryError :> RepositoryError =
struct

open Useful;

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
      PackageNameVersion.nameVersion * RepositoryRemote.remote;

datatype errors = Errors of error list

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val clean = Errors [];

fun isClean (Errors errs) = List.null errs;

fun add (Errors errs) err = Errors (err :: errs);

fun addList (Errors errs) errl = Errors (List.revAppend (errl,errs));

val fromList = addList clean;

fun destRemove dest =
    let
      fun remove (err,(xs,errs)) =
          case dest err of
            SOME x => (x :: xs, errs)
          | NONE => (xs, err :: errs)
    in
      fn Errors errs =>
         let
           val (xs,errs) = List.foldl remove ([],[]) errs
         in
           (xs, Errors (List.rev errs))
         end
    end;

fun canRemove pred =
    let
      fun dest err = if pred err then SOME err else NONE
    in
      fn errs =>
         let
           val (xs,errs) = destRemove dest errs
         in
           (not (List.null xs), errs)
         end
    end;

(* ------------------------------------------------------------------------- *)
(* Fatal errors.                                                             *)
(* ------------------------------------------------------------------------- *)

fun isFatal err =
    case err of
      AncestorNotOnRemote _ => true
    | AncestorWrongChecksumOnRemote _ => true
    | AlreadyInstalled _ => true
    | AlreadyOnRemote _ => true
    | AlreadyStaged _ => true
    | FilenameClash _ => true
    | InstalledUser _ => true
    | MultipleAuthors _ => true
    | ObsoleteAuthors _ => false
    | NotInstalled _ => true
    | NotOnRemote _ => true
    | NotStaged _ => true
    | NoVersionInstalled _ => true
    | TagError _ => true
    | UninstalledObsolete _ => false
    | UninstalledInclude _ => true
    | WrongChecksumInclude _ => true
    | WrongChecksumObsolete _ => false
    | WrongChecksumOnRemote _ => true;

fun fatal (Errors errs) = List.exists isFatal errs;

(* ------------------------------------------------------------------------- *)
(* AlreadyInstalled errors.                                                  *)
(* ------------------------------------------------------------------------- *)

fun destAlreadyInstalled err =
    case err of
      AlreadyInstalled nv => SOME nv
    | _ => NONE;

fun isAlreadyInstalled err =
    Option.isSome (destAlreadyInstalled err);

val removeAlreadyInstalled = canRemove isAlreadyInstalled;

(* ------------------------------------------------------------------------- *)
(* AlreadyStaged errors.                                                     *)
(* ------------------------------------------------------------------------- *)

fun destAlreadyStaged err =
    case err of
      AlreadyStaged nv => SOME nv
    | _ => NONE;

fun isAlreadyStaged err =
    Option.isSome (destAlreadyStaged err);

val removeAlreadyStaged = canRemove isAlreadyStaged;

(* ------------------------------------------------------------------------- *)
(* InstalledUser errors.                                                     *)
(* ------------------------------------------------------------------------- *)

fun destInstalledUser err =
    case err of
      InstalledUser nv => SOME nv
    | _ => NONE;

fun isInstalledUser err =
    Option.isSome (destInstalledUser err);

val removeInstalledUser = destRemove destInstalledUser;

(* ------------------------------------------------------------------------- *)
(* UninstalledInclude errors.                                                *)
(* ------------------------------------------------------------------------- *)

fun destUninstalledInclude err =
    case err of
      UninstalledInclude nv => SOME nv
    | _ => NONE;

fun isUninstalledInclude err =
    Option.isSome (destUninstalledInclude err);

val removeUninstalledInclude = destRemove destUninstalledInclude;

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

  fun toStringNonEmptySet nvs =
      case PackageNameVersionSet.findl (K true) nvs of
        NONE => raise Bug "RepositoryError.toStringNonEmptySet: empty"
      | SOME nv =>
        let
          val s = PackageNameVersion.toString nv

          val n = PackageNameVersionSet.size nvs - 1
        in
          if n = 0 then s
          else
            s ^ " and " ^ Print.toString Print.ppPrettyInt n ^
            " other" ^ (if n = 1 then "" else "s")
        end;

  fun toStringAuthor (nvs,auth) =
      toStringNonEmptySet nvs ^ " authored by " ^
      PackageAuthor.toString auth;

  fun toStringAuthors authors =
      join "\n  " (List.map toStringAuthor authors);
in
  fun toString err =
      (if isFatal err then "Error" else "Warning") ^ ": " ^
      (case err of
         AncestorNotOnRemote (namever,remote) =>
         "depends on package " ^ PackageNameVersion.toString namever ^
         " missing on " ^ RepositoryRemote.toString remote
       | AncestorWrongChecksumOnRemote (namever,remote) =>
         "depends on package " ^ PackageNameVersion.toString namever ^
         " which has different checksum on " ^ RepositoryRemote.toString remote
       | AlreadyInstalled namever =>
         "package " ^ PackageNameVersion.toString namever ^
         " is already installed"
       | AlreadyOnRemote (namever,remote) =>
         "package " ^ PackageNameVersion.toString namever ^
         " already exists on " ^ RepositoryRemote.toString remote
       | AlreadyStaged namever =>
         "package " ^ PackageNameVersion.toString namever ^
         " is already staged for installation"
       | FilenameClash {srcs,dest} =>
         "filename clash in package directory:\n" ^
         "Package file " ^ PackageTheory.toStringFilename dest ^ "\n" ^
         " is target for " ^ toStringSrcs srcs
       | InstalledUser namever =>
         "in use by installed package: " ^
         PackageNameVersion.toString namever
       | MultipleAuthors auths =>
         "packages have multiple authors:\n  " ^
         toStringAuthors auths
       | NotInstalled namever =>
         "package " ^ PackageNameVersion.toString namever ^
         " is not installed"
       | NotOnRemote (namever,remote) =>
         "package " ^ PackageNameVersion.toString namever ^
         " is not on " ^ RepositoryRemote.toString remote
       | NotStaged namever =>
         "package " ^ PackageNameVersion.toString namever ^
         " is not staged for installation"
       | NoVersionInstalled name =>
         "no version of package " ^ PackageName.toString name ^
         " is installed"
       | ObsoleteAuthors auths =>
         "obsoleting packages by other authors:\n  " ^
         toStringAuthors auths
       | TagError (tag,msg) =>
         "package " ^ PackageName.toString tag ^ " information " ^ msg
       | UninstalledObsolete {upload,obsolete} =>
         "upload package " ^ PackageNameVersion.toString upload ^
         "\n  obsoletes package " ^ PackageNameVersion.toString obsolete ^
         ",\n  which is not installed"
       | UninstalledInclude (namever,_) =>
         "includes package " ^
         PackageNameVersion.toString namever ^
         " which is not installed"
       | WrongChecksumInclude namever =>
         "includes package " ^
         PackageNameVersion.toString namever ^
         " which is installed with a different checksum"
       | WrongChecksumObsolete {upload,obsolete} =>
         "upload package " ^ PackageNameVersion.toString upload ^
         "\n  obsoletes package " ^ PackageNameVersion.toString obsolete ^
         ",\n  which is installed with a different checksum"
       | WrongChecksumOnRemote (namever,remote) =>
         "package " ^ PackageNameVersion.toString namever ^
         " has different checksum on " ^ RepositoryRemote.toString remote);
end;

fun report (Errors errs) =
    case List.rev errs of
      [] => raise Bug "RepositoryError.report: clean"
    | errs => join "\n" (List.map toString errs);

end
