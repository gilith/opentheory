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
    AncestorNotOnRepo of PackageName.name * DirectoryRepo.name
  | AncestorWrongChecksumOnRepo of PackageName.name * DirectoryRepo.name
  | AlreadyInstalled of PackageName.name
  | AlreadyOnRepo of PackageName.name * DirectoryRepo.name
  | AlreadyStaged of PackageName.name
  | FilenameClash of
      {srcs : {name : string, filename : string option} list,
       dest : {filename : string}}
  | InstalledDescendent of PackageName.name
  | NotInstalled of PackageName.name
  | NotOnRepo of PackageName.name * DirectoryRepo.name
  | UninstalledParent of PackageName.name
  | WrongChecksumOnRepo of PackageName.name * DirectoryRepo.name;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun destAlreadyInstalled err =
    case err of
      AlreadyInstalled n => SOME n
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
      AlreadyStaged n => SOME n
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
      InstalledDescendent name => SOME name
    | _ => NONE;

fun isInstalledDescendent err =
    Option.isSome (destInstalledDescendent err);

val removeInstalledDescendent =
    let
      fun remove (err,(names,errs)) =
          case destInstalledDescendent err of
            SOME name => (name :: names, errs)
          | NONE => (names, err :: errs)
    in
      List.foldr remove ([],[])
    end;

fun destUninstalledParent err =
    case err of
      UninstalledParent name => SOME name
    | _ => NONE;

fun isUninstalledParent err =
    Option.isSome (destUninstalledParent err);

val removeUninstalledParent =
    let
      fun remove (err,(names,errs)) =
          case destUninstalledParent err of
            SOME name => (name :: names, errs)
          | NONE => (names, err :: errs)
    in
      List.foldr remove ([],[])
    end;

(* ------------------------------------------------------------------------- *)
(* Fatal errors.                                                             *)
(* ------------------------------------------------------------------------- *)

fun isFatal err =
    case err of
      AncestorNotOnRepo _ => true
    | AncestorWrongChecksumOnRepo _ => true
    | AlreadyInstalled _ => true
    | AlreadyOnRepo _ => true
    | AlreadyStaged _ => true
    | FilenameClash _ => true
    | InstalledDescendent _ => true
    | NotInstalled _ => true
    | NotOnRepo _ => true
    | UninstalledParent _ => true
    | WrongChecksumOnRepo _ => true

val existsFatal = List.exists isFatal;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun toString err =
    (if isFatal err then "Error" else "Warning") ^ ": " ^
    (case err of
       AncestorNotOnRepo (name,repo) =>
       "depends on package " ^ PackageName.toString name ^
       " missing on " ^ repo ^ " repo"
     | AncestorWrongChecksumOnRepo (name,repo) =>
       "depends on package " ^ PackageName.toString name ^
       " which has different checksum on " ^ repo ^ " repo"
     | AlreadyInstalled name =>
       "package " ^ PackageName.toString name ^ " is already installed"
     | AlreadyOnRepo (name,repo) =>
       "package " ^ PackageName.toString name ^
       " already exists on " ^ repo ^ " repo"
     | AlreadyStaged name =>
       "package " ^ PackageName.toString name ^
       " is already staged for installation"
     | FilenameClash {srcs,dest} =>
       let
         fun toStringSrc {name,filename} =
             name ^
             (case filename of
                SOME sf => ": " ^ PackageTheory.toStringFilename {filename = sf}
              | NONE => "")
       in
         "filename clash in package directory:\n" ^
         "Package file " ^ PackageTheory.toStringFilename dest ^ "\n" ^
         " is target for " ^ join "\n  and also for " (List.map toStringSrc srcs)
       end
     | InstalledDescendent name =>
       "in use by installed package: " ^ PackageName.toString name
     | NotInstalled name =>
       "package " ^ PackageName.toString name ^ " is not installed"
     | NotOnRepo (name,repo) =>
       "package " ^ PackageName.toString name ^
       " is not on " ^ repo ^ " repo"
     | UninstalledParent name =>
       "depends on uninstalled package: " ^ PackageName.toString name
     | WrongChecksumOnRepo (name,repo) =>
       "package " ^ PackageName.toString name ^
       " has different checksum on " ^ repo ^ " repo");

fun toStringList errs = join "\n" (List.map toString errs);

end
