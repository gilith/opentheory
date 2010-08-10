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
    AlreadyInstalled
  | FilenameClash of
      {srcs : {name : string, filename : string option} list,
       dest : {filename : string}}
  | InstalledDescendent of PackageName.name
  | NotInstalled
  | UninstalledParent of PackageName.name;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun isAlreadyInstalled err =
    case err of
      AlreadyInstalled => true
    | _ => false;

fun removeAlreadyInstalled errs =
    let
      val (xs,errs) = List.partition isAlreadyInstalled errs

      val removed = not (null xs)
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
      AlreadyInstalled => true
    | FilenameClash _ => true
    | InstalledDescendent _ => true
    | NotInstalled => true
    | UninstalledParent _ => true;

val existsFatal = List.exists isFatal;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun toString err =
    (if isFatal err then "Error" else "Warning") ^ ": " ^
    (case err of
       AlreadyInstalled =>
       "package is already installed"
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
         " is target for " ^ join "\n  and also for " (map toStringSrc srcs)
       end
     | InstalledDescendent name =>
       "in use by installed package: " ^ PackageName.toString name
     | NotInstalled =>
       "package is not installed"
     | UninstalledParent name =>
       "depends on uninstalled package: " ^ PackageName.toString name);

fun toStringList errs = join "\n" (map toString errs);

end
