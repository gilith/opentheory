(* ========================================================================= *)
(* THEORY PACKAGE META-DATA                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageInfo :> PackageInfo =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val theoryExtension = "thy";

(* ------------------------------------------------------------------------- *)
(* Directories and filenames.                                                *)
(* ------------------------------------------------------------------------- *)

fun mkTheoryFilename pkg =
    let
      val base = PackageName.base pkg
    in
      OS.Path.joinBaseExt
        {base = PackageBase.toString base, ext = SOME theoryExtension}
    end;

(* ------------------------------------------------------------------------- *)
(* A type of theory package meta-data.                                       *)
(* ------------------------------------------------------------------------- *)

datatype info =
    Info of
      {name : PackageName.name,
       directory : string,
       package : Package.package option ref};

fun mk {name,directory} =
    let
      val package = ref NONE
    in
      Info
        {name = name,
         directory = directory,
         package = package}
    end;

fun name (Info {name = x, ...}) = x;

fun directory (Info {directory = x, ...}) = {directory = x};

fun theoryFilename info =
    let
      val {directory = dir} = directory info

      val file = mkTheoryFilename (name info)

      val filename = OS.Path.joinDirFile {dir = dir, file = file}
    in
      {filename = filename}
    end;

(* ------------------------------------------------------------------------- *)
(* Read the package.                                                         *)
(* ------------------------------------------------------------------------- *)

fun package info =
    let
      val Info {package = pkg, ...} = info
    in
      case !pkg of
        SOME p => p
      | NONE =>
        let
          val filename = theoryFilename info

          val p = Package.fromTextFile filename

          val () = pkg := SOME p
        in
          p
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Is the package installed?                                                 *)
(* ------------------------------------------------------------------------- *)

fun installed info =
    let
      val _ = package info
    in
      true
    end
    handle IO.Io _ => false;

end
