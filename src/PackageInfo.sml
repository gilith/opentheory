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

fun mkTheoryFile pkg =
    let
      val base = PackageName.base pkg
    in
      OS.Path.joinBaseExt
        {base = PackageBase.toString base,
         ext = SOME theoryExtension}
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

fun joinDirectory info =
    let
      val {directory = dir} = directory info
    in
      fn {filename} => {filename = OS.Path.concat (dir,filename)}
    end;

(* ------------------------------------------------------------------------- *)
(* The package theory file.                                                  *)
(* ------------------------------------------------------------------------- *)

fun theoryFile info =
    let
      val file = mkTheoryFile (name info)
    in
      joinDirectory info {filename = file}
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
          val filename = theoryFile info

          val p = Package.fromTextFile filename

          val () = pkg := SOME p
        in
          p
        end
    end;

(* ------------------------------------------------------------------------- *)
(* The files needed by the package.                                          *)
(* ------------------------------------------------------------------------- *)

fun files info =
    let
      val pkg = package info
    in
      theoryFile info :: map (joinDirectory info) (Package.files pkg)
    end;

(* ------------------------------------------------------------------------- *)
(* Is the package properly installed?                                        *)
(* ------------------------------------------------------------------------- *)

fun installed info =
    let
      val _ = package info
    in
      true
    end
    handle IO.Io _ => false;

end
