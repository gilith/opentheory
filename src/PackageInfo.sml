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

val packageFileExtension = "thy";

(* ------------------------------------------------------------------------- *)
(* Directories and filenames.                                                *)
(* ------------------------------------------------------------------------- *)

fun mkPackageFile pkg =
    let
      val base = PackageName.base pkg
    in
      OS.Path.joinBaseExt
        {base = PackageBase.toString base,
         ext = SOME packageFileExtension}
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

(* ------------------------------------------------------------------------- *)
(* Package directory operations.                                             *)
(* ------------------------------------------------------------------------- *)

fun joinDirectory info =
    let
      val {directory = dir} = directory info
    in
      fn {filename} => {filename = OS.Path.concat (dir,filename)}
    end;

fun existsDirectory info =
    let
      val {directory = dir} = directory info
    in
      OS.FileSys.isDir dir
      handle OS.SysErr _ => false
    end;

fun createDirectory info =
    let
      val {directory = dir} = directory info
    in
      OS.FileSys.mkDir dir
    end;

local
  fun delete {filename} = OS.FileSys.remove filename
in
  fun nukeDirectory info =
      let
        val Info {directory = dir, package = pkg, ...} = info

        val filenames = readDirectory {directory = dir}

        val () = app delete filenames

        val () = pkg := NONE

        val () = OS.FileSys.rmDir dir
      in
        ()
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Is the package installed?                                                 *)
(* ------------------------------------------------------------------------- *)

fun isInstalled info = existsDirectory info;

(* ------------------------------------------------------------------------- *)
(* The package theory file.                                                  *)
(* ------------------------------------------------------------------------- *)

fun packageFile info =
    let
      val file = mkPackageFile (name info)
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
          val filename = packageFile info

          val p = Package.fromTextFile filename

          val () = pkg := SOME p
        in
          p
        end
    end;

(* ------------------------------------------------------------------------- *)
(* The files needed by the package.                                          *)
(* ------------------------------------------------------------------------- *)

fun articles info =
    let
      val pkg = package info
    in
      map (joinDirectory info) (Package.articles pkg)
    end;

fun extraFiles info =
    let
      fun join {name,filename} =
          let
            val {filename} = joinDirectory info {filename = filename}
          in
            {name = name, filename = filename}
          end

      val pkg = package info
    in
      map join (Package.extraFiles pkg)
    end;

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun packages info =
    let
      val pkg = package info
    in
      Package.packages pkg
    end;

end
