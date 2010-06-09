(* ========================================================================= *)
(* THEORY PACKAGE META-DATA                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageInfo :> PackageInfo =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory package meta-data.                                       *)
(* ------------------------------------------------------------------------- *)

datatype info =
    Info of
      {name : PackageName.name,
       directory : string,
       filename : string};

(* ------------------------------------------------------------------------- *)
(* Using the meta-data to read the package.                                  *)
(* ------------------------------------------------------------------------- *)

fun toPackage info =
    let
      val Info {name = _, directory, filename} = info

      val file = OS.Path.joinDirFile {dir = directory, file = filename}
    in
      Package.fromTextFile {filename = file}
    end;

end
