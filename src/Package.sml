(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE DATA                                    *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Package :> Package =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Types of theory package data.                                             *)
(* ------------------------------------------------------------------------- *)

datatype package =
    Package of
      {name : PackageName.name option,
       directory : string,
       filename : string,
       contents : PackageContents.contents};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun name (Package {name = x, ...}) = x;

fun directory (Package {directory = x, ...}) = {directory = x};

fun filename (Package {filename = x, ...}) = {filename = x};

fun contents (Package {contents = x, ...}) = x;

fun tags p = PackageContents.tags (contents p);

fun requires p = PackageContents.requires (contents p);

fun theory p = PackageContents.theory (contents p);

fun filenames p = filename p :: Theory.filenames (theory p);

fun dependencies p = PackageContents.dependencies (contents p);

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun fromTextFile {name,directory,filename} =
    let
      val file = OS.Path.joinDirFile {dir = directory, file = filename}

      val contents = PackageContents.fromTextFile {filename = file}
    in
      Package
        {name = name,
         directory = directory,
         filename = filename,
         contents = contents}
    end;

end
