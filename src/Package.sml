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
       contents : PackageContents.contents};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun name (Package {name = x, ...}) = x;

fun directory (Package {directory = x, ...}) = {directory = x};

fun contents (Package {contents = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun fromTextFile {name,filename} =
    let
      val directory = OS.Path.dir filename

      val contents = PackageContents.fromTextFile {filename = filename}
    in
      Package
        {name = name,
         directory = directory,
         contents = contents}
    end;

end
