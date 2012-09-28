(* ========================================================================= *)
(* EXTRA PACKAGE FILES                                                       *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageExtra :> PackageExtra =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of extra package files.                                            *)
(* ------------------------------------------------------------------------- *)

datatype extra' =
    Extra of
      {name : PackageName.name,
       filename : string};

type extra = extra';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk ext' : extra = ext';

fun dest ext : extra' = ext;

fun name' (Extra {name = x, ...}) = x;

fun filename' (Extra {filename = x, ...}) = {filename = x};

fun name ext = name' (dest ext);

fun filename ext = filename' (dest ext);

(* ------------------------------------------------------------------------- *)
(* Remove the directory from the filename path.                              *)
(* ------------------------------------------------------------------------- *)

fun normalize ext =
    let
      val Extra {name,filename} = dest ext

      val filename = OS.Path.file filename
    in
      mk (Extra {name = name, filename = filename})
    end;

end
