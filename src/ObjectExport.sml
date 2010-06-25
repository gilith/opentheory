(* ========================================================================= *)
(* EXPORTED THEOREM OBJECTS                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectExport :> ObjectExport =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of exported theorem objects.                                       *)
(* ------------------------------------------------------------------------- *)

datatype export = Export of Thm.thm ObjectProvMap.map;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty = Export (ObjectProvMap.new ());

fun null (Export m) = ObjectProvMap.null m;

fun size (Export m) = ObjectProvMap.size m;

fun insert (Export m) obj_th = Export (ObjectProvMap.insert m obj_th);

fun toMap (Export m) = m;

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

fun compress exp = raise Bug "ObjectExport.compress: not implemented";

end
