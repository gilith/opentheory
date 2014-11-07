(* ========================================================================= *)
(* THEOREM METADATA                                                          *)
(* Copyright (c) 2014 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Metadata =
sig

(* ------------------------------------------------------------------------- *)
(* A type of metadata.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype metadata = Metadata of metadata NameMap.map

val empty : metadata

val null : metadata -> bool

val singleton : Name.name * metadata -> metadata

val add : metadata -> Name.name * metadata -> metadata

val union : metadata -> metadata -> metadata

val toList : metadata -> (Name.name * metadata) list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : metadata Print.pp

end
