(* ========================================================================= *)
(* SYMBOLS CONTAINED IN A SET OF THEOREM OBJECTS                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectThms =
sig

(* ------------------------------------------------------------------------- *)
(* A type of object set theorems.                                            *)
(* ------------------------------------------------------------------------- *)

type thms

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : thms

val thms : thms -> Thms.thms

(* ------------------------------------------------------------------------- *)
(* Looking up symbols and theorems.                                          *)
(* ------------------------------------------------------------------------- *)

val peekThm : thms -> Sequent.sequent -> ObjectProv.object option

val peekTypeOp : thms -> Name.name -> ObjectProv.object option

val peekConst : thms -> Name.name -> ObjectProv.object option

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

val union : thms -> thms -> thms

val unionList : thms list -> thms

(* ------------------------------------------------------------------------- *)
(* I/O.                                                                      *)
(* ------------------------------------------------------------------------- *)

val toExport : thms -> ObjectExport.export

val fromExport : ObjectExport.export -> thms

end
