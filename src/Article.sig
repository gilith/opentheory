(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Article =
sig

(* ------------------------------------------------------------------------- *)
(* Translations                                                              *)
(* ------------------------------------------------------------------------- *)

type translation

val import_type : translation -> Name.name -> Name.name
val export_type : translation -> Name.name -> Name.name

val import_const : translation -> Name.name -> Name.name
val export_const : translation -> Name.name -> Name.name

val import_rule : translation -> Name.name -> Name.name
val export_rule : translation -> Name.name -> Name.name

val natural : translation

val hol_light : translation ref

(* ------------------------------------------------------------------------- *)
(* Articles                                                                  *)
(* ------------------------------------------------------------------------- *)

type article

(* ------------------------------------------------------------------------- *)
(* I/O                                                                       *)
(* ------------------------------------------------------------------------- *)

val from_textfile :
    {filename : string, translation : translation} -> Thm.thm list * article

(***
val to_textfile :
    {filename : string, translation : translation, article : article} -> unit
***)

end
