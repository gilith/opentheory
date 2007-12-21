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

val importType : translation -> Name.name -> Name.name
val exportType : translation -> Name.name -> Name.name

val importConst : translation -> Name.name -> Name.name
val exportConst : translation -> Name.name -> Name.name

val importRule : translation -> Name.name -> Name.name
val exportRule : translation -> Name.name -> Name.name

val natural : translation

val holLight : translation

(* ------------------------------------------------------------------------- *)
(* Articles                                                                  *)
(* ------------------------------------------------------------------------- *)

type article

(* ------------------------------------------------------------------------- *)
(* I/O                                                                       *)
(* ------------------------------------------------------------------------- *)

val fromTextfile :
    {filename : string, translation : translation} -> Thm.thm list * article

(***
val toTextfile :
    {filename : string, translation : translation, article : article} -> unit
***)

end
