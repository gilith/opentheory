(* ========================================================================= *)
(* OPENTHEORY OBJECTS                                                        *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Object =
sig

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory objects.                                             *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Oerror
  | Onum of int
  | Oname of Name.name
  | Olist of object list
  | Otype of Type.ty
  | Oterm of Term.term
  | Othm of Thm.thm
  | Ocall of Name.name

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val destOerror : object -> unit
val isOerror : object -> bool

val destOnum : object -> int
val isOnum : object -> bool

val destOname : object -> Name.name
val isOname : object -> bool

val destOlist : object -> object list
val isOlist : object -> bool

val destOtype : object -> Type.ty
val isOtype : object -> bool

val destOterm : object -> Term.term
val isOterm : object -> bool

val destOthm : object -> Thm.thm
val isOthm : object -> bool

val destOcall : object -> Name.name
val isOcall : object -> bool

val mkOunit : unit -> object

val mkOpair : object * object -> object
val destOpair : object -> object * object
val isOpair : object -> bool

val destOtriple : object -> object * object * object
val isOtriple : object -> bool

val destOvar : object -> Name.name * Type.ty
val isOvar : object -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : object * object -> order

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : object Parser.pp

(* ------------------------------------------------------------------------- *)
(* Extracting the theorems stored in an object.                              *)
(* ------------------------------------------------------------------------- *)

val thms : object -> Thm.thm list

end
