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

val onil : object
val mkOnil : unit -> object
val isOnil : object -> bool

val mkOcons : object * object -> object
val destOcons : object -> object * object
val isOcons : object -> bool

val ounit : object
val mkOunit : unit -> object
val isOunit : object -> bool

val mkOpair : object * object -> object
val destOpair : object -> object * object
val isOpair : object -> bool

val destOtriple : object -> object * object * object
val isOtriple : object -> bool

val destOtype : object -> Type.ty
val isOtype : object -> bool

val mkOtypes : Type.ty list -> object
val destOtypes : object -> Type.ty list
val isOtypes : object -> bool

val mkOtypeVar : object -> object
val destOtypeVar : object -> object
val isOtypeVar : object -> bool

val mkOtypeOp : object * object -> object
val destOtypeOp : object -> object * object
val isOtypeOp : object -> bool

val mkOvar : Var.var -> object
val destOvar : object -> Var.var
val isOvar : object -> bool

val destOterm : object -> Term.term
val isOterm : object -> bool

val mkOterms : Term.term list -> object
val destOterms : object -> Term.term list
val isOterms : object -> bool

val mkOtermVar : object * object -> object
val destOtermVar : object -> object * object
val isOtermVar : object -> bool

val mkOtermConst : object * object -> object
val destOtermConst : object -> object * object
val isOtermConst : object -> bool

val mkOtermComb : object * object -> object
val destOtermComb : object -> object * object
val isOtermComb : object -> bool

val mkOtermAbs : object * object -> object
val destOtermAbs : object -> object * object
val isOtermAbs : object -> bool

val destOthm : object -> Thm.thm
val isOthm : object -> bool

val destOcall : object -> Name.name
val isOcall : object -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : object * object -> order

(* ------------------------------------------------------------------------- *)
(* Lifting interpretations to Oname objects.                                 *)
(* ------------------------------------------------------------------------- *)

val interpretType : Interpretation.interpretation -> object -> object

val interpretConst : Interpretation.interpretation -> object -> object

val interpretRule : Interpretation.interpretation -> object -> object

(* ------------------------------------------------------------------------- *)
(* Extracting the theorems stored in an object.                              *)
(* ------------------------------------------------------------------------- *)

val thms : object -> Thm.thm list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : object Print.pp

end
