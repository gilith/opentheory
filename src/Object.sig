(* ========================================================================= *)
(* OPENTHEORY OBJECTS                                                        *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Object =
sig

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory objects.                                             *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Error
  | Int of int
  | Name of Name.name
  | TypeOp of TypeOp.typeOp
  | Type of Type.ty
  | Const of Const.const
  | Var of Var.var
  | Term of Term.term
  | Thm of Thm.thm
  | List of object list
  | Call of Name.name

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* Error objects *)

val destOerror : object -> unit

val isOerror : object -> bool

(* Int objects *)

val destOint : object -> int

val isOint : object -> bool

(* Name objects *)

val destOname : object -> Name.name

val isOname : object -> bool

(* List objects *)

val destOlist : object -> object list

val isOlist : object -> bool

(* Nil list objects *)

val onil : object

val mkOnil : unit -> object

val isOnil : object -> bool

(* Cons list objects *)

val mkOcons : object * object -> object

val destOcons : object -> object * object

val isOcons : object -> bool

(* Unit objects *)

val ounit : object

val mkOunit : unit -> object

val isOunit : object -> bool

(* Pair objects *)

val mkOpair : object * object -> object

val destOpair : object -> object * object

val isOpair : object -> bool

(* Triple objects *)

val mkOtriple : object * object * object -> object

val destOtriple : object -> object * object * object

val isOtriple : object -> bool

(* Type objects *)

val destOtype : object -> Type.ty

val isOtype : object -> bool

(* Type list objects *)

val mkOtypes : Type.ty list -> object

val destOtypes : object -> Type.ty list

val isOtypes : object -> bool

(* Type variable type objects *)

val mkOtypeVar : object -> object

val destOtypeVar : object -> object

val isOtypeVar : object -> bool

(* Type operator type objects *)

val mkOtypeOp : TypeOp.typeOp * object -> object

val destOtypeOp : object -> TypeOp.typeOp * object

val isOtypeOp : object -> bool

(* Term variable objects *)

val mkOvar : Var.var -> object

val destOvar : object -> Var.var

val isOvar : object -> bool

(* Term objects *)

val destOterm : object -> Term.term

val isOterm : object -> bool

(* Term list objects *)

val mkOterms : Term.term list -> object

val destOterms : object -> Term.term list

val isOterms : object -> bool

(* Term variable term objects *)

val mkOtermVar : object * object -> object

val destOtermVar : object -> object * object

val isOtermVar : object -> bool

(* Constant term objects *)

val mkOtermConst : Const.const * object -> object

val destOtermConst : object -> Const.const * object

val isOtermConst : object -> bool

(* Function application term objects *)

val mkOtermApp : object * object -> object

val destOtermApp : object -> object * object

val isOtermApp : object -> bool

(* Lambda abstraction term objects *)

val mkOtermAbs : object * object -> object

val destOtermAbs : object -> object * object

val isOtermAbs : object -> bool

(* Sequent objects *)

val mkOseq : Sequent.sequent -> object * object

val destOseq : object * object -> Sequent.sequent

val isOseq : object * object -> bool

(* Theorem objects *)

val destOthm : object -> Thm.thm

val isOthm : object -> bool

(* Function call objects *)

val destOcall : object -> Name.name

val isOcall : object -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : object * object -> order

(* ------------------------------------------------------------------------- *)
(* Extracting the theorems stored in an object.                              *)
(* ------------------------------------------------------------------------- *)

val thms : object -> Thm.thm list

(* ------------------------------------------------------------------------- *)
(* Extracting the symbols in an object.                                      *)
(* ------------------------------------------------------------------------- *)

val symbol : object -> Symbol.symbol

val symbolAdd : Symbol.symbol -> object -> Symbol.symbol

val symbolAddList : Symbol.symbol -> object list -> Symbol.symbol

(* ------------------------------------------------------------------------- *)
(* Breaking down objects into commands.                                      *)
(* ------------------------------------------------------------------------- *)

val toCommand : object -> Command.command * object list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : object Print.pp

end
