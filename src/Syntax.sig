(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Syntax =
sig

type name = Name.name
type ty = Type.ty
type var = Var.var
type term = Term.term
type sequent = Sequent.sequent
type thm = Thm.thm

(* ------------------------------------------------------------------------- *)
(* Primitive                                                                 *)
(* ------------------------------------------------------------------------- *)

(* Type variables *)

val mk_type_var : name -> ty
val dest_type_var : ty -> name
val is_type_var : ty -> bool
val equal_type_var : name -> ty -> bool

val alpha : ty

(* Type operators *)

val mk_type_op : name * ty list -> ty
val dest_type_op : ty -> name * ty list
val is_type_op : ty -> bool

(* The type of booleans *)

val bool_ty : ty

(* Function types *)

val mk_fun : ty * ty -> ty
val dest_fun : ty -> ty * ty
val is_fun : ty -> bool
val list_mk_fun : ty list * ty -> ty
val strip_fun : ty -> ty list * ty

(* The type of individuals *)

val ind_ty : ty

(* Constants *)

val mk_const : name * ty -> term
val dest_const : term -> name * ty
val is_const : term -> bool

(* Variables *)

val mk_var : var -> term
val dest_var : term -> var
val is_var : term -> bool
val equal_var : var -> term -> bool

(* Function applications *)

val mk_comb : term * term -> term
val dest_comb : term -> term * term
val is_comb : term -> bool
val rator : term -> term
val rand : term -> term
val land : term -> term
val list_mk_comb : term * term list -> term
val strip_comb : term -> term * term list

(* Lambda abstractions *)

val mk_abs : var * term -> term
val dest_abs : term -> var * term
val is_abs : term -> bool
val list_mk_abs : var list * term -> term
val strip_abs : term -> var list * term

(* Equality *)

val eq_ty : ty -> ty
val eq_tm : term
val mk_eq : term * term -> term
val dest_eq : term -> term * term
val is_eq : term -> bool
val lhs : term -> term
val rhs : term -> term

(* Hilbert's indefinite choice operator (epsilon) *)

val select_ty : ty -> ty
val select_tm : term
val mk_select : var * term -> term
val dest_select : term -> var * term
val is_select : term -> bool

(* Unary operators *)

val mk_unop : name -> ty * term -> term
val dest_unop : name -> term -> ty * term
val is_unop : name -> term -> bool

(* Binary operators *)

val mk_binop : name -> ty * term * term -> term
val dest_binop : name -> term -> ty * term * term
val is_binop : name -> term -> bool

(* Theorems *)

val thm_id : thm -> int

val axioms : thm -> SequentSet.set

val hyp : thm -> TermAlphaSet.set

val concl : thm -> term

(* ------------------------------------------------------------------------- *)
(* Boolean                                                                   *)
(* ------------------------------------------------------------------------- *)

(* Negations *)

val mk_neg : term -> term
val dest_neg : term -> term
val is_neg : term -> bool

(* Implications *)

val mk_imp : term * term -> term
val dest_imp : term -> term * term
val is_imp : term -> bool

(* Universal quantifiers *)

val mk_forall : var * term -> term
val dest_forall : term -> var * term
val is_forall : term -> bool
val list_mk_forall : var list * term -> term
val strip_forall : term -> var list * term

(* Existential quantifiers *)

val mk_exists : var * term -> term
val dest_exists : term -> var * term
val is_exists : term -> bool
val list_mk_exists : var list * term -> term
val strip_exists : term -> var list * term

(* Unique existential quantifiers *)

val mk_exists_unique : var * term -> term
val dest_exists_unique : term -> var * term
val is_exists_unique : term -> bool
val list_mk_exists_unique : var list * term -> term
val strip_exists_unique : term -> var list * term

(* ------------------------------------------------------------------------- *)
(* Pretty printing                                                           *)
(* ------------------------------------------------------------------------- *)

val SHOW_TYPES : bool ref

val pp_type : Parser.ppstream -> ty -> unit
val type_to_string : ty -> string

val pp_term : Parser.ppstream -> term -> unit
val term_to_string : term -> string

val pp_subst : Parser.ppstream -> TermSubst.subst -> unit
val subst_to_string : TermSubst.subst -> string

val pp_sequent : Parser.ppstream -> sequent -> unit
val sequent_to_string : sequent -> string

val pp_thm : Parser.ppstream -> thm -> unit
val thm_to_string : thm -> string

end
