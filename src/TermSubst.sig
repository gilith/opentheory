(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TERMS                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature TermSubst =
sig

(* ------------------------------------------------------------------------- *)
(* Capture-avoiding substitutions of type and term variables                 *)
(* ------------------------------------------------------------------------- *)

type subst

val empty : subst

val singleton_type : Name.name * Type.ty -> subst
val singleton : Var.var * Term.term -> subst

val from_list_type : (Name.name * Type.ty) list -> subst
val from_list : (Var.var * Term.term) list -> subst

val null : subst -> bool

val add_type : Name.name * Type.ty -> subst -> subst
val add : Var.var * Term.term -> subst -> subst

val add_list_type : (Name.name * Type.ty) list -> subst -> subst
val add_list : (Var.var * Term.term) list -> subst -> subst

val peek_type : subst -> Name.name -> Type.ty option
val peek : subst -> Var.var -> Term.term option

val norm : subst -> subst  (* Removes identity substitutions v |-> v *)

val subst_type : subst -> Type.ty -> Type.ty
val subst : subst -> Term.term -> Term.term

val to_list_type : subst -> (Name.name * Type.ty) list
val to_list : subst -> (Var.var * Term.term) list

end
