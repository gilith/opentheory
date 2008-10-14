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

val singletonType : Name.name * Type.ty -> subst
val singleton : Var.var * Term.term -> subst

val fromListType : (Name.name * Type.ty) list -> subst
val fromList : (Var.var * Term.term) list -> subst

val null : subst -> bool

val addType : Name.name * Type.ty -> subst -> subst
val add : Var.var * Term.term -> subst -> subst

val addListType : (Name.name * Type.ty) list -> subst -> subst
val addList : (Var.var * Term.term) list -> subst -> subst

val peekType : subst -> Name.name -> Type.ty option
val peek : subst -> Var.var -> Term.term option

val toListType : subst -> (Name.name * Type.ty) list
val toList : subst -> (Var.var * Term.term) list

(* ------------------------------------------------------------------------- *)
(* Normalization removes identity substitutions v |-> v.                     *)
(* ------------------------------------------------------------------------- *)

val norm : subst -> subst

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

val substType : subst -> Type.ty -> Type.ty option
val subst : subst -> Term.term -> Term.term option

end
