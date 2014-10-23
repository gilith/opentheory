(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TERMS                                *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature TermSubst =
sig

(* ------------------------------------------------------------------------- *)
(* Term substitution maps.                                                   *)
(* ------------------------------------------------------------------------- *)

type substMap = Term.term VarMap.map

val emptyMap : substMap

val nullMap : substMap -> bool

val singletonMap : Var.var * Term.term -> substMap

val normalizeMap : substMap -> substMap

val fromListMap : (Var.var * Term.term) list -> substMap

(* ------------------------------------------------------------------------- *)
(* Capture-avoiding substitutions of type and term variables.                *)
(* ------------------------------------------------------------------------- *)

type subst

val empty : subst

val null : subst -> bool

val mk : TypeSubst.subst -> substMap -> subst

val mkMono : substMap -> subst

val dest : subst -> TypeSubst.subst * substMap

val typeSubst : subst -> TypeSubst.subst

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

(* Types *)

val sharingSubstType : Type.ty -> subst -> Type.ty option * subst

val substType : subst -> Type.ty -> Type.ty option

(* Variables *)

val sharingSubstVar : Var.var -> subst -> Var.var option * subst

val substVar : subst -> Var.var -> Var.var option

(* Terms *)

val sharingSubst : Term.term -> subst -> Term.term option * subst

val subst : subst -> Term.term -> Term.term option

(* Term sets *)

val sharingSubstAlphaSet :
    TermAlphaSet.set -> subst -> TermAlphaSet.set option * subst

val substAlphaSet : subst -> TermAlphaSet.set -> TermAlphaSet.set option

(* Term substitution maps *)

val sharingSubstSubstMap : substMap -> subst -> substMap option * subst

val substSubstMap : subst -> substMap -> substMap option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppMap : substMap Print.pp

val toStringMap : substMap -> string

val pp : subst Print.pp

val toString : subst -> string

end
