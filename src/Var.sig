(* ========================================================================= *)
(* HIGHER ORDER LOGIC VARIABLES                                              *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Var =
sig

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic term variables.                              *)
(* ------------------------------------------------------------------------- *)

type var = TypeTerm.var

val mk : Name.name * Type.ty -> var

val dest : var -> Name.name * Type.ty

(* ------------------------------------------------------------------------- *)
(* The name of a variable.                                                   *)
(* ------------------------------------------------------------------------- *)

val name : var -> Name.name

(* ------------------------------------------------------------------------- *)
(* The type of a variable.                                                   *)
(* ------------------------------------------------------------------------- *)

val typeOf : var -> Type.ty

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : var * var -> order

val equal : var -> var -> bool

val equalList : var list -> var list -> bool

val checkEqual : (TypeTerm.term -> TypeTerm.term -> unit) -> var -> var -> unit

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

val addSharingTypeVars : var -> Type.sharingTypeVars -> Type.sharingTypeVars

val typeVars : var -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

val addSharingTypeOps : var -> Type.sharingTypeOps -> Type.sharingTypeOps

val typeOps : var -> TypeOpSet.set

(* ------------------------------------------------------------------------- *)
(* Fresh variables.                                                          *)
(* ------------------------------------------------------------------------- *)

val renameAvoiding : NameSet.set -> var -> var

(* ------------------------------------------------------------------------- *)
(* Type substitutions.                                                       *)
(* ------------------------------------------------------------------------- *)

val sharingSubst : var -> TypeSubst.subst -> var option * TypeSubst.subst

val subst : TypeSubst.subst -> var -> var option

(* ------------------------------------------------------------------------- *)
(* Type rewrites.                                                            *)
(* ------------------------------------------------------------------------- *)

val sharingRewrite :
    var -> TypeRewrite.rewrite -> var option * TypeRewrite.rewrite

val rewrite : TypeRewrite.rewrite -> var -> var option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : var Print.pp

val toString : var -> string

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val toHtml : Show.show -> var -> Html.inline list

end
