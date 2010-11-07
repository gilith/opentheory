(* ========================================================================= *)
(* REWRITING HIGHER ORDER LOGIC TERMS                                        *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature TermRewrite =
sig

(* ------------------------------------------------------------------------- *)
(* Bottom-up term rewrites: return NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

type rewrite

val new : TypeRewrite.rewrite -> (Term.term' -> Term.term option) -> rewrite

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

(* Types *)

val sharingRewriteType : Type.ty -> rewrite -> Type.ty option * rewrite

val rewriteType : rewrite -> Type.ty -> Type.ty option

(* Variables *)

val sharingRewriteVar : Var.var -> rewrite -> Var.var option * rewrite

val rewriteVar : rewrite -> Var.var -> Var.var option

(* Terms *)

val sharingRewrite : Term.term -> rewrite -> Term.term option * rewrite

val rewrite : rewrite -> Term.term -> Term.term option

(* Term sets *)

val sharingRewriteAlphaSet :
    TermAlphaSet.set -> rewrite -> TermAlphaSet.set option * rewrite

val rewriteAlphaSet : rewrite -> TermAlphaSet.set -> TermAlphaSet.set option

end
