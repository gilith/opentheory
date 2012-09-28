(* ========================================================================= *)
(* REWRITING HIGHER ORDER LOGIC TERMS                                        *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature TermRewrite =
sig

(* ------------------------------------------------------------------------- *)
(* Bottom-up term rewrites: return NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

type rewrite

val new : TypeRewrite.rewrite -> (Term.term' -> Term.term option) -> rewrite

val id : rewrite

val undef : rewrite  (* Remove definitions from type operators and constants *)

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

(* Lists *)

val sharingRewriteList :
    ('a -> rewrite -> 'a option * rewrite) ->
    'a list -> rewrite -> 'a list option * rewrite

val rewriteList :
    ('a -> rewrite -> 'a option * rewrite) ->
    rewrite -> 'a list -> 'a list option

(* Types *)

val sharingRewriteType : Type.ty -> rewrite -> Type.ty option * rewrite

val rewriteType : rewrite -> Type.ty -> Type.ty option

(* Variables *)

val sharingRewriteVar : Var.var -> rewrite -> Var.var option * rewrite

val rewriteVar : rewrite -> Var.var -> Var.var option

(* Terms *)

val sharingRewriteTerm : Term.term -> rewrite -> Term.term option * rewrite

val rewriteTerm : rewrite -> Term.term -> Term.term option

(* Term lists *)

val sharingRewriteTermList :
    Term.term list -> rewrite -> Term.term list option * rewrite

val rewriteTermList : rewrite -> Term.term list -> Term.term list option

(* Term sets *)

val sharingRewriteTermAlphaSet :
    TermAlphaSet.set -> rewrite -> TermAlphaSet.set option * rewrite

val rewriteTermAlphaSet :
    rewrite -> TermAlphaSet.set -> TermAlphaSet.set option

end
