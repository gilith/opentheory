(* ========================================================================= *)
(* REWRITING OPENTHEORY OBJECTS                                              *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectRewrite =
sig

(* ------------------------------------------------------------------------- *)
(* Bottom-up object rewrites: return NONE for unchanged.                     *)
(* ------------------------------------------------------------------------- *)

type rewrite

val new :
    TermRewrite.rewrite -> (Object.object' -> Object.object option) -> rewrite

val id : rewrite

val sharingRewriteObject :
    Object.object -> rewrite -> Object.object option * rewrite

val rewriteObject : rewrite -> Object.object -> Object.object option

end
