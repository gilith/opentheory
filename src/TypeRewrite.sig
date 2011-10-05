(* ========================================================================= *)
(* REWRITING HIGHER ORDER LOGIC TYPES                                        *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature TypeRewrite =
sig

(* ------------------------------------------------------------------------- *)
(* Bottom-up type rewrites: return NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

type rewrite

val new : (Type.ty' -> Type.ty option) -> rewrite

val id : rewrite

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

(* Types *)

val sharingRewriteType : Type.ty -> rewrite -> Type.ty option * rewrite

val rewriteType : rewrite -> Type.ty -> Type.ty option

(* Type lists *)

val sharingRewriteTypeList :
    Type.ty list -> rewrite -> Type.ty list option * rewrite

val rewriteTypeList : rewrite -> Type.ty list -> Type.ty list option

end
