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

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

val sharingRewrite : Type.ty -> rewrite -> Type.ty option * rewrite

val rewrite : rewrite -> Type.ty -> Type.ty option

end
