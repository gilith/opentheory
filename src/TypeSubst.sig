(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TYPES                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature TypeSubst =
sig

(* ------------------------------------------------------------------------- *)
(* Type substitutions.                                                       *)
(* ------------------------------------------------------------------------- *)

type subst

val empty : subst

val null : subst -> bool

val add : (Name.name * Type.ty) -> subst -> subst

val peek : subst -> Name.name -> Type.ty option

val toList : subst -> (Name.name * Type.ty) list

(* ------------------------------------------------------------------------- *)
(* Normalization removes identity substitutions v |-> v.                     *)
(* ------------------------------------------------------------------------- *)

val norm : subst -> subst

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

type sharingSubst

val newSharingSubst : subst -> sharingSubst

val sharingSubst : Type.ty -> sharingSubst -> Type.ty option * sharingSubst

val subst : subst -> Type.ty -> Type.ty option

(* ------------------------------------------------------------------------- *)
(* Matching.                                                                 *)
(* ------------------------------------------------------------------------- *)

val matchList' : subst -> (Type.ty * Type.ty) list -> subst

val matchList : (Type.ty * Type.ty) list -> subst

val match' : subst -> Type.ty -> Type.ty -> subst

val match : Type.ty -> Type.ty -> subst

end
