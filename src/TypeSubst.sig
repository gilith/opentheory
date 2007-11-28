(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TYPES                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature TypeSubst =
sig

(* ------------------------------------------------------------------------- *)
(* Substitutions                                                             *)
(* ------------------------------------------------------------------------- *)

type subst

val empty : subst

val null : subst -> bool

val add : (Name.name * Type.ty) -> subst -> subst

val peek : subst -> Name.name -> Type.ty option

val norm : subst -> subst

val subst : subst -> Type.ty -> Type.ty

val to_list : subst -> (Name.name * Type.ty) list

(* ------------------------------------------------------------------------- *)
(* Matching                                                                  *)
(* ------------------------------------------------------------------------- *)

val match_list' : subst -> (Type.ty * Type.ty) list -> subst

val match_list : (Type.ty * Type.ty) list -> subst

val match' : subst -> Type.ty -> Type.ty -> subst

val match : Type.ty -> Type.ty -> subst

end
