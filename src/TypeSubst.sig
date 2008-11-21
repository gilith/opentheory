(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TYPES                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature TypeSubst =
sig

(* ------------------------------------------------------------------------- *)
(* Type substitution maps.                                                   *)
(* ------------------------------------------------------------------------- *)

type substMap = Type.ty NameMap.map

val emptyMap : substMap

val fromListMap : (Name.name * Type.ty) list -> substMap

(* ------------------------------------------------------------------------- *)
(* Type substitutions.                                                       *)
(* ------------------------------------------------------------------------- *)

type subst

val empty : subst

val null : subst -> bool

val mk : substMap -> subst

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

val sharingSubst : Type.ty -> subst -> Type.ty option * subst

val subst : subst -> Type.ty -> Type.ty option

(* ------------------------------------------------------------------------- *)
(* Matching.                                                                 *)
(* ------------------------------------------------------------------------- *)

val matchList' : substMap -> (Type.ty * Type.ty) list -> substMap

val match' : substMap -> Type.ty -> Type.ty -> substMap

val matchList : (Type.ty * Type.ty) list -> subst

val match : Type.ty -> Type.ty -> subst

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppMap : substMap Print.pp

val toStringMap : substMap -> string

val pp : subst Print.pp

val toString : subst -> string

end
