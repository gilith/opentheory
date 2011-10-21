(* ========================================================================= *)
(* REWRITING OPENTHEORY OBJECTS                                              *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectRewrite =
sig

(* ------------------------------------------------------------------------- *)
(* Bottom-up object rewrites: return NONE for unchanged.                     *)
(* ------------------------------------------------------------------------- *)

type rewrite

val new : (ObjectProv.object' -> ObjectProv.object option) -> rewrite

val id : rewrite

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

val sharingRewriteObject :
    ObjectProv.object -> rewrite -> ObjectProv.object option * rewrite

val rewriteObject : rewrite -> ObjectProv.object -> ObjectProv.object option

end
