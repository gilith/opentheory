(* ========================================================================= *)
(* REWRITING OPENTHEORY OBJECTS                                              *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectRewrite =
sig

(* ------------------------------------------------------------------------- *)
(* A type of parameters for rewriting objects.                               *)
(* ------------------------------------------------------------------------- *)

type parameters =
     {apply : Object.object' -> Object.object option,
      savable : bool}

(* ------------------------------------------------------------------------- *)
(* Bottom-up object rewrites: return NONE for unchanged.                     *)
(* ------------------------------------------------------------------------- *)

type rewrite

val new : parameters -> rewrite

val id : rewrite

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

val sharingRewriteObject :
    Object.object -> rewrite -> Object.object option * rewrite

val rewriteObject : rewrite -> Object.object -> Object.object option

end
