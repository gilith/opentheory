(* ========================================================================= *)
(* REWRITING OPENTHEORY OBJECTS                                              *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectRewrite :> ObjectRewrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val unwantedString = "Unwanted"
and unwantedIdString = "id";

(* ------------------------------------------------------------------------- *)
(* Unwanted constants.                                                       *)
(* ------------------------------------------------------------------------- *)

val unwantedNamespace = Namespace.fromList [unwantedString];

val unwantedIdName = Name.mk (unwantedNamespace,unwantedIdString);

fun destUnwantedIdConst c =
    let
      val n = Const.name c

(*OpenTheoryTrace2
      val () = Print.trace Name.pp "ObjectRewrite.destUnwantedIdConst.n" n
*)
    in
      if Name.equal n unwantedIdName then ()
      else raise Error "ObjectRewrite.destUnwantedIdConst"
    end;

fun destUnwantedIdTerm tm =
    let
      val (c,ty) = Term.destConst tm

      val () = destUnwantedIdConst c
    in
      ty
    end;

fun destUnwantedIdRefl th =
    destUnwantedIdTerm (Term.destRefl (Thm.concl th));

fun destUnwantedIdTermObject ob =
    destUnwantedIdTerm (Object.destTerm ob);

fun destUnwantedIdReflObject ob =
    destUnwantedIdRefl (Object.destThm ob);

fun destUnwantedIdTermObjectProv obj =
    destUnwantedIdTermObject (ObjectProv.object obj);

fun destUnwantedIdReflObjectProv obj =
    destUnwantedIdReflObject (ObjectProv.object obj);

(* ------------------------------------------------------------------------- *)
(* A type of object rewrites.                                                *)
(* ------------------------------------------------------------------------- *)

type rewrite = ObjectProv.object -> ObjectProv.object;

(* ------------------------------------------------------------------------- *)
(* An unchanged exception.                                                   *)
(* ------------------------------------------------------------------------- *)

exception Unchanged;

(* ------------------------------------------------------------------------- *)
(* Applying rewrites to objects.                                             *)
(* ------------------------------------------------------------------------- *)

fun apply rewr obj = rewr obj handle Unchanged => obj;

(* ------------------------------------------------------------------------- *)
(* Rewrite combinators.                                                      *)
(* ------------------------------------------------------------------------- *)

val id : rewrite = fn _ => raise Unchanged;

val fail : rewrite = fn _ => raise Error "ObjectRewrite.fail";

fun seq r1 r2 obj = apply r2 (r1 obj) handle Unchanged => r2 obj;

fun cond r1 r2 obj = r1 obj handle Error _ => r2 obj;

fun try r = cond r id;

(* ------------------------------------------------------------------------- *)
(* Eliminating Unwanted.id terms.                                            *)
(* ------------------------------------------------------------------------- *)

fun unwantedId obj =
    let
      val ObjectProv.Object' {object = ob, provenance = prov} =
          ObjectProv.dest obj

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectRewrite.unwantedId.ob" ob

      val () = Print.trace ObjectProv.ppProvenance
                 "ObjectRewrite.unwantedId.prov" prov
*)
    in
      case prov of
        ObjectProv.Default =>
        let
          val (f,a) = Object.destAppTerm ob

          val _ = destUnwantedIdTerm f

          val obj' =
              ObjectProv.Object'
                {object = Object.Term a,
                 provenance = ObjectProv.Default}
        in
          ObjectProv.mk obj'
        end
      | ObjectProv.Special
          {command = Command.AppTerm,
           arguments = [objF,objA],
           generated = [_],
           result = 0} =>
        let
          val _ = destUnwantedIdTermObjectProv objF
        in
          objA
        end
      | ObjectProv.Special
          {command = Command.AppThm,
           arguments = [objF,objA],
           generated = [_],
           result = 0} =>
        let
          val _ = destUnwantedIdReflObjectProv objF
        in
          objA
        end
      | ObjectProv.Special _ => raise Error "ObjectRewrite.unwantedId"
    end;

(* ------------------------------------------------------------------------- *)
(* Default rewrite.                                                          *)
(* ------------------------------------------------------------------------- *)

val default = try unwantedId;

end
