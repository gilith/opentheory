(* ========================================================================= *)
(* UNWANTED OPENTHEORY OBJECTS                                               *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectUnwanted :> ObjectUnwanted =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* We always save proofs in this module.                                     *)
(* ------------------------------------------------------------------------- *)

val savable = {savable = true};

(* ------------------------------------------------------------------------- *)
(* The Unwanted namespace.                                                   *)
(* ------------------------------------------------------------------------- *)

val namespace = Namespace.fromList ["Unwanted"];

(* ------------------------------------------------------------------------- *)
(* Unwanted.id constants.                                                    *)
(* ------------------------------------------------------------------------- *)

val idName = Name.mk (namespace,"id");

local
  val xNameObj = ObjectProv.mkName (Name.mkGlobal "x");

  val alphaNameObj = ObjectProv.mkName (Name.mkGlobal "A");

  val alphaTypeObj = ObjectProv.mkVarType alphaNameObj;

  val xVarObj = ObjectProv.mkVar savable xNameObj alphaTypeObj;

  val xTermObj = ObjectProv.mkVarTerm savable xVarObj;
in
  val idTermObject = ObjectProv.mkAbsTerm savable xVarObj xTermObj;
end;

val (idConstObject,idDefObject) =
    ObjectProv.mkDefineConst {savable = true} idName idTermObject;

(***
(* ------------------------------------------------------------------------- *)
(* Unwanted constants.                                                       *)
(* ------------------------------------------------------------------------- *)

fun destUnwantedIdConst c =
    let
      val n = Const.name c

(*OpenTheoryTrace4
      val () = Print.trace Name.pp "ObjectRewrite.destUnwantedIdConst.n" n
*)
    in
      if Name.equal n Name.unwantedIdConst then ()
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

fun unwantedId obj =
    let
      val ObjectProv.Object' {object = ob, provenance = prov} =
          ObjectProv.dest obj

(*OpenTheoryTrace4
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
***)

end
