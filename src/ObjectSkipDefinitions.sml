(* ========================================================================= *)
(* REPLACE DEFINITIONS WITH THEORY ASSUMPTIONS IN OPENTHEORY OBJECTS         *)
(* Copyright (c) 2016 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectSkipDefinitions :> ObjectSkipDefinitions =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Everything is savable in this module.                                     *)
(* ------------------------------------------------------------------------- *)

val savable = {savable = true};

(* ------------------------------------------------------------------------- *)
(* Skip definitions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun undefData data =
    case data of
      ObjectData.Const c =>
      let
        val c = Const.mkUndef (Const.name c)
      in
        ObjectData.Const c
      end
    | ObjectData.List l =>
      let
        val l = map undefData l
      in
        ObjectData.List l
      end
    | ObjectData.Thm th =>
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Bug "ObjectSkipDefinitions.undefData: bad hyp"

        val concl =
            case TermRewrite.rewriteTerm TermRewrite.undef concl of
              NONE => raise Bug "ObjectSkipDefinitions.undefData: bad concl"
            | SOME c => c

        val th = Thm.axiom (Sequent.Sequent {hyp = hyp, concl = concl})
      in
        ObjectData.Thm th
      end
    | ObjectData.TypeOp t =>
      let
        val t = TypeOp.mkUndef (TypeOp.name t)
      in
        ObjectData.TypeOp t
      end
    | _ => raise Bug "ObjectSkipDefinitions.undefData: strange object";

fun isDefinedObject obj =
    case Object.provenance obj of
      Object.Default => false
    | Object.Special {command,...} =>
      case command of
        Command.DefineConst => true
      | Command.DefineConstList => true
      | Command.DefineTypeOp => true
      | Command.DefineTypeOpLegacy => true
      | _ => false;

fun skipDefinition obj =
    if not (isDefinedObject obj) then NONE
    else
      let
        val data = Object.data obj
      in
        SOME (Object.mkUnsavable (undefData data))
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Error ("in ObjectSkipDefinitions.skipDefinition:\n" ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Skip all definitions in a proof term.                                     *)
(* ------------------------------------------------------------------------- *)

datatype skipDefinitions =
    SkipDefinitions of
      {specialMap : Object.object option IntMap.map};

val empty =
    let
      val specialMap = IntMap.new ()
    in
      SkipDefinitions {specialMap = specialMap}
    end;

fun peekSpecialMap skip =
    let
      val SkipDefinitions {specialMap,...} = skip
    in
      IntMap.peek specialMap
    end;

fun insertSpecialMap skip i_obj' =
    let
      val SkipDefinitions {specialMap} = skip

      val specialMap = IntMap.insert specialMap i_obj'
    in
      SkipDefinitions {specialMap = specialMap}
    end;

local
  fun preDescent obj skip =
      case peekSpecialMap skip (Object.id obj) of
        NONE => {descend = true, result = (NONE,skip)}
      | SOME obj' => {descend = false, result = (obj',skip)};

  fun postDescent obj0 obj1' skip =
      let
        val unchanged = true

        val (unchanged,obj1) =
            case obj1' of
              NONE => (unchanged,obj0)
            | SOME obj => (false,obj)

        val obj2' = skipDefinition obj1

        val (unchanged,obj2) =
            case obj2' of
              NONE => (unchanged,obj1)
            | SOME obj => (false,obj)

        val obj2' = if unchanged then NONE else SOME obj2

        val skip = insertSpecialMap skip (Object.id obj0, obj2')
      in
        (obj2',skip)
      end;
in
  fun sharingSkipDefinitions obj skip =
      let
        val {savable} = savable
      in
        Object.maps
          {preDescent = preDescent,
           postDescent = postDescent,
           savable = savable} obj skip
      end;
end;

fun skipDefinitions skip obj =
    let
      val (obj',_) = sharingSkipDefinitions obj skip
    in
      obj'
    end;

end
