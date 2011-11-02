(* ========================================================================= *)
(* UNWANTED OPENTHEORY OBJECTS                                               *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectUnwanted :> ObjectUnwanted =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* The Unwanted namespace.                                                   *)
(* ------------------------------------------------------------------------- *)

val namespace = Namespace.fromList ["Unwanted"];

(* ------------------------------------------------------------------------- *)
(* Unwanted.id constants.                                                    *)
(* ------------------------------------------------------------------------- *)

val idName = Name.mk (namespace,"id");

val unwantedIdConst = Const.mkUndef idName;

local
  val sav = {savable = true};

  val xNameObj = Object.mkName (Name.mkGlobal "x");

  val alphaNameObj = Object.mkName (Name.mkGlobal "A");

  val alphaTypeObj = Object.mkVarType alphaNameObj;

  val xVarObj = Object.mkVar sav xNameObj alphaTypeObj;

  val xTermObj = Object.mkVarTerm sav xVarObj;
in
  val idTermObject = Object.mkAbsTerm sav xVarObj xTermObj;
end;

val (idConstObject,idDefObject) =
    Object.mkDefineConst {savable = true} idName idTermObject;

val idConst = Object.destConst idConstObject;

val isIdConst = Term.equalConst idConst;

fun destIdxTerm tm =
    Term.destUnaryOp (Const.equal idConst) tm
(*OpenTheoryDebug
    handle Error err => raise Error ("in ObjectUnwanted.destIdxTerm:\n" ^ err);
*)

val isIdxTerm = can destIdxTerm;

(* ------------------------------------------------------------------------- *)
(* Eliminating instances of `Unwanted.id x` subterms.                        *)
(* ------------------------------------------------------------------------- *)

datatype eliminateIdx =
    EliminateIdx of
      {idxSearch : TermSearch.search};

val newIdx =
    let
      val idxSearch =
          TermSearch.new
            {predicate = isIdxTerm,
             leftToRight = true}
    in
      EliminateIdx
        {idxSearch = idxSearch}
    end;

fun isIdxData data elim =
    let
      val EliminateIdx {idxSearch} = elim

      val (subtm,idxSearch) = ObjectData.sharingSearch data idxSearch

      val result = Option.isSome subtm

      val elim =
          EliminateIdx
            {idxSearch = idxSearch}
    in
      (result,elim)
    end;

fun isIdxObject obj elim = isIdxData (Object.data obj) elim;

fun eliminateIdx obj elim =
    let
      val obj' = NONE
    in
      (obj',elim)
    end;

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
    destUnwantedIdTerm (ObjectData.destTerm ob);

fun destUnwantedIdReflObject ob =
    destUnwantedIdRefl (ObjectData.destThm ob);

fun destUnwantedIdTermObject obj =
    destUnwantedIdTermObject (Object.object obj);

fun destUnwantedIdReflObject obj =
    destUnwantedIdReflObject (Object.object obj);

fun unwantedId obj =
    let
      val Object.Object' {object = ob, provenance = prov} =
          Object.dest obj

(*OpenTheoryTrace4
      val () = Print.trace ObjectData.pp "ObjectRewrite.unwantedId.ob" ob

      val () = Print.trace Object.ppProvenance
                 "ObjectRewrite.unwantedId.prov" prov
*)
    in
      case prov of
        Object.Default =>
        let
          val (f,a) = ObjectData.destAppTerm ob

          val _ = destUnwantedIdTerm f

          val obj' =
              Object.Object'
                {object = ObjectData.Term a,
                 provenance = Object.Default}
        in
          Object.mk obj'
        end
      | Object.Special
          {command = Command.AppTerm,
           arguments = [objF,objA],
           generated = [_],
           result = 0} =>
        let
          val _ = destUnwantedIdTermObject objF
        in
          objA
        end
      | Object.Special
          {command = Command.AppThm,
           arguments = [objF,objA],
           generated = [_],
           result = 0} =>
        let
          val _ = destUnwantedIdReflObject objF
        in
          objA
        end
      | Object.Special _ => raise Error "ObjectRewrite.unwantedId"
    end;
***)

(* ------------------------------------------------------------------------- *)
(* Eliminating Unwanted objects.                                             *)
(* ------------------------------------------------------------------------- *)

datatype eliminate =
    Eliminate of
      {defaultMap : (bool * Object.object) ObjectDataMap.map,
       specialMap : Object.object option IntMap.map,
       elimIdx : eliminateIdx,
       savable : bool};

fun new {savable} =
    let
      val defaultMap =
          ObjectDataMap.fromList
            [(ObjectData.Const unwantedIdConst, (false,idConstObject))]

      and specialMap = IntMap.new ()

      and elimIdx = newIdx
    in
      Eliminate
        {defaultMap = defaultMap,
         specialMap = specialMap,
         elimIdx = elimIdx,
         savable = savable}
    end;

local
  fun eliminateTopIdx obj elim =
      let
        val Eliminate
              {defaultMap,
               specialMap,
               elimIdx,
               savable} = elim

        val (obj',elimIdx) = eliminateIdx obj elimIdx

(*OpenTheoryDebug
        val () =
            let
              val result = Option.getOpt (obj',obj)

              val (present,_) = isIdxObject result elimIdx
            in
              if not present then ()
              else
                let
                  val bug =
                      "ObjectUnwanted.eliminateTopIdx:\n" ^
                      Print.toString Object.pp result
                in
                  raise Bug bug
                end
            end
*)

        val elim =
            Eliminate
              {defaultMap = defaultMap,
               specialMap = specialMap,
               elimIdx = elimIdx,
               savable = savable}
      in
        (obj',elim)
      end;

  fun eliminateSeq' (f,(unchanged,obj,elim)) =
        let
          val (obj',elim) = f obj elim

          val (unchanged,obj) =
              case obj' of
                NONE => (unchanged,obj)
              | SOME obj => (false,obj)
        in
          (unchanged,obj,elim)
        end;

  val eliminateTopMethods =
      [eliminateTopIdx];

  fun eliminateTop obj elim =
      let
        val unchanged = true

        val (unchanged,obj,elim) =
            List.foldl eliminateSeq' (unchanged,obj,elim)
              eliminateTopMethods

        val obj' = if unchanged then NONE else SOME obj
      in
        (obj',elim)
      end;

  fun eliminateOb' ob elim =
      let
        val Eliminate
              {defaultMap,
               specialMap,
               elimIdx,
               savable} = elim
      in
        case ObjectDataMap.peek defaultMap ob of
          SOME obj' => (obj',elim)
        | NONE =>
          let
            val (cmd,obs) = ObjectData.command ob

            val (objs',elim) = maps eliminateOb' obs elim

            val unchanged = List.all fst objs'
            and objs = List.map snd objs'

            val obj =
                let
                  val xs = Object.mkCommand {savable = savable} cmd objs

(*OpenTheoryDebug
                  val () =
                      case xs of
                        [_] => ()
                      | _ => raise Bug "ObjectUnwanted.eliminateOb'"
*)
                in
                  hd xs
                end

            val (obj',elim) =
                let
                  val (obj',elim) = eliminateTop obj elim

                  val obj' =
                      case obj' of
                        NONE => (unchanged,obj)
                      | SOME obj => (false,obj)
                in
                  (obj',elim)
                end

            val defaultMap = ObjectDataMap.insert defaultMap (ob,obj')

            val elim =
                Eliminate
                  {defaultMap = defaultMap,
                   specialMap = specialMap,
                   elimIdx = elimIdx,
                   savable = savable}
          in
            (obj',elim)
          end
      end;

  fun eliminateOb ob elim =
      let
        val ((unchanged,obj),elim) = eliminateOb' ob elim

        val obj' = if unchanged then NONE else SOME obj

(*OpenTheoryTrace4
        val () =
            let
              val ppElim =
                  Print.ppOp2 " ->" ObjectData.pp
                    (Print.ppOption Object.pp)
            in
              Print.trace ppElim "ObjectUnwanted.eliminateOb"
                (ob,obj')
            end
*)
      in
        (obj',elim)
      end;

  fun eliminateObj obj elim =
      if not (Object.isDefault obj) then eliminateTop obj elim
      else eliminateOb (Object.data obj) elim;

  fun preDescent obj elim =
      let
        val Eliminate {specialMap,...} = elim

        val i = Object.id obj
      in
        case IntMap.peek specialMap i of
          NONE => {descend = true, result = (NONE,elim)}
        | SOME obj' => {descend = false, result = (obj',elim)}
      end;

  fun postDescent obj0 obj1' elim =
      let
        val i = Object.id obj0

        val unchanged = true

        val (unchanged,obj1) =
            case obj1' of
              NONE => (unchanged,obj0)
            | SOME obj => (false,obj)

        val (obj2',elim) = eliminateObj obj1 elim

        val (unchanged,obj2) =
            case obj2' of
              NONE => (unchanged,obj1)
            | SOME obj => (false,obj)

        val obj2' = if unchanged then NONE else SOME obj2

        val Eliminate
              {defaultMap,
               specialMap,
               elimIdx,
               savable} = elim

        val specialMap = IntMap.insert specialMap (i,obj2')

        val elim =
            Eliminate
              {defaultMap = defaultMap,
               specialMap = specialMap,
               elimIdx = elimIdx,
               savable = savable}
      in
        (obj2',elim)
      end;
in
  fun sharingEliminate obj elim =
      let
        val Eliminate {savable,...} = elim
      in
        Object.maps
          {preDescent = preDescent,
           postDescent = postDescent,
           savable = savable} obj elim
      end;
end;

fun eliminate elim obj =
    let
      val (obj',_) = sharingEliminate obj elim
    in
      obj'
    end;

end
