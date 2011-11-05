(* ========================================================================= *)
(* UNWANTED OPENTHEORY OBJECTS                                               *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectUnwanted :> ObjectUnwanted =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Everything is savable in this module.                                     *)
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

val unwantedIdConst = Const.mkUndef idName;

local
  val xNameObj = Object.mkName (Name.mkGlobal "x");

  val alphaNameObj = Object.mkName (Name.mkGlobal "A");

  val alphaTypeObj = Object.mkVarType alphaNameObj;

  val xVarObj = Object.mkVar savable xNameObj alphaTypeObj;

  val xTermObj = Object.mkVarTerm savable xVarObj;

  val idTermObj = Object.mkAbsTerm savable xVarObj xTermObj;

  val (idConstObj,idDefObj) = Object.mkDefineConst savable idName idTermObj;
in
  val idConstObject = idConstObj;

  fun mkIdDefObject objT =
      let
        val objM = Object.mkList savable [alphaNameObj,objT]

        val objYS = Object.mkList savable [objM]

        val objMS = Object.mkNil

        val objS = Object.mkList savable [objYS,objMS]
      in
        Object.mkSubst savable objS idDefObj
      end;
end;

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
      {idxSearch : TermSearch.search,
       symbol : ObjectSymbol.symbol,
       termRewr : Object.object TermMap.map};

val newIdx =
    let
      val idxSearch =
          TermSearch.new
            {predicate = isIdxTerm,
             leftToRight = true}

      and symbol = ObjectSymbol.empty

      and termRewr = TermMap.new ()
    in
      EliminateIdx
        {idxSearch = idxSearch,
         symbol = symbol,
         termRewr = termRewr}
    end;

fun isIdxTerm tm elim =
    let
      val EliminateIdx {idxSearch,symbol,termRewr} = elim

      val (subtm,idxSearch) = TermSearch.sharingSearchTerm tm idxSearch

      val result = Option.isSome subtm

      val elim =
          EliminateIdx
            {idxSearch = idxSearch,
             symbol = symbol,
             termRewr = termRewr}
    in
      (result,elim)
    end;

fun isIdxData data elim =
    let
      val EliminateIdx {idxSearch,symbol,termRewr} = elim

      val (subtm,idxSearch) = ObjectData.sharingSearch data idxSearch

      val result = Option.isSome subtm

      val elim =
          EliminateIdx
            {idxSearch = idxSearch,
             symbol = symbol,
             termRewr = termRewr}
    in
      (result,elim)
    end;

fun isIdxObject obj elim = isIdxData (Object.data obj) elim;

fun addIdxSymbol elim obj =
    let
      val EliminateIdx {idxSearch,symbol,termRewr} = elim

      val symbol = ObjectSymbol.addObject symbol obj
    in
      EliminateIdx
        {idxSearch = idxSearch,
         symbol = symbol,
         termRewr = termRewr}
    end;

local
  fun replaceAxiomId obj =
      let
        val (_,objC) = Object.unMkAxiom obj

        val (_,objI) = Object.unMkAppTerm objC

        val (objV,_) = Object.unMkAbsTerm objI

        val (_,objT) = Object.unMkVar objV

        val obj' = mkIdDefObject objT
      in
        if Thm.equal (Object.destThm obj') (Object.destThm obj) then obj'
        else raise Error "ObjectUnwanted.replaceAxiomId"
      end;

  fun cleanRemoveIdx obj =
      (case Object.provenance obj of
         Object.Default => NONE
       | Object.Special {command,arguments,...} =>
         case (command,arguments) of
           (Command.AppTerm,[objF,objA]) =>
           let
(*OpenTheoryDebug
             val () =
                 if Term.equalConst idConst (Object.destTerm objF) then ()
                 else raise Error "bad id function in appTerm"
*)
           in
             SOME objA
           end
         | (Command.AppThm,[objF,objA]) =>
           let
             val tm = Thm.concl (Object.destThm objF)
           in
             if not (Term.isRefl tm) then NONE
             else
               let
(*OpenTheoryDebug
                 val () =
                     if Term.equalConst idConst (Term.destRefl tm) then ()
                     else raise Error "bad id function in appTerm"
*)
               in
                 SOME objA
               end
           end
         | _ => NONE)
(*OpenTheoryDebug
      handle Error err =>
        raise Error ("in ObjectUnwanted.eliminateIdx.cleanRemoveIdx:\n" ^ err);
*)

  fun rewriteTerm tm elim =
      

  have  (Gamma union { tm }) |- concl  (0)

  prove |- tm = simplified(tm)  (1) using rewriteTerm tm

  

  prove { simplified(tm) } |- tm

  prove (Gamma union ({ simplified(tm) } - { concl })) |- tm = concl  [deductAntisym 

  fun rewriteHyp (tm,(obj,elim)) =
      let
        val (result,elim) = isIdxTerm tm elim
      in
        if not result then (obj,elim)
        else
          let
            val obj' = rewriteTerm tm elim
          in
            (obj',elim)
          end
      end

  fun rewriteConcl tm (obj,elim) =
      let
        val (result,elim) = isIdxTerm tm elim
      in
        if not result then (obj,elim)
        else
          let
            val objE = rewriteTerm tm elim

            val obj' = Object.mkEqMp savable objE obj
          in
            (obj',elim)
          end
      end

  fun rewriteThmIdx obj elim =
      let
        val elim = addIdxSymbol elim obj

        val Sequent.Sequent {hyp,concl} = Thm.sequent (Object.destThm obj)

        val (obj,elim) = TermAlphaSet.foldl rewriteHyp (obj,elim) hyp

        val (obj,elim) = rewriteConcl concl (obj,elim)
      in
        (SOME obj, elim)
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Error ("in ObjectUnwanted.eliminateIdx.rewriteThmIdx:\n" ^ err);
*)
in
  fun eliminateIdx obj elim =
      (case total replaceAxiomId obj of
         SOME obj => (SOME obj, elim)
       | NONE =>
         let
           val (result,elim) = isIdxObject obj elim
         in
           if not result then (NONE,elim)
           else
             case cleanRemoveIdx obj of
               SOME obj => (SOME obj, elim)
             | NONE => rewriteThmIdx obj elim
         end)
(*OpenTheoryDebug
      handle Error err =>
        raise Error ("in ObjectUnwanted.eliminateIdx:\n" ^ err);
*)
end;

(* ------------------------------------------------------------------------- *)
(* Eliminating Unwanted objects.                                             *)
(* ------------------------------------------------------------------------- *)

datatype eliminate =
    Eliminate of
      {defaultMap : (bool * Object.object) ObjectDataMap.map,
       specialMap : Object.object option IntMap.map,
       elimIdx : eliminateIdx};

val new =
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
         elimIdx = elimIdx}
    end;

local
  fun eliminateTopIdx obj elim =
      let
        val Eliminate
              {defaultMap,
               specialMap,
               elimIdx} = elim

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
               elimIdx = elimIdx}
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
               elimIdx} = elim
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
                  val xs = Object.mkCommand savable cmd objs

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
                   elimIdx = elimIdx}
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
               elimIdx} = elim

        val specialMap = IntMap.insert specialMap (i,obj2')

        val elim =
            Eliminate
              {defaultMap = defaultMap,
               specialMap = specialMap,
               elimIdx = elimIdx}
      in
        (obj2',elim)
      end;
in
  fun sharingEliminate obj elim =
      let
        val {savable} = savable
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
