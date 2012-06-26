(* ========================================================================= *)
(* UNWANTED OPENTHEORY OBJECTS                                               *)
(* Copyright (c) 2011 Joe Hurd, distributed under the MIT license            *)
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

val idxTermSearch =
    TermSearch.new
      {predicate = isIdxTerm,
       leftToRight = true};

(* ------------------------------------------------------------------------- *)
(* Eliminating instances of `Unwanted.id x` subterms.                        *)
(* ------------------------------------------------------------------------- *)

datatype eliminateIdx =
    EliminateIdx of
      {idxSearch : TermSearch.search,
       store : ObjectStore.store,
       termRewr : Object.object TermMap.map};

val emptyIdx =
    let
      val idxSearch = idxTermSearch
      and store = ObjectStore.emptyTermBuilder
      and termRewr = TermMap.new ()
    in
      EliminateIdx
        {idxSearch = idxSearch,
         store = store,
         termRewr = termRewr}
    end;

fun isTermIdx tm elim =
    let
      val EliminateIdx {idxSearch,store,termRewr} = elim

      val (subtm,idxSearch) = TermSearch.sharingSearchTerm tm idxSearch

      val result = Option.isSome subtm

      val elim =
          EliminateIdx
            {idxSearch = idxSearch,
             store = store,
             termRewr = termRewr}
    in
      (result,elim)
    end;

fun isDataIdx data elim =
    let
      val EliminateIdx {idxSearch,store,termRewr} = elim

      val (subtm,idxSearch) = ObjectData.sharingSearch data idxSearch

      val result = Option.isSome subtm

      val elim =
          EliminateIdx
            {idxSearch = idxSearch,
             store = store,
             termRewr = termRewr}
    in
      (result,elim)
    end;

fun isObjectIdx obj elim = isDataIdx (Object.data obj) elim;

fun addDataIdx elim obj =
    let
      val EliminateIdx {idxSearch,store,termRewr} = elim

      val store = ObjectStore.add store obj
    in
      EliminateIdx
        {idxSearch = idxSearch,
         store = store,
         termRewr = termRewr}
    end;

fun buildDataIdx d elim =
    let
      val EliminateIdx {idxSearch,store,termRewr} = elim

      val (obj,store) = ObjectStore.build d store

(*OpenTheoryDebug
      val () =
          if Object.equalData d obj then ()
          else raise Bug "ObjectUnwanted.buildDataIdx"
*)

      val elim =
          EliminateIdx
            {idxSearch = idxSearch,
             store = store,
             termRewr = termRewr}
    in
      (obj,elim)
    end;

fun buildTypeIdx ty elim = buildDataIdx (ObjectData.Type ty) elim;

fun buildVarIdx v elim = buildDataIdx (ObjectData.Var v) elim;

fun buildTermIdx tm elim = buildDataIdx (ObjectData.Term tm) elim;

fun peekRewrIdx elim tm =
    let
      val EliminateIdx {termRewr,...} = elim
    in
      TermMap.peek termRewr tm
    end;

fun insertRewrIdx elim tm_obj =
    let
      val EliminateIdx {idxSearch,store,termRewr} = elim

      val termRewr = TermMap.insert termRewr tm_obj
    in
      EliminateIdx
        {idxSearch = idxSearch,
         store = store,
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
      case peekRewrIdx elim tm of
        SOME obj => (obj,elim)
      | NONE =>
        let
          val (result,elim) = isTermIdx tm elim

          val (obj,elim) =
              if not result then
                let
                  val (objTm,elim) = buildTermIdx tm elim

                  val objTh = Object.mkRefl savable objTm
                in
                  (objTh,elim)
                end
              else
                case Term.dest tm of
                  TypeTerm.Const' _ =>
                  raise Bug "ObjectUnwanted.eliminateIdx.rewriteTerm: const"
                | TypeTerm.Var' _ =>
                  raise Bug "ObjectUnwanted.eliminateIdx.rewriteTerm: var"
                | TypeTerm.Abs' (v,b) =>
                  let
                    val (objV,elim) = buildVarIdx v elim

                    val (objB,elim) = rewriteTerm b elim

                    val obj = Object.mkAbsThm savable objV objB
                  in
                    (obj,elim)
                  end
                | TypeTerm.App' (f,a) =>
                  let
                    val (objA,elim) = rewriteTerm a elim
                  in
                    if isIdConst f then
                      let
                        val (objT,elim) = buildTypeIdx (Term.typeOf a) elim

                        val objF = mkIdDefObject objT

                        val objD = Object.mkAppThm savable objF objA

                        val (g,b) =
                            Term.destApp (Thm.concl (Object.destThm objD))

                        val (objG,elim) = buildTermIdx g elim

                        val (objB,elim) = buildTermIdx b elim

                        val objG = Object.mkRefl savable objG

                        val objB = Object.mkBetaConv savable objB

                        val objR = Object.mkAppThm savable objG objB

                        val obj = Object.mkEqMp savable objR objD
                      in
                        (obj,elim)
                      end
                    else
                      let
                        val (objF,elim) = rewriteTerm f elim

                        val obj = Object.mkAppThm savable objF objA
                      in
                        (obj,elim)
                      end
                  end

          val elim = insertRewrIdx elim (tm,obj)
        in
          (obj,elim)
        end;

  fun rewriteHyp (hyp,(obj0,elim)) =
      let
        val (result,elim) = isTermIdx hyp elim
      in
        if not result then (obj0,elim)
        else
(* ------------------------------------------------------------------------- *)
(* given  (Gamma union { hyp }) |- concl  (0)                                *)
(* ------------------------------------------------------------------------- *)
          let
(* ------------------------------------------------------------------------- *)
(* prove  |- hyp <=> hyp'  (1)                                               *)
(*    by  rewriteTerm `hyp`                                                  *)
(* ------------------------------------------------------------------------- *)
            val (obj1,elim) = rewriteTerm hyp elim

            val (eq,hyp') = Term.destApp (Thm.concl (Object.destThm obj1))

            val (eq,elim) = buildTermIdx (Term.rator eq) elim

            val (hyp,elim) = buildTermIdx hyp elim

            val (hyp',elim) = buildTermIdx hyp' elim

(* ------------------------------------------------------------------------- *)
(* prove  |- (<=>) = (<=>)  (2)                                              *)
(*    by  refl `(<=>)`                                                       *)
(* ------------------------------------------------------------------------- *)
            val obj2 = Object.mkRefl savable eq

(* ------------------------------------------------------------------------- *)
(* prove  |- ((<=>) hyp) = ((<=>) hyp')  (3)                                 *)
(*    by  appThm (2) (1)                                                     *)
(* ------------------------------------------------------------------------- *)
            val obj3 = Object.mkAppThm savable obj2 obj1

(* ------------------------------------------------------------------------- *)
(* prove  |- hyp <=> hyp  (4)                                                *)
(*    by  refl `hyp`                                                         *)
(* ------------------------------------------------------------------------- *)
            val obj4 = Object.mkRefl savable hyp

(* ------------------------------------------------------------------------- *)
(* prove  |- (hyp <=> hyp) <=> (hyp' <=> hyp)  (5)                           *)
(*    by  appThm (3) (4)                                                     *)
(* ------------------------------------------------------------------------- *)
            val obj5 = Object.mkAppThm savable obj3 obj4

(* ------------------------------------------------------------------------- *)
(* prove  |- hyp' <=> hyp  (6)                                               *)
(*    by  eqMp (5) (4)                                                       *)
(* ------------------------------------------------------------------------- *)
            val obj6 = Object.mkEqMp savable obj5 obj4

(* ------------------------------------------------------------------------- *)
(* prove  { hyp' } |- hyp'  (7)                                              *)
(*    by  assume `hyp'`                                                      *)
(* ------------------------------------------------------------------------- *)
            val obj7 = Object.mkAssume savable hyp'

(* ------------------------------------------------------------------------- *)
(* prove  { hyp' } |- hyp  (8)                                               *)
(*    by  eqMp (6) (7)                                                       *)
(* ------------------------------------------------------------------------- *)
            val obj8 = Object.mkEqMp savable obj6 obj7

(* ------------------------------------------------------------------------- *)
(* prove  (Gamma union { hyp' }) |- hyp <=> concl  (9)                       *)
(*    by  deductAntisym (8) (0)                                              *)
(* ------------------------------------------------------------------------- *)
            val obj9 = Object.mkDeductAntisym savable obj8 obj0

(* ------------------------------------------------------------------------- *)
(* prove  (Gamma union { hyp' }) |- concl  (10)                              *)
(*    by  eqMp (9) (8)                                                       *)
(* ------------------------------------------------------------------------- *)
            val obj10 = Object.mkEqMp savable obj9 obj8
          in
            (obj10,elim)
          end
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Error ("in ObjectUnwanted.eliminateIdx.rewriteHyp:\n" ^ err);
*)

  fun rewriteConcl concl (obj0,elim) =
      let
        val (result,elim) = isTermIdx concl elim
      in
        if not result then (obj0,elim)
        else
(* ------------------------------------------------------------------------- *)
(* given  Gamma |- concl  (0)                                                *)
(* ------------------------------------------------------------------------- *)
          let
(* ------------------------------------------------------------------------- *)
(* prove  |- concl = concl'  (1)                                             *)
(*    by  rewriteTerm `concl`                                                *)
(* ------------------------------------------------------------------------- *)
            val (obj1,elim) = rewriteTerm concl elim

(* ------------------------------------------------------------------------- *)
(* prove  Gamma |- concl'  (2)                                               *)
(*    by  eqMp (1) (0)                                                       *)
(* ------------------------------------------------------------------------- *)
            val obj2 = Object.mkEqMp savable obj1 obj0
          in
            (obj2,elim)
          end
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Error ("in ObjectUnwanted.eliminateIdx.rewriteConcl:\n" ^ err);
*)

  fun rewriteThmIdx obj elim =
      let
        val elim = addDataIdx elim obj

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
           val (result,elim) = isObjectIdx obj elim
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
      {unwantedSearch : TermSearch.search,
       defaultMap : Object.object ObjectDataMap.map,
       specialMap : Object.object option IntMap.map,
       elimIdx : eliminateIdx};

val isUnwantedConst = Const.equal unwantedIdConst;

fun isUnwantedTerm tm =
    case Term.dest tm of
      TypeTerm.Const' (c,_) => isUnwantedConst c
    | _ => false;

val empty =
    let
      val unwantedSearch =
          TermSearch.new
            {predicate = isUnwantedTerm,
             leftToRight = true}

      and defaultMap =
          ObjectDataMap.fromList
            [(ObjectData.Const unwantedIdConst, idConstObject)]

      and specialMap = IntMap.new ()

      and elimIdx = emptyIdx
    in
      Eliminate
        {unwantedSearch = unwantedSearch,
         defaultMap = defaultMap,
         specialMap = specialMap,
         elimIdx = elimIdx}
    end;

fun containsUnwanted d elim =
    case d of
      ObjectData.Const c => (isUnwantedConst c, elim)
    | _ =>
      let
        val Eliminate
              {unwantedSearch,
               defaultMap,
               specialMap,
               elimIdx} = elim

        val (to,unwantedSearch) = ObjectData.sharingSearch d unwantedSearch

        val elim =
            Eliminate
              {unwantedSearch = unwantedSearch,
               defaultMap = defaultMap,
               specialMap = specialMap,
               elimIdx = elimIdx}
      in
        (Option.isSome to, elim)
      end;

fun peekDefaultMap elim =
    let
      val Eliminate {defaultMap,...} = elim
    in
      ObjectDataMap.peek defaultMap
    end;

fun insertDefaultMap elim d_obj' =
    let
      val Eliminate
            {unwantedSearch,
             defaultMap,
             specialMap,
             elimIdx} = elim

      val defaultMap = ObjectDataMap.insert defaultMap d_obj'
    in
      Eliminate
        {unwantedSearch = unwantedSearch,
         defaultMap = defaultMap,
         specialMap = specialMap,
         elimIdx = elimIdx}
    end;

fun peekSpecialMap elim =
    let
      val Eliminate {specialMap,...} = elim
    in
      IntMap.peek specialMap
    end;

fun insertSpecialMap elim i_obj' =
    let
      val Eliminate
            {unwantedSearch,
             defaultMap,
             specialMap,
             elimIdx} = elim

      val specialMap = IntMap.insert specialMap i_obj'
    in
      Eliminate
        {unwantedSearch = unwantedSearch,
         defaultMap = defaultMap,
         specialMap = specialMap,
         elimIdx = elimIdx}
    end;

local
  fun eliminateTopIdx obj elim =
      let
        val Eliminate
              {unwantedSearch,
               defaultMap,
               specialMap,
               elimIdx} = elim

        val (obj',elimIdx) = eliminateIdx obj elimIdx

(*OpenTheoryDebug
        val () =
            let
              val result = Option.getOpt (obj',obj)

              val (present,_) = isObjectIdx result elimIdx
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
              {unwantedSearch = unwantedSearch,
               defaultMap = defaultMap,
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

  fun eliminateData d elim =
      let
(*OpenTheoryDebug
        val () =
            if fst (containsUnwanted d elim) then ()
            else raise Bug "ObjectUnwanted.eliminateData: nothing to do"
*)
      in
        case peekDefaultMap elim d of
          SOME obj => (obj,elim)
        | NONE =>
          let
            val (cmd,args) = ObjectData.command d

            val (objs,elim) = Useful.maps eliminateData' args elim

            val obj =
                let
                  val xs = Object.mkCommand savable cmd objs

(*OpenTheoryDebug
                  val () =
                      case xs of
                        [_] => ()
                      | _ => raise Bug "ObjectUnwanted.eliminateData"
*)
                in
                  hd xs
                end

            val (obj',elim) = eliminateTop obj elim

            val obj = Option.getOpt (obj',obj)

            val elim = insertDefaultMap elim (d,obj)
          in
            (obj,elim)
          end
      end

  and eliminateData' d elim =
      let
        val (b,elim) = containsUnwanted d elim
      in
        if b then eliminateData d elim
        else
          case peekDefaultMap elim d of
            SOME obj => (obj,elim)
          | NONE =>
            let
              val obj = Object.mkUnsavable d

              val elim = insertDefaultMap elim (d,obj)
            in
              (obj,elim)
            end
      end;

  fun eliminateDefault obj elim =
      let
(*OpenTheoryDebug
        val () =
            if Object.isDefault obj then ()
            else raise Bug "ObjectUnwanted.eliminateDefault: not default"
*)
        val d = Object.data obj

        val (b,elim) = containsUnwanted d elim
      in
        if not b then (NONE,elim)
        else
          let
            val (obj,elim) = eliminateData d elim

(*OpenTheoryDebug
            val () =
                let
                  val ppElim = Print.ppOp2 " ->" ObjectData.pp Object.pp

                  val (bad,_) = containsUnwanted (Object.data obj) elim

                  val show = bad
(*OpenTheoryTrace4
                  val show = true
*)
                  val () =
                      if not show then ()
                      else
                        Print.trace ppElim "ObjectUnwanted.eliminateDefault"
                          (d,obj)
                in
                  if not bad then ()
                  else raise Bug "ObjectUnwanted.eliminateDefault: unwanted"
                end
*)
          in
            (SOME obj, elim)
          end
      end;

  fun preDescent obj elim =
      case peekSpecialMap elim (Object.id obj) of
        NONE => {descend = true, result = (NONE,elim)}
      | SOME obj' => {descend = false, result = (obj',elim)};

  fun postDescent obj0 obj1' elim =
      let
        val unchanged = true

        val (unchanged,obj1) =
            case obj1' of
              NONE => (unchanged,obj0)
            | SOME obj => (false,obj)

        val (obj2',elim) =
            if Object.isDefault obj0 then
              let
(*OpenTheoryDebug
                val () =
                    if unchanged then ()
                    else raise Bug "ObjectUnwanted.postDescent: default changed"
*)
              in
                eliminateDefault obj1 elim
              end
            else
              let
(*OpenTheoryDebug
                val () =
                    let
                      fun ppObj obj =
                          Print.program
                            [Object.pp obj,
                             Print.ppString " [",
                             Object.ppProvenance (Object.provenance obj),
                             Print.ppString "]"]

                      val ppElim = Print.ppOp2 " ->" ppObj (Print.ppOption ppObj)

                      val (bad,_) = containsUnwanted (Object.data obj1) elim

                      val show = bad
(*OpenTheoryTrace4
                      val show = true
*)
                      val () =
                          if not show then ()
                          else
                            Print.trace ppElim "ObjectUnwanted.postDescent"
                              (obj0,obj1')
                    in
                      if not bad then ()
                      else raise Bug "ObjectUnwanted.postDescent: unwanted obj1"
                    end
*)
              in
                if unchanged then
                  let
                    (* If nothing changed during a Special descent then *)
                    (* no Unwanted constants were instantiated, and so *)
                    (* there's nothing to be done at this subproof *)
                  in
                    (NONE,elim)
                  end
                else
                  eliminateTop obj1 elim
              end

        val (unchanged,obj2) =
            case obj2' of
              NONE => (unchanged,obj1)
            | SOME obj => (false,obj)

(*OpenTheoryDebug
        val () =
            let
              fun ppObj obj =
                  Print.program
                    [Object.pp obj,
                     Print.ppString " [",
                     Object.ppProvenance (Object.provenance obj),
                     Print.ppString "]"]

              val ppElim =
                  Print.ppOp2 " ->" ppObj
                    (Print.ppOp2 " ->" (Print.ppOption ppObj)
                       (Print.ppOption ppObj))

              val (bad,_) = containsUnwanted (Object.data obj2) elim

              val show = bad
(*OpenTheoryTrace4
              val show = true
*)
              val () =
                  if not show then ()
                  else
                    Print.trace ppElim "ObjectUnwanted.postDescent"
                      (obj0,(obj1',obj2'))
            in
              if not bad then ()
              else raise Bug "ObjectUnwanted.postDescent: unwanted obj2"
            end
*)
        val obj2' = if unchanged then NONE else SOME obj2

        val elim = insertSpecialMap elim (Object.id obj0, obj2')
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
