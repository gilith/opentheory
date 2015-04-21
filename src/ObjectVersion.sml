(* ========================================================================= *)
(* CONVERTING OBJECTS TO A GIVEN ARTICLE VERSION                             *)
(* Copyright (c) 2014 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectVersion :> ObjectVersion =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Everything is savable in this module.                                     *)
(* ------------------------------------------------------------------------- *)

val savable = {savable = true};

(* ------------------------------------------------------------------------- *)
(* A type of object conversions.                                             *)
(* ------------------------------------------------------------------------- *)

datatype convert =
    Convert of
      {version : ArticleVersion.version,
       cache : Object.object option IntMap.map,
       store : ObjectStore.store};

fun new version =
    let
      val cache = IntMap.new ()
      and store = ObjectStore.emptyTermBuilder
    in
      Convert
        {version = version,
         cache = cache,
         store = store}
    end;

fun version (Convert {version = x, ...}) = x;

fun peekCache (Convert {cache,...}) objI =
    IntMap.peek cache (Object.id objI);

fun insertCache cvt (objI,objR') =
    let
      val Convert {version,cache,store} = cvt

      val cache = IntMap.insert cache (Object.id objI, objR')
    in
      Convert
        {version = version,
         cache = cache,
         store = store}
    end;

fun addStore cvt objs =
    let
      val Convert {version,cache,store} = cvt

      val store = ObjectStore.addList store objs
    in
      Convert
        {version = version,
         cache = cache,
         store = store}
    end;

fun buildStore data cvt =
    let
      val Convert {version,cache,store} = cvt

      val (obj,store) = ObjectStore.build data store

      val cvt =
          Convert
            {version = version,
             cache = cache,
             store = store}
    in
      (obj,cvt)
    end;

fun buildTypeStore ty = buildStore (ObjectData.Type ty);

fun buildVarStore v = buildStore (ObjectData.Var v);

fun buildTermStore tm = buildStore (ObjectData.Term tm);

(* ------------------------------------------------------------------------- *)
(* Convert a command to article version 5.                                   *)
(* ------------------------------------------------------------------------- *)

fun convert5 cmd args res cvt =
    case (cmd,args) of
      (Command.DefineConstList,[objL,objT]) =>
      let
        val vtm =
            let
              fun mk asm =
                  let
                    val (v,tm) = Term.destEq asm

                    val v = Term.destVar v
                  in
                    (v,tm)
                  end

              val th = Object.destThm objT

              val asms = TermAlphaSet.toList (Thm.hyp th)
            in
              VarMap.fromList (List.map mk asms)
            end

        fun simulate objL objT cvt =
            if Object.isNil objL then (objL,objT,cvt)
            else
              let
                val (objNV,cvt) = convert5 Command.HdTl [objL] 0 cvt

                val (objN,cvt) = convert5 Command.HdTl [objNV] 0 cvt

                val (objV,cvt) = convert5 Command.HdTl [objNV] 1 cvt

                val (objV,cvt) = convert5 Command.HdTl [objV] 0 cvt

                val n = Object.destName objN
                and v = Object.destVar objV

                val (objTm,cvt) =
                    case VarMap.peek vtm v of
                      SOME tm => buildTermStore tm cvt
                    | NONE => raise Bug "ObjectVersion.convert5.DefineConstList"

                val (objC,objD) = Object.mkDefineConst savable n objTm

                val (objTy,cvt) = buildTypeStore (Var.typeOf v) cvt

                val objCT = Object.mkConstTerm savable objC objTy

                val objTySub = Object.mkNil

                val objTmSub =
                    let
                      val objVCT =
                          Object.mkCons savable objV
                            (Object.mkCons savable objCT Object.mkNil)
                    in
                      Object.mkCons savable objVCT Object.mkNil
                    end

                val objSub =
                    Object.mkCons savable objTySub
                      (Object.mkCons savable objTmSub Object.mkNil)

                val objT = Object.mkSubst savable objSub objT

                val (objT,cvt) = convert5 Command.ProveHyp [objD,objT] 0 cvt

                val (objL,cvt) = convert5 Command.HdTl [objL] 1 cvt

                val (objCL,objT,cvt) = simulate objL objT cvt

                val objCL = Object.mkCons savable objC objCL
              in
                (objCL,objT,cvt)
              end

        val cvt = addStore cvt [objL,objT]

        val (objC,objD,cvt) = simulate objL objT cvt
      in
        case res of
          0 => (objC,cvt)
        | 1 => (objD,cvt)
        | _ => raise Bug "ObjectVersion.convert5.DefineConstList"
      end
    | (Command.DefineTypeOp,[objN,objA,objR,objV,objTh]) =>
      let
        val n = Object.destName objN
        and a = Object.destName objA
        and r = Object.destName objR

        val (objT,objA,objR,objAR,objRA) =
            Object.mkDefineTypeOpLegacy savable n a r objV objTh
      in
        case res of
          0 => (objT,cvt)
        | 1 => (objA,cvt)
        | 2 => (objR,cvt)
        | 3 =>
          let
            val cvt = addStore cvt [objTh,objT,objA,objR]

            val absRepTh = Object.destThm objAR

            val aVar = Term.destVar (Term.rhs (Thm.concl absRepTh))

            val (objA,cvt) = buildVarStore aVar cvt
          in
            (Object.mkAbsThm savable objA objAR, cvt)
          end
        | 4 =>
          let
            val cvt = addStore cvt [objTh,objT,objA,objR]

            val (obj0,cvt) = convert5 Command.Sym [objRA] 0 cvt

            val th0 = Object.destThm obj0

            val rVar = Term.destVar (Term.rand (Term.rhs (Thm.concl th0)))

            val (objR,cvt) = buildVarStore rVar cvt
          in
            (Object.mkAbsThm savable objR obj0, cvt)
          end
        | _ => raise Bug "ObjectVersion.convert5.DefineTypeOp"
      end
    | (Command.HdTl,[objL]) =>
      let
        val (objH,objT) =
            case Object.unMkCons objL of
              SOME x => x
            | NONE =>
              let
                val bug = "ObjectVersion.convert5.HdTl: not a cons command"

(*OpenTheoryDebug
                val prov = Object.provenance objL

                val bug =
                    bug ^ ", but rather\n  " ^
                    Print.toString Object.ppProvenance prov ^
                    "\nthat resulted in\n  " ^
                    Print.toString Object.pp objL
*)
              in
                raise Bug bug
              end
      in
        case res of
          0 => (objH,cvt)
        | 1 => (objT,cvt)
        | _ => raise Bug "ObjectVersion.convert5.HdTl: bad index"
      end
    | (Command.ProveHyp,[objA,objB]) =>
      let
        val obj0 = Object.mkDeductAntisym savable objA objB
      in
        (Object.mkEqMp savable obj0 objA, cvt)
      end
    | (Command.Sym,[objT]) =>
      let
        val cvt = addStore cvt [objT]

        val th0 = Object.destThm objT

        val (eqTm,lhsTm) = Term.destApp (Term.rator (Thm.concl th0))

        val (eqObj,cvt) = buildTermStore eqTm cvt

        val (lhsObj,cvt) = buildTermStore lhsTm cvt

        val eqObj = Object.mkRefl savable eqObj

        val lhsObj = Object.mkRefl savable lhsObj

        val obj0 = Object.mkAppThm savable eqObj objT

        val obj1 = Object.mkAppThm savable obj0 lhsObj
      in
        (Object.mkEqMp savable obj1 lhsObj, cvt)
      end
    | (Command.Trans,[objA,objB]) =>
      let
        val cvt = addStore cvt [objA]

        val tm0 = Term.rator (Thm.concl (Object.destThm objA))

        val (obj0,cvt) = buildTermStore tm0 cvt

        val obj1 = Object.mkRefl savable obj0

        val obj2 = Object.mkAppThm savable obj1 objB
      in
        (Object.mkEqMp savable obj2 objA, cvt)
      end
    | _ =>
      let
        val bug =
            "ObjectVersion.convert5: command " ^
            Command.toString cmd ^ "/" ^ Int.toString (length args) ^
            " not supported"
      in
        raise Bug bug
      end;

(* ------------------------------------------------------------------------- *)
(* Convert a command to article version 6.                                   *)
(* ------------------------------------------------------------------------- *)

fun convert6 cmd args res cvt =
    case (cmd,args) of
      (Command.DefineTypeOpLegacy,[objN,objA,objR,objV,objTh]) =>
      let
        val n = Object.destName objN
        and a = Object.destName objA
        and r = Object.destName objR

        val (objT,objA,objR,objAR,objRA) =
            Object.mkDefineTypeOp savable n a r objV objTh
      in
        case res of
          0 => (objT,cvt)
        | 1 => (objA,cvt)
        | 2 => (objR,cvt)
        | 3 =>
          let
            val cvt = addStore cvt [objTh,objT,objA,objR]

            val absRepTh = Object.destThm objAR

            val (_,aTm) = Term.destAbs (Term.rhs (Thm.concl absRepTh))

            val (objA,cvt) = buildTermStore aTm cvt

            val objA = Object.mkRefl savable objA

            val obj0 = Object.mkAppThm savable objAR objA

            val th0 = Object.destThm obj0

            val (tm0,rhsTm) = Term.destApp (Thm.concl th0)

            val (eqTm,lhsTm) = Term.destApp tm0

            val (objEq,cvt) = buildTermStore eqTm cvt

            val (objLhs,cvt) = buildTermStore lhsTm cvt

            val (objRhs,cvt) = buildTermStore rhsTm cvt

            val objEq = Object.mkRefl savable objEq

            val objLhs = Object.mkBetaConv savable objLhs

            val objRhs = Object.mkBetaConv savable objRhs

            val obj1 = Object.mkAppThm savable objEq objLhs

            val obj2 = Object.mkAppThm savable obj1 objRhs
          in
            (Object.mkEqMp savable obj2 obj0, cvt)
          end
        | 4 =>
          let
            val cvt = addStore cvt [objTh,objT,objA,objR]

            val repAbsTh = Object.destThm objRA

            val (_,tm0) = Term.destAbs (Term.lhs (Thm.concl repAbsTh))

            val rTm = Term.rhs tm0

            val (objR,cvt) = buildTermStore rTm cvt

            val objR = Object.mkRefl savable objR

            val obj0 = Object.mkAppThm savable objRA objR

            val th0 = Object.destThm obj0

            val (tm1,rhsTm) = Term.destApp (Thm.concl th0)

            val (iffTm,lhsTm) = Term.destApp tm1

            val (objIff,cvt) = buildTermStore iffTm cvt

            val (objLhs,cvt) = buildTermStore lhsTm cvt

            val (objRhs,cvt) = buildTermStore rhsTm cvt

            val objIff = Object.mkRefl savable objIff

            val objLhs = Object.mkBetaConv savable objLhs

            val objRhs = Object.mkBetaConv savable objRhs

            val obj1 = Object.mkAppThm savable objIff objLhs

            val obj2 = Object.mkAppThm savable obj1 objRhs

            val obj3 = Object.mkEqMp savable obj2 obj0
          in
            (Object.mkSym savable obj3, cvt)
          end
        | _ => raise Bug "ObjectVersion.convert6.DefineTypeOpLegacy"
      end
    | _ =>
      let
        val bug =
            "ObjectVersion.convert6: command " ^
            Command.toString cmd ^ "/" ^ Int.toString (length args) ^
            " not supported"
      in
        raise Bug bug
      end;

(* ------------------------------------------------------------------------- *)
(* Convert to a given article version: return NONE for unchanged.            *)
(* ------------------------------------------------------------------------- *)

local
  fun convertProvenance prov cvt =
      case prov of
        Object.Default => NONE
      | Object.Special {command = cmd, arguments = args, result = res, ...} =>
        let
          val ver = version cvt
        in
          if ArticleVersion.supported ver cmd then NONE
          else
            case ArticleVersion.toInt ver of
              5 => SOME (convert5 cmd args res cvt)
            | 6 => SOME (convert6 cmd args res cvt)
            | _ =>
              let
                val bug = "ObjectVersion.convertProvenance: bad article version"
              in
                raise Bug bug
              end
        end;

  fun preDescent objI cvt =
      case peekCache cvt objI of
        SOME objR' => {descend = false, result = (objR',cvt)}
      | NONE => {descend = true, result = (NONE,cvt)};

  fun postDescent objI objR' cvt =
      let
        val objR = Option.getOpt (objR',objI)

        val (objS',cvt) =
            case convertProvenance (Object.provenance objR) cvt of
              NONE => (objR',cvt)
            | SOME (objS,cvt) => (SOME objS, cvt)

        val cvt = insertCache cvt (objI,objS')
      in
        (objS',cvt)
      end;
in
  val sharingConvert =
      Object.maps
        {preDescent = preDescent,
         postDescent = postDescent,
         savable = true};
end;

fun convert cvt obj =
    let
      val (obj',_) = sharingConvert obj cvt
    in
      obj'
    end;

end
