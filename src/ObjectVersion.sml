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

fun buildTermStore tm = buildStore (ObjectData.Term tm);

(* ------------------------------------------------------------------------- *)
(* Convert a command to article version 5.                                   *)
(* ------------------------------------------------------------------------- *)

fun convert5 cmd args res cvt =
    raise Bug "ObjectVersion.convert5: not implemented";

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

            val (_,tm0) = Term.destAbs (Term.rhs (Thm.concl repAbsTh))

            val rTm = Term.rhs tm0

            val (objR,cvt) = buildTermStore rTm cvt

            val objR = Object.mkRefl savable objR

            val obj0 = Object.mkAppThm savable objRA objR

            val th0 = Object.destThm obj0

            val (guardTm,letTm) = Term.destApp (Thm.concl th0)

            val (objGuard,cvt) = buildTermStore guardTm cvt

            val (objLet,cvt) = buildTermStore letTm cvt

            val objGuard = Object.mkRefl savable objGuard

            val objLet = Object.mkBetaConv savable objLet

            val obj1 = Object.mkAppThm savable objGuard objLet
          in
            (Object.mkEqMp savable obj1 obj0, cvt)
          end
        | _ => raise Bug "ObjectVersion.convert6.DefineTypeOpLegacy"
      end
    | _ =>
      raise Bug "ObjectVersion.convert6: not implemented";

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
