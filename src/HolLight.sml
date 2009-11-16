(* ========================================================================= *)
(* SIMULATING THE HOL LIGHT THEOREM PROVER                                   *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure HolLight :> HolLight =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* The HOL Light namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

val namespace = Namespace.mkNested (Namespace.global,"hol-light");

fun mkName s = Name.mk (namespace,s);

(* ------------------------------------------------------------------------- *)
(* Converting data.                                                          *)
(* ------------------------------------------------------------------------- *)

fun typeSubstToSubst oins =
    let
      fun f (x,y) = (Type.destVar (Object.destOtype y), Object.destOtype x)

      val l = Object.destOlist oins
      val l = map (f o Object.destOpair) l

      val tyM = TypeSubst.fromListMap (rev l)

      val tmM = TermSubst.emptyTermMap
    in
      TermSubst.mk (tyM,tmM)
    end
    handle Error err =>
      raise Error ("HolLight.typeSubstToSubst failed:\n" ^ err);

fun substToSubst oins =
    let
      fun f (x,y) = (Term.destVar (Object.destOterm y), Object.destOterm x)

      val l = Object.destOlist oins
      val l = map (f o Object.destOpair) l

      val tyM = TypeSubst.emptyMap

      val tmM = TermSubst.fromListTermMap (rev l)
    in
      TermSubst.mk (tyM,tmM)
    end
    handle Error err =>
      raise Error ("HolLight.substToSubst failed:\n" ^ err);

(* ------------------------------------------------------------------------- *)
(* Primitive rules of definition.                                            *)
(* ------------------------------------------------------------------------- *)

fun newBasicDefinition ctxt =
    let
      val Simulation.Context {interpretation = int, input, ...} = ctxt

      val tm = Object.destOterm input

      val (v,def) = Term.destEq tm

      val (vn,ty) = Var.dest (Term.destVar v)

      val cn = Interpretation.interpretConst int vn

      val input =
          if Name.equal vn cn then NONE
          else
            let
              val v = Var.mk (cn,ty)

              val tm = Term.mkEq (Term.mkVar v, def)
            in
              SOME (Object.Oterm tm)
            end

      val thms =
          case total (Thm.defineConst cn) def of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun newBasicTypeDefinition ctxt =
    let
      val Simulation.Context {interpretation = int, input, ...} = ctxt

      val (tyName,absRepName,nonEmptyThOb) = Object.destOtriple input

      val tyName' = Object.destOname tyName
      and (absName,repName) = Object.destOpair absRepName
      and nonEmptyTh = Object.destOthm nonEmptyThOb

      val absName' = Object.destOname absName
      and repName' = Object.destOname repName

      val tyName = Interpretation.interpretTypeOp int tyName'
      and absName = Interpretation.interpretConst int absName'
      and repName = Interpretation.interpretConst int repName'

      val unchanged =
          Name.equal tyName tyName' andalso
          Name.equal absName absName' andalso
          Name.equal repName repName'

      val input =
          if unchanged then NONE
          else
            let
              val absN = Object.Oname absName
              and repN = Object.Oname repName

              val tyN = Object.Oname tyName
              and absRepN = Object.mkOpair (absN,repN)
            in
              SOME (Object.mkOtriple (tyN,absRepN,nonEmptyThOb))
            end

      val tyVars =
          let
            val (predTm,_) = Term.destApp (Thm.concl nonEmptyTh)
          in
            NameSet.toList (Term.typeVars predTm)
          end

      val thms =
          case total (Thm.defineTypeOp tyName {abs = absName, rep = repName}
                        tyVars) nonEmptyTh of
            SOME (absRepTh,repAbsTh) => ThmSet.fromList [absRepTh,repAbsTh]
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference.                                             *)
(* ------------------------------------------------------------------------- *)

fun abs ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val (otm,oth) = Object.destOpair input

      val v = Term.destVar (Object.destOterm otm)
      and th = Object.destOthm oth

      val input = NONE

      val thms =
          case total (Thm.abs v) th of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun assume ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val tm = Object.destOterm input

      val input = NONE

      val thms =
          case total Thm.assume tm of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun beta ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val tm = Object.destOterm input

      val input = NONE

      val thms =
          case total Thm.betaConv tm of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun deductAntisymRule ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val (oth1,oth2) = Object.destOpair input

      val th1 = Object.destOthm oth1
      and th2 = Object.destOthm oth2

      val input = NONE

      val thms =
          case total (Thm.deductAntisym th1) th2 of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun eqMp ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val (oth1,oth2) = Object.destOpair input

      val th1 = Object.destOthm oth1
      and th2 = Object.destOthm oth2

      val input = NONE

      val thms =
          case total (Thm.eqMp th1) th2 of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun inst ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val (oins,oth) = Object.destOpair input

      val ins = substToSubst oins
      and th = Object.destOthm oth

      val input = NONE

      val thms =
          case total (Thm.subst ins) th of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun instType ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val (oins,oth) = Object.destOpair input

      val ins = typeSubstToSubst oins
      and th = Object.destOthm oth

      val input = NONE

      val thms =
          case total (Thm.subst ins) th of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun mkComb ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val (oth1,oth2) = Object.destOpair input

      val th1 = Object.destOthm oth1
      and th2 = Object.destOthm oth2

      val input = NONE

      val thms =
          case total (Thm.app th1) th2 of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun refl ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val tm = Object.destOterm input

      val input = NONE

      val thms =
          case total Thm.refl tm of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

fun trans ctxt =
    let
      val Simulation.Context {input,...} = ctxt

      val (oth1,oth2) = Object.destOpair input

      val th1 = Object.destOthm oth1
      and th2 = Object.destOthm oth2

      val input = NONE

      val thms =
          case total (Rule.trans th1) th2 of
            SOME th => ThmSet.singleton th
          | NONE => ThmSet.empty
    in
      Simulation.Result
        {input = input,
         thms = thms}
    end;

(* ------------------------------------------------------------------------- *)
(* Simulations.                                                              *)
(* ------------------------------------------------------------------------- *)

val simulationList =
    [("new_basic_definition",newBasicDefinition),
     ("new_basic_type_definition",newBasicTypeDefinition),
     ("ABS",abs),
     ("ASSUME",assume),
     ("BETA",beta),
     ("DEDUCT_ANTISYM_RULE",deductAntisymRule),
     ("EQ_MP",eqMp),
     ("INST",inst),
     ("INST_TYPE",instType),
     ("MK_COMB",mkComb),
     ("REFL",refl),
     ("TRANS",trans)];

val simulations =
    let
      fun mk (s,f) = (mkName s, Simulation.Simulation f)
    in
      Simulation.fromList (map mk simulationList)
    end;

end
