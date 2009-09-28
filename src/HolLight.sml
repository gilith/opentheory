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

fun newBasicDefinition data =
    let
      val ObjectRead.SimulationData {target,...} = data
      val tm = Sequent.concl target
      val (t,def) = Term.destEq tm
      val (c,_) = Term.destConst t
      val n = Const.name c
    in
      Thm.defineConst n def
    end
    handle Error err =>
      raise Error ("HolLight.newBasicDefinition failed:\n" ^ err);

fun newBasicTypeDefinition data =
    let
      val ObjectRead.SimulationData {input,target,...} = data

      val (isAbsRepTh,abs,rep) =
          let
            val (l,r) = Term.destEq (Sequent.concl target)
          in
            case total Term.destEq r of
              SOME (t,_) =>
              let
                val (rep,t) = Term.destApp t
                val (abs,_) = Term.destApp t
              in
                (false,abs,rep)
              end
            | NONE =>
              let
                val (abs,t) = Term.destApp l
                val (rep,_) = Term.destApp t
              in
                (true,abs,rep)
              end
          end

      val (abs,absTy) = Term.destConst abs
      and (rep,_) = Term.destConst rep

      val abs = Const.name abs
      and rep = Const.name rep

      val (name,tyVars) =
          let
            val (_,ty) = Type.destFun absTy
            val (ot,tys) = Type.destOp ty
          in
            (TypeOp.name ot, map Type.destVar tys)
          end

      val (_,_,nonEmptyTh) = Object.destOtriple input
      val nonEmptyTh = Object.destOthm nonEmptyTh

      val (absRepTh,repAbsTh) =
          Thm.defineTypeOp name {abs = abs, rep = rep} tyVars nonEmptyTh
    in
      if isAbsRepTh then absRepTh else repAbsTh
    end
    handle Error err =>
      raise Error ("HolLight.newBasicTypeDefinition failed:\n" ^ err);

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference.                                             *)
(* ------------------------------------------------------------------------- *)

fun abs data =
    let
      val ObjectRead.SimulationData {input,...} = data
      val (otm,oth) = Object.destOpair input
      val v = Term.destVar (Object.destOterm otm)
      val th = Object.destOthm oth
    in
      Thm.abs v th
    end;

fun assume data =
    let
      val ObjectRead.SimulationData {input,...} = data
    in
      Thm.assume (Object.destOterm input)
    end;

fun beta data =
    let
      val ObjectRead.SimulationData {input,...} = data
    in
      Thm.betaConv (Object.destOterm input)
    end;

fun deductAntisymRule data =
    let
      val ObjectRead.SimulationData {input,...} = data
      val (oth1,oth2) = Object.destOpair input
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Thm.deductAntisym th1 th2
    end;

fun eqMp data =
    let
      val ObjectRead.SimulationData {input,...} = data
      val (oth1,oth2) = Object.destOpair input
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Thm.eqMp th1 th2
    end;

fun inst data =
    let
      val ObjectRead.SimulationData {input,...} = data
      val (oins,oth) = Object.destOpair input
      val ins = substToSubst oins
      val th = Object.destOthm oth
    in
      Thm.subst ins th
    end;

fun instType data =
    let
      val ObjectRead.SimulationData {input,...} = data
      val (oins,oth) = Object.destOpair input
      val ins = typeSubstToSubst oins
      val th = Object.destOthm oth
    in
      Thm.subst ins th
    end;

fun mkComb data =
    let
      val ObjectRead.SimulationData {input,...} = data
      val (oth1,oth2) = Object.destOpair input
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Thm.app th1 th2
    end;

fun refl data =
    let
      val ObjectRead.SimulationData {input,...} = data
    in
      Thm.refl (Object.destOterm input)
    end;

fun trans data =
    let
      val ObjectRead.SimulationData {input,...} = data
      val (oth1,oth2) = Object.destOpair input
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Rule.trans th1 th2
    end;

(* ------------------------------------------------------------------------- *)
(* Simulations.                                                              *)
(* ------------------------------------------------------------------------- *)

val simulations =
    List.foldl
      (fn ((s,f),m) => NameMap.insert m (Name.mk (namespace,s), f))
      (NameMap.new ())
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

end
