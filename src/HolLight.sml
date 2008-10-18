(* ========================================================================= *)
(* SIMULATING THE HOL LIGHT THEOREM PROVER                                   *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
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
      val tyM = TypeSubst.fromListMap l
      val tmM = TermSubst.emptyTermMap
    in
      TermSubst.mk (tyM,tmM)
    end
    handle Error err =>
      raise Bug ("HolLight.typeSubstToSubst failed:\n" ^ err);

fun substToSubst oins =
    let
      fun f (x,y) = (Term.destVar (Object.destOterm y), Object.destOterm x)
      val l = Object.destOlist oins
      val l = map (f o Object.destOpair) l
      val tyM = TypeSubst.emptyMap
      val tmM = TermSubst.fromListTermMap l
    in
      TermSubst.mk (tyM,tmM)
    end
    handle Error err =>
      raise Bug ("HolLight.substToSubst failed:\n" ^ err);

(* ------------------------------------------------------------------------- *)
(* Primitive rules of definition.                                            *)
(* ------------------------------------------------------------------------- *)

fun newBasicDefinition _ seq _ =
    let
      val {concl = tm, ...} : Sequent.sequent = seq
      val (c,t) = Term.destEq tm
      val (n,ty) = Term.destConst c
      val v = Var.Var (n,ty)
      val tm = Term.mkEq (Term.mkVar v, t)
    in
      Object.Othm (Rule.define tm)
    end
    handle Error err =>
      raise Bug ("HolLight.newBasicDefinition failed:\n" ^ err);

fun newBasicTypeDefinition _ seq arg =
    let
      val (abs,rep) =
          let
            val {concl = tm,...} = seq
            val (l,r) = Term.destEq tm
          in
            case total Term.destEq r of
              SOME (t,_) =>
              let
                val (rep,t) = Term.destComb t
                val (abs,_) = Term.destComb t
              in
                (abs,rep)
              end
            | NONE =>
              let
                val (abs,t) = Term.destComb l
                val (rep,_) = Term.destComb t
              in
                (abs,rep)
              end
          end

      val (abs,absTy) = Term.destConst abs
      and (rep,_) = Term.destConst rep

      val name =
          let
            val (_,ty) = Type.destFun absTy
            val (name,_) = Type.destOp ty
          in
            name
          end

      val (_,_,nonEmptyTh) = Object.destOtriple arg
      val nonEmptyTh = Object.destOthm nonEmptyTh

      val tyVars = NameSet.toList (Term.typeVars (Syntax.concl nonEmptyTh))

      val (absRepTh,repAbsTh) =
          Rule.defineType name {abs = abs, rep = rep} tyVars nonEmptyTh
    in
      Object.mkOpair (Object.Othm absRepTh, Object.Othm repAbsTh)
    end
    handle Error err =>
      raise Bug ("HolLight.newBasicTypeDefinition failed:\n" ^ err);

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference.                                             *)
(* ------------------------------------------------------------------------- *)

fun abs _ _ arg =
    let
      val (otm,oth) = Object.destOpair arg
      val v = Term.destVar (Object.destOterm otm)
      val th = Object.destOthm oth
    in
      Object.Othm (Rule.abs v th)
    end;

fun assume _ _ arg = Object.Othm (Rule.assume (Object.destOterm arg));

fun beta _ _ arg = Object.Othm (Rule.betaConv (Object.destOterm arg));

fun deductAntisymRule _ _ arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (Rule.deductAntisym th1 th2)
    end;

fun eqMp _ _ arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (Rule.eqMp th1 th2)
    end;

fun inst _ _ arg =
    let
      val (oins,oth) = Object.destOpair arg
      val ins = substToSubst oins
      val th = Object.destOthm oth
    in
      Object.Othm (Rule.subst ins th)
    end;

fun instType _ _ arg =
    let
      val (oins,oth) = Object.destOpair arg
      val ins = typeSubstToSubst oins
      val th = Object.destOthm oth
    in
      Object.Othm (Rule.subst ins th)
    end;

fun mkComb _ _ arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (Rule.comb th1 th2)
    end;

fun refl _ _ arg = Object.Othm (Rule.refl (Object.destOterm arg));

fun trans _ _ arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (Rule.trans th1 th2)
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
