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
    in
      TermSubst.fromListType (map (f o Object.destOpair) l)
    end
    handle Error err =>
      raise Bug ("holLightTypeSubstToSubst failed:\n" ^ err);

fun substToSubst oins =
    let
      fun f (x,y) = (Term.destVar (Object.destOterm y), Object.destOterm x)
      val l = Object.destOlist oins
    in
      TermSubst.fromList (map (f o Object.destOpair) l)
    end
    handle Error err =>
      raise Bug ("holLightSubstToSubst failed:\n" ^ err);

(* ------------------------------------------------------------------------- *)
(* Primitive rules of definition.                                            *)
(* ------------------------------------------------------------------------- *)

fun newBasicDefinition interpretation arg =
    let
      val tm = Object.destOterm arg
      val (v,t) = Term.destEq tm
      val (n,ty) = Term.destVar v
      val n = Interpretation.interpretConst interpretation n
      val v = Term.mkVar (n,ty)
      val tm = Term.mkEq (v,t)
    in
      Object.Othm (Rule.define tm)
    end
    handle Error err =>
      raise Bug ("holLightNewBasicDefinition failed:\n" ^ err);

fun newBasicTypeDefinition interpretation arg =
    let
      val (name,absRep,nonEmptyTh) = Object.destOtriple arg
      val name = Object.destOname name
      val name = Interpretation.interpretType interpretation name
      val (abs,rep) = Object.destOpair absRep
      val abs = Object.destOname abs
      and rep = Object.destOname rep
      and nonEmptyTh = Object.destOthm nonEmptyTh
      val abs = Interpretation.interpretConst interpretation abs
      and rep = Interpretation.interpretConst interpretation rep
      val tyVars = NameSet.toList (Term.typeVars (Syntax.concl nonEmptyTh))
      val (absRepTh,repAbsTh) =
          Rule.defineType name {abs = abs, rep = rep} tyVars nonEmptyTh
    in
      Object.mkOpair (Object.Othm absRepTh, Object.Othm repAbsTh)
    end
    handle Error err =>
      raise Bug ("holLightNewBasicTypeDefinition failed:\n" ^ err);

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference.                                             *)
(* ------------------------------------------------------------------------- *)

fun abs _ arg =
    let
      val (otm,oth) = Object.destOpair arg
      val v = Term.destVar (Object.destOterm otm)
      val th = Object.destOthm oth
    in
      Object.Othm (Rule.abs v th)
    end;

fun assume _ arg = Object.Othm (Rule.assume (Object.destOterm arg));

fun beta _ arg = Object.Othm (Rule.betaConv (Object.destOterm arg));

fun deductAntisymRule _ arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (Rule.deductAntisym th1 th2)
    end;

fun eqMp _ arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (Rule.eqMp th1 th2)
    end;

fun inst _ arg =
    let
      val (oins,oth) = Object.destOpair arg
      val ins = substToSubst oins
      val th = Object.destOthm oth
    in
      Object.Othm (Rule.subst ins th)
    end;

fun instType _ arg =
    let
      val (oins,oth) = Object.destOpair arg
      val ins = typeSubstToSubst oins
      val th = Object.destOthm oth
    in
      Object.Othm (Rule.subst ins th)
    end;

fun mkComb _ arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (Rule.comb th1 th2)
    end;

fun refl _ arg = Object.Othm (Rule.refl (Object.destOterm arg));

fun trans _ arg =
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
