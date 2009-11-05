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

val newBasicDefinition =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      fun mkConst ctxt n =
          let
            val Simulation.Context {interpretation = int, input, ...} = ctxt

            val tm = Object.destOterm input

            val (t,def) = Term.destEq tm

            val vn = Var.name (Term.destVar t)
          in
            if not (Name.equal vn n) then NONE
            else
              let
                val cn = Interpretation.interpretConst int n

                val th = Thm.defineConst cn def

                val (c,_) = Term.destConst (Term.lhs (Thm.concl th))
              in
                SOME c
              end
          end

      fun mkThm _ seq =
          let
            val tm = Sequent.concl seq

            val (t,def) = Term.destEq tm

            val (c,_) = Term.destConst t

            val n = Const.name c
          in
            SOME (Thm.defineConst n def)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val newBasicTypeDefinition =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt seq =
          let
            val Simulation.Context {input,...} = ctxt

            val (isAbsRepTh,abs,rep) =
                let
                  val (l,r) = Term.destEq (Sequent.concl seq)
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
            SOME (if isAbsRepTh then absRepTh else repAbsTh)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference.                                             *)
(* ------------------------------------------------------------------------- *)

val abs =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt

            val (otm,oth) = Object.destOpair input

            val v = Term.destVar (Object.destOterm otm)
            and th = Object.destOthm oth
          in
            SOME (Thm.abs v th)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val assume =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt
          in
            SOME (Thm.assume (Object.destOterm input))
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val beta =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt
          in
            SOME (Thm.betaConv (Object.destOterm input))
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val deductAntisymRule =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt

            val (oth1,oth2) = Object.destOpair input

            val th1 = Object.destOthm oth1
            and th2 = Object.destOthm oth2
          in
            SOME (Thm.deductAntisym th1 th2)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val eqMp =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt

            val (oth1,oth2) = Object.destOpair input

            val th1 = Object.destOthm oth1
            and th2 = Object.destOthm oth2
          in
            SOME (Thm.eqMp th1 th2)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val inst =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt

            val (oins,oth) = Object.destOpair input

            val ins = substToSubst oins
            and th = Object.destOthm oth
          in
            SOME (Thm.subst ins th)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val instType =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt

            val (oins,oth) = Object.destOpair input

            val ins = typeSubstToSubst oins
            and th = Object.destOthm oth
          in
            SOME (Thm.subst ins th)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val mkComb =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt

            val (oth1,oth2) = Object.destOpair input

            val th1 = Object.destOthm oth1
            and th2 = Object.destOthm oth2
          in
            SOME (Thm.app th1 th2)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val refl =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt
          in
            SOME (Thm.refl (Object.destOterm input))
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val trans =
    let
      val mkTypeOp = Simulation.skipMkTypeOp

      val mkConst = Simulation.skipMkConst

      fun mkThm ctxt _ =
          let
            val Simulation.Context {input,...} = ctxt

            val (oth1,oth2) = Object.destOpair input

            val th1 = Object.destOthm oth1
            and th2 = Object.destOthm oth2
          in
            SOME (Rule.trans th1 th2)
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
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
