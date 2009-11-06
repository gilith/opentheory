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
      fun destInput input =
          let
            val tm = Object.destOterm input

            val (v,def) = Term.destEq tm

            val vn = Var.name (Term.destVar v)
          in
            (vn,def)
          end

      fun mkDef int (vn,def) =
          let
            val cn = Interpretation.interpretConst int vn

            val th = Thm.defineConst cn def

            val (c,_) = Term.destConst (Term.lhs (Thm.concl th))
          in
            (c,th)
          end

      val mkTypeOp = Simulation.skipMkTypeOp

      fun mkConst ctxt n =
          let
            val Simulation.Context {interpretation = int, input, ...} = ctxt

            val def as (vn,_) = destInput input
          in
            if not (Name.equal vn n) then NONE
            else
              let
                val (c,_) = mkDef int def
              in
                SOME c
              end
          end

      fun mkThm ctxt _ =
          let
            val Simulation.Context {interpretation = int, input, ...} = ctxt

            val def = destInput input

            val (_,th) = mkDef int def
          in
            SOME th
          end
    in
      Simulation.Simulation
        {mkTypeOp = mkTypeOp,
         mkConst = mkConst,
         mkThm = mkThm}
    end;

val newBasicTypeDefinition =
    let
      fun destInput input =
          let
            val (tyName,absRepName,nonEmptyTh) = Object.destOtriple input

            val tyName = Object.destOname tyName
            and (absName,repName) = Object.destOpair absRepName
            and nonEmptyTh = Object.destOthm nonEmptyTh

            val absName = Object.destOname absName
            and repName = Object.destOname repName

            val (predTm,_) = Term.destApp (Thm.concl nonEmptyTh)

            val tyVars = NameSet.toList (Term.typeVars predTm)
          in
            (tyName,(absName,repName),tyVars,nonEmptyTh)
          end

      fun mkDef int (tyName,(absName,repName),tyVars,nonEmptyTh) =
          let
            val tyName = Interpretation.interpretTypeOp int tyName
            and absName = Interpretation.interpretConst int absName
            and repName = Interpretation.interpretConst int repName

            val (absRepTh,repAbsTh) =
                Thm.defineTypeOp
                  tyName {abs = absName, rep = repName} tyVars nonEmptyTh

            val (absTm,repTm) =
                let
                  val (l,r) = Term.destEq (Thm.concl absRepTh)

                  val (abs,t) = Term.destApp l
                  val (rep,_) = Term.destApp t
                in
                  (abs,rep)
                end

            val (absConst,absTy) = Term.destConst absTm
            and (repConst,_) = Term.destConst repTm

            val typeOp =
                let
                  val (_,ty) = Type.destFun absTy
                  val (ot,_) = Type.destOp ty
                in
                  ot
                end
          in
            (typeOp,(absConst,repConst),(absRepTh,repAbsTh))
          end

      fun mkTypeOp ctxt n =
          let
            val Simulation.Context {interpretation = int, input, ...} = ctxt

            val def as (tyName,_,_,_) = destInput input
          in
            if not (Name.equal n tyName) then NONE
            else
              let
                val (typeOp,_,_) = mkDef int def
              in
                SOME typeOp
              end
          end

      fun mkConst ctxt n =
          let
            val Simulation.Context {interpretation = int, input, ...} = ctxt

            val def as (_,(absName,repName),_,_) = destInput input
          in
            if Name.equal n absName then
              let
                val (_,(absConst,_),_) = mkDef int def
              in
                SOME absConst
              end
            else if Name.equal n repName then
              let
                val (_,(_,repConst),_) = mkDef int def
              in
                SOME repConst
              end
            else
              NONE
          end

      fun mkThm ctxt seq =
          let
            val Simulation.Context {interpretation = int, input, ...} = ctxt

            val def = destInput input

            val (_,_,(absRepTh,repAbsTh)) = mkDef int def

            val isAbsRepTh = Term.isEq (Term.rhs (Sequent.concl seq))
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
