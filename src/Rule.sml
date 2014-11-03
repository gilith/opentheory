(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Rule :> Rule =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Applying equalities at subterms.                                          *)
(* ------------------------------------------------------------------------- *)

fun rator th tm =
    Thm.app th (Thm.refl tm)
(*OpenTheoryDebug
    handle Error err =>
      let
        val err = "Rule.rator: " ^ err
      in
        raise Error err
      end;
*)

fun rand tm th =
    Thm.app (Thm.refl tm) th
(*OpenTheoryDebug
    handle Error err =>
      let
        val err = "Rule.rand: " ^ err
      in
        raise Error err
      end;
*)

(* ------------------------------------------------------------------------- *)
(* Symmetry of equality.                                                     *)
(* ------------------------------------------------------------------------- *)

fun sym th =
    let
      val (eqTm,lTm) = Term.destApp (Term.rator (Thm.concl th))

      val th0 = Thm.refl lTm

      val th1 = Thm.app (rand eqTm th) th0
    in
      Thm.eqMp th1 th0
    end
(*OpenTheoryDebug
    handle Error err =>
      let
        val err = "Rule.sym: " ^ err
      in
        raise Error err
      end;
*)

(* ------------------------------------------------------------------------- *)
(* Transitivity of equality.                                                 *)
(* ------------------------------------------------------------------------- *)

fun trans th1 th2 =
    let
      val tm = Term.rator (Thm.concl th1)

      val th3 = rand tm th2
    in
      Thm.eqMp th3 th1
    end
(*OpenTheoryDebug
    handle Error err =>
      let
        val err = "Rule.trans: " ^ err
      in
        raise Error err
      end;
*)

(* ------------------------------------------------------------------------- *)
(* Proving hypotheses.                                                       *)
(* ------------------------------------------------------------------------- *)

fun proveHyp th1 th2 =
    let
      val th3 = Thm.deductAntisym th1 th2
    in
      Thm.eqMp th3 th1
    end
(*OpenTheoryDebug
    handle Error err =>
      let
        val err = "Rule.proveHyp: " ^ err
      in
        raise Error err
      end;
*)

(* ------------------------------------------------------------------------- *)
(* Alpha conversion.                                                         *)
(* ------------------------------------------------------------------------- *)

fun alpha seq th =
    let
      val hs = TermSet.fromList (TermAlphaSet.toList (Thm.hyp th))

      fun check (h,th) =
          if TermSet.member h hs then th
          else
            let
              val th0 = Thm.assume h

              val th1 = Thm.deductAntisym th0 th
            in
              Thm.eqMp th1 th0
            end

(*OpenTheoryTrace5
      val _ = Print.trace Sequent.pp "seq" seq
      val _ = Print.trace Thm.pp "th" th
*)

      val Sequent.Sequent {hyp = h, concl = c} = seq

      val th =
          if Term.equal c (Thm.concl th) then th
          else Thm.eqMp (Thm.refl c) th

      val th = TermAlphaSet.foldl check th h
    in
      if Sequent.dealphaEqual (Thm.sequent th) seq then th
      else raise Error "not alpha-equivalent"
    end
(*OpenTheoryDebug
    handle Error err =>
      let
        val err = "seq = " ^ Sequent.toString seq ^ "\n" ^
                  "th = " ^ Thm.toString th ^ "\n" ^
                  "Rule.alpha: " ^ err
      in
        raise Error err
      end;
*)

fun findAlpha set seq =
    case ThmSet.peek set (Thm.axiom seq) of
      SOME th => SOME (alpha seq th)
    | NONE => NONE;

(* ------------------------------------------------------------------------- *)
(* The new principle of constant definition.                                 *)
(* ------------------------------------------------------------------------- *)

fun defineConstList nvs th =
    let
      fun add (tm,vm) =
          let
            val (v,tm) = Term.destEq tm

            val v = Term.destVar v

            val () =
                if not (VarMap.inDomain v vm) then ()
                else raise Error "repeated vars in assumptions"
          in
            VarMap.insert vm (v,tm)
          end

      fun del (n,v) vm =
          let
            val tm =
                case VarMap.peek vm v of
                  SOME tm => tm
                | NONE => raise Error "given var not in assumptions"

            val vm = VarMap.delete vm v

            val (c,def) = Thm.defineConst n tm

            val vc = (v, Term.mkConst (c, Var.typeOf v))
          in
            ((c,(vc,def)),vm)
          end

      val vm = TermAlphaSet.foldl add (VarMap.new ()) (Thm.hyp th)

      val () =
          let
            val vs = Term.freeVars (Thm.concl th)
          in
            if VarSet.subset vs (VarSet.domain vm) then ()
            else raise Error "additional free vars in definition theorem"
          end

      val (cs,sub,defs,vm) =
          let
            val (cs_vcs_defs,vm) = maps del nvs vm

            val (cs,vcs_defs) = unzip cs_vcs_defs

            val (vcs,defs) = unzip vcs_defs

            val sub = TermSubst.mkMono (TermSubst.fromListMap vcs)
          in
            (cs,sub,defs,vm)
          end

      val () =
          if VarMap.null vm then ()
          else raise Error "additional assumptions in definition theorem"

      val th = Thm.subst sub th

      val th = List.foldl (uncurry proveHyp) th defs
    in
      (cs,th)
    end
(*OpenTheoryDebug
    handle Error err =>
      let
        val err = "Rule.defineConstList: " ^ err
      in
        raise Error err
      end;
*)

(* ------------------------------------------------------------------------- *)
(* The legacy (a.k.a. HOL Light) version of defineTypeOp.                    *)
(* ------------------------------------------------------------------------- *)

fun defineTypeOpLegacy name abs rep tyVars existenceTh =
    let
      val (ot,absC,repC,absRepTh,repAbsTh) =
          Thm.defineTypeOp name abs rep tyVars existenceTh

      val absRepTh' =
          let
            val (_,aTm) = Term.destAbs (Term.rhs (Thm.concl absRepTh))

            val th0 = rator absRepTh aTm

            val (tm0,rhsTm) = Term.destApp (Thm.concl th0)

            val (eqTm,lhsTm) = Term.destApp tm0

            val th1 = rand eqTm (Thm.betaConv lhsTm)

            val th2 = Thm.app th1 (Thm.betaConv rhsTm)
          in
            Thm.eqMp th2 th0
          end

      val repAbsTh' =
          let
            val (_,tm0) = Term.destAbs (Term.lhs (Thm.concl repAbsTh))

            val rTm = Term.rhs tm0

            val th0 = rator repAbsTh rTm

            val (tm1,rhsTm) = Term.destApp (Thm.concl th0)

            val (iffTm,lhsTm) = Term.destApp tm1

            val th1 = rand iffTm (Thm.betaConv lhsTm)

            val th2 = Thm.app th1 (Thm.betaConv rhsTm)
          in
            sym (Thm.eqMp th2 th0)
          end
    in
      (ot,absC,repC,absRepTh',repAbsTh')
    end
(*OpenTheoryDebug
    handle Error err =>
      let
        val err = "Rule.defineTypeOpLegacy: " ^ err
      in
        raise Error err
      end;
*)

end
