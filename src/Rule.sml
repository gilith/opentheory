(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Rule :> Rule =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Transitivity of equality.                                                 *)
(* ------------------------------------------------------------------------- *)

fun trans th1 th2 =
    let
      val tm = Term.rator (Thm.concl th1)
      val th3 = Thm.app (Thm.refl tm) th2
    in
      Thm.eqMp th3 th1
    end;

(* ------------------------------------------------------------------------- *)
(* Alpha conversion.                                                         *)
(* ------------------------------------------------------------------------- *)

fun alpha seq th =
    let
      fun dealpha s = TermSet.fromList (TermAlphaSet.toList s)

      fun norm th = (dealpha (Thm.hyp th), th)

      fun check (t,(ts,th)) =
          if TermSet.member t ts then (ts,th)
          else
            let
              val th0 = Thm.assume t
              val th1 = Thm.deductAntisym th0 th
              val th = Thm.eqMp th1 th0
            in
              norm th
            end

(*OpenTheoryTrace5
      val _ = Print.trace Sequent.pp "seq" seq
      val _ = Print.trace Thm.pp "th" th
*)
      val Sequent.Sequent {concl = c, hyp = h} = seq

      val th = if Term.equal c (Thm.concl th) then th
               else Thm.eqMp (Thm.refl c) th

      val (_,th) = TermAlphaSet.foldl check (norm th) h

      val _ = Term.equal (Thm.concl th) c orelse
              raise Error "concl is wrong"
      val _ = TermSet.equal (dealpha h) (dealpha (Thm.hyp th)) orelse
              raise Error "hyp is wrong"
    in
      th
    end
    handle Error err => raise Error ("Rule.alpha: " ^ err);

fun findAlpha set seq =
    case ThmSet.peek set (Thm.axiom seq) of
      SOME th => SOME (alpha seq th)
    | NONE => NONE;

(* ------------------------------------------------------------------------- *)
(* Alpha conversion ignoring definitions of type operators and constants.    *)
(* ------------------------------------------------------------------------- *)

fun redefAlpha seq th =
    let
      val sym = Symbol.addSequent Symbol.empty (Thm.sequent th)

      val seq = Option.getOpt (Symbol.redefSequent sym seq, seq)
    in
      alpha seq th
    end;

end
