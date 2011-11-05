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

end
