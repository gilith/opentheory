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
      val tm = rator (concl th1)
      val th3 = comb (refl tm) th2
    in
      eqMp th3 th1
    end;

(* ------------------------------------------------------------------------- *)
(* Alpha conversion.                                                         *)
(* ------------------------------------------------------------------------- *)

fun alpha seq th =
    let
      val dealpha = TermSet.fromList o TermAlphaSet.toList

      fun norm th = (dealpha (hyp th), th)

      fun check (t,(ts,th)) =
          if TermSet.member t ts then (ts,th)
          else
            let
              val th0 = assume t
              val th1 = deductAntisym th0 th
              val th = eqMp th1 th0
            in
              norm th
            end

(*OpenTheoryTrace5
      val _ = Print.trace ppSequent "seq" seq
      val _ = Print.trace ppThm "th" th
*)
      val {concl = c, hyp = h} = seq
      val th = if Term.equal c (concl th) then th else eqMp (refl c) th
      val (_,th) = TermAlphaSet.foldl check (norm th) h
      val _ = Term.equal (concl th) c orelse
              raise Error "concl is wrong"
      val _ = TermSet.equal (dealpha h) (dealpha (hyp th)) orelse
              raise Error "hyp is wrong"
    in
      th
    end
    handle Error err => raise Error ("Rule.alpha: " ^ err);

fun findAlpha set seq =
    case ThmSet.peek set (Thm.axiom seq) of
      SOME th => SOME (alpha seq th)
    | NONE => NONE;

end
