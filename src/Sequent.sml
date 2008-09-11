(* ========================================================================= *)
(* HIGHER ORDER LOGIC SEQUENTS                                               *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Sequent :> Sequent =
struct

open Useful

(* ------------------------------------------------------------------------- *)
(* Sequents                                                                  *)
(* ------------------------------------------------------------------------- *)

type sequent = {hyp : TermAlphaSet.set, concl : Term.term};

(* ------------------------------------------------------------------------- *)
(* Checking the hypotheses and conclusion are of type bool                   *)
(* ------------------------------------------------------------------------- *)

fun boolean {hyp,concl} =
    Type.equal (Term.typeOf concl) Type.boolTy andalso
    TermAlphaSet.all (fn h => Type.equal (Term.typeOf h) Type.boolTy) hyp;

(* ------------------------------------------------------------------------- *)
(* A total order on sequents modulo alpha equivalence                        *)
(* ------------------------------------------------------------------------- *)

fun compare ({hyp = h1, concl = c1}, {hyp = h2, concl = c2}) =
    prodCompare Term.alphaCompare TermAlphaSet.compare ((c1,h1),(c2,h2));

fun equal s1 s2 = compare (s1,s2) = EQUAL;

end

structure SequentOrdered =
struct type t = Sequent.sequent val compare = Sequent.compare end

structure SequentSet = ElementSet (SequentOrdered)

structure SequentMap = KeyMap (SequentOrdered)
