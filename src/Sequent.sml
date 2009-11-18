(* ========================================================================= *)
(* HIGHER ORDER LOGIC SEQUENTS                                               *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Sequent :> Sequent =
struct

open Useful

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic sequents.                                    *)
(* ------------------------------------------------------------------------- *)

datatype sequent =
    Sequent of
      {hyp : TermAlphaSet.set,
       concl : Term.term}

fun hyp (Sequent {hyp = x, ...}) = x;

fun concl (Sequent {concl = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Checking the hypotheses and conclusion are of type bool.                  *)
(* ------------------------------------------------------------------------- *)

fun boolean (Sequent {hyp,concl}) =
    Type.equal (Term.typeOf concl) Type.bool andalso
    TermAlphaSet.all (fn h => Type.equal (Term.typeOf h) Type.bool) hyp;

(* ------------------------------------------------------------------------- *)
(* A total order on sequents modulo alpha equivalence.                       *)
(* ------------------------------------------------------------------------- *)

fun compare (s1,s2) =
    if Portable.pointerEqual (s1,s2) then EQUAL
    else
      let
        val Sequent {hyp = h1, concl = c1} = s1
        and Sequent {hyp = h2, concl = c2} = s2
      in
        case Term.alphaCompare (c1,c2) of
          LESS => LESS
        | EQUAL => TermAlphaSet.compare (h1,h2)
        | GREATER => GREATER
      end;

fun equal s1 s2 = compare (s1,s2) = EQUAL;

fun dealphaCompare (s1,s2) =
    if Portable.pointerEqual (s1,s2) then EQUAL
    else
      let
        val Sequent {hyp = h1, concl = c1} = s1
        and Sequent {hyp = h2, concl = c2} = s2
      in
        case Term.compare (c1,c2) of
          LESS => LESS
        | EQUAL => TermAlphaSet.dealphaCompare (h1,h2)
        | GREATER => GREATER
      end;

fun dealphaEqual s1 s2 = dealphaCompare (s1,s2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Type operators and constants.                                             *)
(* ------------------------------------------------------------------------- *)

fun typeOps (Sequent {hyp,concl}) =
    TypeOpSet.union (TermAlphaSet.typeOps hyp) (Term.typeOps concl);

fun consts (Sequent {hyp,concl}) =
    ConstSet.union (TermAlphaSet.consts hyp) (Term.consts concl);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun dots n = if n <= 5 then nChars #"." n else ".." ^ Int.toString n ^ "..";
in
  fun ppGen {showHyp, connective} =
      let
        val connective_space = connective ^ " "
        val indent_space = size connective_space
        val space_connective = " " ^ connective
      in
        fn Sequent {hyp,concl} =>
           if TermAlphaSet.null hyp then
             Print.blockProgram Print.Inconsistent indent_space
               [Print.addString connective_space,
                Term.pp concl]
           else
             Print.block Print.Inconsistent 2
               (Print.ppOp2 space_connective
                  (Print.ppBracket "{" "}"
                     (if showHyp then
                        (Print.ppMap TermAlphaSet.toList
                           (Print.ppOpList "," Term.pp))
                      else
                        Print.ppMap (dots o TermAlphaSet.size)
                          Print.ppString))
                  Term.pp (hyp,concl))
      end;
end;

val showHyp = ref false;

fun pp seq = ppGen {showHyp = !showHyp, connective = "?-"} seq;

val toString = Print.toString pp;

end

structure SequentOrdered =
struct type t = Sequent.sequent val compare = Sequent.compare end

structure SequentSet =
struct

  local
    structure S = ElementSet (SequentOrdered);
  in
    open S;
  end;

  val typeOps =
      let
        fun addNames (tm,acc) = TypeOpSet.union acc (Sequent.typeOps tm)
      in
        foldl addNames TypeOpSet.empty
      end;

  val consts =
      let
        fun addNames (tm,acc) = ConstSet.union acc (Sequent.consts tm)
      in
        foldl addNames ConstSet.empty
      end;

end

structure SequentMap = KeyMap (SequentOrdered)
