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
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

fun addSharingTypeOps (Sequent {hyp,concl}) share =
    let
      val share = TermAlphaSet.addSharingTypeOps hyp share
    in
      Term.addSharingTypeOps concl share
    end;

fun typeOps seq =
    let
      val share = Term.emptySharingTypeOps

      val share = addSharingTypeOps seq share
    in
      Term.toSetSharingTypeOps share
    end;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

fun addSharingConsts (Sequent {hyp,concl}) share =
    let
      val share = TermAlphaSet.addSharingConsts hyp share
    in
      Term.addSharingConsts concl share
    end;

fun consts seq =
    let
      val share = Term.emptySharingConsts

      val share = addSharingConsts seq share
    in
      Term.toSetSharingConsts share
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {connective : string,
       hypGrammar : Term.grammar,
       conclGrammar : Term.grammar,
       showHyp : bool};

val defaultGrammar =
    Grammar
      {connective = "?-",
       hypGrammar = Term.defaultGrammar,
       conclGrammar = Term.defaultGrammar,
       showHyp = false};

local
  fun dots n = if n <= 5 then nChars #"." n else ".." ^ Int.toString n ^ "..";
in
  fun ppWithGrammar gram =
      let
        val Grammar {connective,hypGrammar,conclGrammar,showHyp} = gram

        val connective_space = connective ^ " "
        val indent_space = size connective_space
        val space_connective = " " ^ connective

        val ppHypTermWS = Term.ppWithGrammar hypGrammar

        val ppConclWS = Term.ppWithGrammar conclGrammar
      in
        fn show =>
           let
             val ppHypTerm = ppHypTermWS show

             val ppHypSet =
                 if showHyp then
                   Print.ppMap TermAlphaSet.toList
                     (Print.ppOpList "," ppHypTerm)
                 else
                   Print.ppString o dots o TermAlphaSet.size

             val ppHyp = Print.ppBracket "{" "}" ppHypSet

             val ppConcl = ppConclWS show
           in
             fn Sequent {hyp,concl} =>
                if TermAlphaSet.null hyp then
                  Print.blockProgram Print.Inconsistent indent_space
                    [Print.ppString connective_space,
                     ppConcl concl]
                else
                  Print.block Print.Inconsistent 2
                    (Print.ppOp2 space_connective ppHyp ppConcl (hyp,concl))
           end
      end;
end;

val ppWithShow = ppWithGrammar defaultGrammar;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

fun ppHtml show =
    let
      val ppTerm = Term.ppHtml show

      val ppHyp =
          Print.ppBracket "{" "}"
            (Print.ppMap TermAlphaSet.toList
               (Print.ppOpList "," ppTerm))

      val ppTurnstyle = Html.ppFixed (Html.Entity "#8870")

      val ppConcl = ppTerm
    in
      fn Sequent {hyp,concl} =>
         if TermAlphaSet.null hyp then
           Print.blockProgram Print.Inconsistent 2
             [ppTurnstyle,
              Print.ppString " ",
              ppConcl concl]
         else
           Print.blockProgram Print.Inconsistent 2
             [ppHyp hyp,
              Print.ppString " ",
              ppTurnstyle,
              Print.addBreak 1,
              ppConcl concl]
    end;

end

structure SequentOrdered =
struct type t = Sequent.sequent val compare = Sequent.compare end

structure SequentMap = KeyMap (SequentOrdered)

structure SequentSet =
struct

  local
    structure S = ElementSet (SequentMap);
  in
    open S;
  end;

  local
    fun addSeq (seq,share) = Sequent.addSharingTypeOps seq share;
  in
    fun addSharingTypeOps set share = foldl addSeq share set;
  end;

  fun typeOps set =
      let
        val share = Term.emptySharingTypeOps

        val share = addSharingTypeOps set share
      in
        Term.toSetSharingTypeOps share
      end;

  local
    fun addSeq (seq,share) = Sequent.addSharingConsts seq share;
  in
    fun addSharingConsts set share = foldl addSeq share set;
  end;

  fun consts set =
      let
        val share = Term.emptySharingConsts

        val share = addSharingConsts set share
      in
        Term.toSetSharingConsts share
      end;

end
