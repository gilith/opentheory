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

fun isBool (Sequent {hyp,concl}) =
    Term.isBool concl andalso TermAlphaSet.isBool hyp;

fun checkBool seq =
    if isBool seq then ()
    else raise Error "sequent is not boolean";

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
(* Substitutions.                                                            *)
(* ------------------------------------------------------------------------- *)

fun sharingSubst seq sub =
    let
      val Sequent {hyp,concl} = seq

      val (hyp',sub) = TermSubst.sharingSubstAlphaSet hyp sub

      val (concl',sub) = TermSubst.sharingSubst concl sub

      val seq' =
          case (hyp',concl') of
            (SOME hyp, SOME concl) => SOME (Sequent {hyp = hyp, concl = concl})
          | (SOME hyp, NONE) => SOME (Sequent {hyp = hyp, concl = concl})
          | (NONE, SOME concl) => SOME (Sequent {hyp = hyp, concl = concl})
          | (NONE,NONE) => NONE
    in
      (seq',sub)
    end;

fun subst sub seq =
    let
      val (seq',_) = sharingSubst seq sub
    in
      seq'
    end;

(* ------------------------------------------------------------------------- *)
(* Rewrites.                                                                 *)
(* ------------------------------------------------------------------------- *)

fun sharingRewrite seq rewr =
    let
      val Sequent {hyp,concl} = seq

      val (hyp',rewr) = TermRewrite.sharingRewriteAlphaSet hyp rewr

      val (concl',rewr) = TermRewrite.sharingRewriteTerm concl rewr

      val seq' =
          case (hyp',concl') of
            (SOME hyp, SOME concl) => SOME (Sequent {hyp = hyp, concl = concl})
          | (SOME hyp, NONE) => SOME (Sequent {hyp = hyp, concl = concl})
          | (NONE, SOME concl) => SOME (Sequent {hyp = hyp, concl = concl})
          | (NONE,NONE) => NONE
    in
      (seq',rewr)
    end;

fun rewrite rewr seq =
    let
      val (seq',_) = sharingRewrite seq rewr
    in
      seq'
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {connective : Print.token,
       hypGrammar : Term.grammar,
       conclGrammar : Term.grammar,
       ppConnective : (sequent * Print.token) Print.pp,
       showHyp : bool};

val defaultGrammar =
    let
      val connective = "|-"
      and hypGrammar = Term.defaultGrammar
      and conclGrammar = Term.defaultGrammar
      and ppConnective = Print.ppMap snd Print.ppString
      and showHyp = true
    in
      Grammar
        {connective = connective,
         hypGrammar = hypGrammar,
         conclGrammar = conclGrammar,
         ppConnective = ppConnective,
         showHyp = showHyp}
    end;

local
  fun dots n = if n <= 5 then nChars #"." n else ".." ^ Int.toString n ^ "..";
in
  fun ppWithGrammar gram =
      let
        val Grammar
              {connective,
               hypGrammar,
               conclGrammar,
               ppConnective,
               showHyp} = gram

        val indent = size connective + 1

        val ppHypTermWS = Term.ppWithGrammar hypGrammar

        val ppConclWS = Term.ppWithGrammar conclGrammar
      in
        fn show =>
           let
             val ppHypTerm = ppHypTermWS show

             val ppHypElts =
                 if showHyp then
                   Print.ppMap TermAlphaSet.toList
                     (Print.ppOpList "," ppHypTerm)
                 else
                   Print.ppMap (dots o TermAlphaSet.size)
                     Print.ppString

             val ppHypSet = Print.ppBracket "{" "}" ppHypElts

             val ppConcl = ppConclWS show
           in
             fn seq =>
                let
                  val Sequent {hyp,concl} = seq

                  val ppConnectiveConcl =
                      Print.inconsistentBlock indent
                        [ppConnective (seq,connective),
                         Print.ppString " ",
                         ppConcl concl]
                in
                  if TermAlphaSet.null hyp then ppConnectiveConcl
                  else
                    Print.inconsistentBlock 0
                      [ppHypSet hyp,
                       Print.break,
                       ppConnectiveConcl]
                end
           end
      end;
end;

val ppWithShow = ppWithGrammar defaultGrammar;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val toHtmlConnective =
    let
      val conn = [Html.Entity "#8870"]
    in
      fn (_,c) =>
         case c of
           "-" => conn
         | _ => raise Bug "Sequent.toHtmlConnective"
    end;

val htmlGrammar =
    let
      val connective = "-"
      and hypGrammar = Term.htmlGrammar
      and conclGrammar = Term.htmlGrammar
      and ppConnective = Print.ppMap toHtmlConnective Html.ppFixed
      and showHyp = true
    in
      Grammar
        {connective = connective,
         hypGrammar = hypGrammar,
         conclGrammar = conclGrammar,
         ppConnective = ppConnective,
         showHyp = showHyp}
    end;

val ppHtml = ppWithGrammar htmlGrammar;

local
  val attrs = Html.singletonAttrs ("class","sequent");

  fun mkPara inlines = Html.Para (attrs,inlines);
in
  fun toHtmlWithGrammar grammar =
      let
        val ppHtml = ppWithGrammar grammar
      in
        fn show => mkPara o Html.toFixed (ppHtml show)
      end;
end;

val toHtml = toHtmlWithGrammar htmlGrammar;

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

local
  fun add (seq,(seqs,unchanged,sub)) =
      let
        val (seq',sub) = Sequent.sharingSubst seq sub

        val (seqs,unchanged) =
            case seq' of
              SOME seq => (seq :: seqs, false)
            | NONE => (seq :: seqs, unchanged)
      in
        (seqs,unchanged,sub)
      end;
in
  fun sharingSubst set sub =
      let
        val (seqs,unchanged,sub) = foldl add ([],true,sub) set

        val set' = if unchanged then NONE else SOME (fromList seqs)
      in
        (set',sub)
      end;
end;

fun subst sub set =
    let
      val (set',_) = sharingSubst set sub
    in
      set'
    end;

local
  fun add (seq,(seqs,unchanged,rewr)) =
      let
        val (seq',rewr) = Sequent.sharingRewrite seq rewr

        val (seqs,unchanged) =
            case seq' of
              SOME seq => (seq :: seqs, false)
            | NONE => (seq :: seqs, unchanged)
      in
        (seqs,unchanged,rewr)
      end;
in
  fun sharingRewrite set rewr =
      let
        val (seqs,unchanged,rewr) = foldl add ([],true,rewr) set

        val set' = if unchanged then NONE else SOME (fromList seqs)
      in
        (set',rewr)
      end;
end;

fun rewrite rewr set =
    let
      val (set',_) = sharingRewrite set rewr
    in
      set'
    end;

end
