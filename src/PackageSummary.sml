(* ========================================================================= *)
(* THEORY PACKAGE SUMMARIES                                                  *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageSummary :> PackageSummary =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type tracking the source package of sequents.                           *)
(* ------------------------------------------------------------------------- *)

type sequentSource = PackageNameVersion.nameVersion SequentMap.map;

(* ------------------------------------------------------------------------- *)
(* A type of theory package summaries.                                       *)
(* ------------------------------------------------------------------------- *)

datatype summary' =
    Summary' of
      {summary : Summary.summary,
       requires : sequentSource,
       provides : sequentSource}

type summary = summary';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk sum : summary = sum;

fun dest sum : summary' = sum;

fun summary (Summary' {summary = x, ...}) = x;

fun requires sum = Summary.requires (summary sum);

fun provides sum = Summary.provides (summary sum);

(* ------------------------------------------------------------------------- *)
(* Check summary.                                                            *)
(* ------------------------------------------------------------------------- *)

fun check chks ctxt show sum = Summary.check chks ctxt show (summary sum);

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

fun toHtmlConnective class title (seq,c) =
    let
      val attrs =
          Html.fromListAttrs
            [("class", class),
             ("title", title seq)]

      val inlines = [Html.Entity "#8870"]

      val conn = Html.Span (attrs,inlines)
    in
      case c of
        "-" => [conn]
      | _ => raise Bug "PackageSummary.toHtmlConnective"
    end;

fun htmlGrammarSequent class title =
    let
      val Sequent.Grammar
            {connective = _,
             hypGrammar,
             conclGrammar,
             ppConnective = _,
             ppStandardAxiom,
             showHyp} = Sequent.htmlGrammar

      val connective = "-"

      val ppConnective =
          Print.ppMap (toHtmlConnective class title) Html.ppFixed
    in
      Sequent.Grammar
        {connective = connective,
         hypGrammar = hypGrammar,
         conclGrammar = conclGrammar,
         ppConnective = ppConnective,
         ppStandardAxiom = ppStandardAxiom,
         showHyp = showHyp}
    end;

fun mkTitle src text seq =
    let
      val pkg =
          case SequentMap.peek src seq of
            SOME n => PackageNameVersion.toString n
          | NONE => "this package"
    in
      text ^ " in " ^ pkg
    end;

fun htmlGrammar req prov =
    let
      val Summary.Grammar
            {ppTypeOp,
             ppConst,
             showTheoremAssumptions,...} = Summary.htmlGrammar

      val assumptionTitle = mkTitle req "Assumption made"
      and axiomTitle = mkTitle req "Axiom asserted"
      and theoremTitle = mkTitle prov "Theorem proved"

      val assumptionGrammar = htmlGrammarSequent "assumption" assumptionTitle
      and axiomGrammar = htmlGrammarSequent "axiom" axiomTitle
      and theoremGrammar = htmlGrammarSequent "theorem" theoremTitle
    in
      Summary.Grammar
        {assumptionGrammar = assumptionGrammar,
         axiomGrammar = axiomGrammar,
         theoremGrammar = theoremGrammar,
         ppTypeOp = ppTypeOp,
         ppConst = ppConst,
         showTheoremAssumptions = showTheoremAssumptions}
    end;

fun toHtml ctxt show sum =
    let
      val Summary' {summary,requires,provides} = dest sum

      val grammar = htmlGrammar requires provides
    in
      Summary.toHtmlWithGrammar grammar ctxt show summary
    end;

end
