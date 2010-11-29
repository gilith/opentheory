(* ========================================================================= *)
(* THEORY PACKAGE SUMMARIES                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageSummary :> PackageSummary =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type tracking the source package of sequents.                           *)
(* ------------------------------------------------------------------------- *)

type sequentSource = PackageName.name SequentMap.map;

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

(* ------------------------------------------------------------------------- *)
(* Check summary.                                                            *)
(* ------------------------------------------------------------------------- *)

fun check show sum = Summary.check show (summary sum);

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
        "-" => conn
      | _ => raise Bug "PackageSummary.toHtmlConnective"
    end;

fun htmlGrammarSequent class title =
    let
      val connective = "-"

      val hypGrammar = Term.htmlGrammar

      val conclGrammar = Term.htmlGrammar

      val ppConnective =
          Print.ppMap (toHtmlConnective class title) Html.ppFixed

      val showHyp = true
    in
      Sequent.Grammar
        {connective = connective,
         hypGrammar = hypGrammar,
         conclGrammar = conclGrammar,
         ppConnective = ppConnective,
         showHyp = showHyp}
    end;

fun mkTitle src text seq =
    let
      val pkg =
          case SequentMap.peek src seq of
            SOME n => PackageName.toString n
          | NONE => "this package"
    in
      text ^ " in " ^ pkg
    end;

fun htmlGrammar req prov =
    let
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
         theoremGrammar = theoremGrammar}
    end;

fun toHtml show sum =
    let
      val Summary' {summary,requires,provides} = dest sum

      val grammar = htmlGrammar requires provides
    in
      Summary.toHtmlWithGrammar grammar show summary
    end;

end
