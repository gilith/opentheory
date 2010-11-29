(* ========================================================================= *)
(* THEORY SUMMARIES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Summary :> Summary =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory summaries.                                               *)
(* ------------------------------------------------------------------------- *)

datatype summary' =
    Summary' of
      {requires : Sequents.sequents,
       provides : Sequents.sequents};

type summary = summary'

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk sum : summary = sum;

fun dest sum : summary' = sum;

fun requires' (Summary' {requires = x, ...}) = x;

fun provides' (Summary' {provides = x, ...}) = x;

fun requires sum = requires' (dest sum);

fun provides sum = provides' (dest sum);

fun fromThms ths =
    let
      val req = Sequents.fromSet (ThmSet.axioms (Thms.thms ths))
      and prov = Sequents.fromThms ths

      val sum' =
          Summary'
            {requires = req,
             provides = prov}
    in
      mk sum'
    end;

(* ------------------------------------------------------------------------- *)
(* Substitutions.                                                            *)
(* ------------------------------------------------------------------------- *)

fun sharingSubst sum sub =
    let
      val Summary' {requires = req, provides = prov} = dest sum

      val (req',sub) = Sequents.sharingSubst req sub

      val (prov',sub) = Sequents.sharingSubst prov sub

      val sum' =
          case (req',prov') of
            (SOME req, SOME prov) =>
            SOME (mk (Summary' {requires = req, provides = prov}))
          | (SOME req, NONE) =>
            SOME (mk (Summary' {requires = req, provides = prov}))
          | (NONE, SOME prov) =>
            SOME (mk (Summary' {requires = req, provides = prov}))
          | (NONE,NONE) =>
            NONE
    in
      (sum',sub)
    end;

fun subst sub sum =
    let
      val (sum',_) = sharingSubst sum sub
    in
      sum'
    end;

(* ------------------------------------------------------------------------- *)
(* Rewrites.                                                                 *)
(* ------------------------------------------------------------------------- *)

fun sharingRewrite sum rewr =
    let
      val Summary' {requires = req, provides = prov} = dest sum

      val (req',rewr) = Sequents.sharingRewrite req rewr

      val (prov',rewr) = Sequents.sharingRewrite prov rewr

      val sum' =
          case (req',prov') of
            (SOME req, SOME prov) =>
            SOME (mk (Summary' {requires = req, provides = prov}))
          | (SOME req, NONE) =>
            SOME (mk (Summary' {requires = req, provides = prov}))
          | (NONE, SOME prov) =>
            SOME (mk (Summary' {requires = req, provides = prov}))
          | (NONE,NONE) =>
            NONE
    in
      (sum',rewr)
    end;

fun rewrite rewr sum =
    let
      val (sum',_) = sharingRewrite sum rewr
    in
      sum'
    end;

(* ------------------------------------------------------------------------- *)
(* A type of theory summary information (for pretty printing).               *)
(* ------------------------------------------------------------------------- *)

datatype info =
    Info of
      {input : Symbol.symbol,
       assumed : SequentSet.set,
       defined : Symbol.symbol,
       axioms : SequentSet.set,
       thms : SequentSet.set};

local
  fun allSymbolsIn sym =
      let
        val ots = Symbol.typeOps sym
        and cs = Symbol.consts sym
      in
        fn seq =>
           TypeOpSet.subset (Sequent.typeOps seq) ots andalso
           ConstSet.subset (Sequent.consts seq) cs
      end;
in
  fun toInfo summary =
      let
(*OpenTheoryTrace5
        val () = trace "entering Summary.toInfo\n"
*)
        val Summary' {requires,provides} = dest summary

        val {undefined = inp, defined = def} =
            let
              val req = Sequents.symbol requires
              and prov = Sequents.symbol provides

              val sym = Symbol.union req prov
            in
              Symbol.partitionUndef sym
            end

        val (ass,ax) =
            let
              val req = Sequents.sequents requires
            in
              SequentSet.partition (allSymbolsIn inp) req
            end

        val ths = Sequents.sequents provides

(*OpenTheoryTrace5
        val () = trace "exiting Summary.toInfo\n"
*)
      in
        Info
          {input = inp,
           assumed = ass,
           defined = def,
           axioms = ax,
           thms = ths}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {assumptionGrammar : Sequent.grammar,
       axiomGrammar : Sequent.grammar,
       theoremGrammar : Sequent.grammar};

val defaultGrammar =
    let
      val assumptionGrammar = Sequent.defaultGrammar
      and axiomGrammar = Sequent.defaultGrammar
      and theoremGrammar = Sequent.defaultGrammar
    in
      Grammar
        {assumptionGrammar = assumptionGrammar,
         axiomGrammar = axiomGrammar,
         theoremGrammar = theoremGrammar}
    end;

local
  fun ppList ppX prefix name xs =
      if null xs then Print.skip
      else
        Print.sequence
          (Print.blockProgram Print.Inconsistent 2
             (Print.ppString prefix ::
              Print.ppString name ::
              Print.ppString ":" ::
              map (Print.sequence (Print.addBreak 1) o ppX) xs))
          Print.addNewline;

  fun ppSequentSet ppSeq (name,seqs) =
      let
        val seqs = SequentSet.toList seqs
      in
        Print.blockProgram Print.Consistent 2
          (Print.ppString name ::
           Print.ppString ":" ::
           List.map (Print.sequence Print.addNewline o ppSeq) seqs)
      end;
in
  fun ppInfoWithShow ppAssumptionWS ppAxiomWS ppTheoremWS show =
      let
        val ppTypeOp = TypeOp.ppWithShow show

        val ppConst = Const.ppWithShow show

        val ppAssumption = ppAssumptionWS show

        val ppAxiom = ppAxiomWS show

        val ppTheorem = ppTheoremWS show

        fun ppSymbol (prefix,sym) =
            let
              val ots = TypeOpSet.toList (Symbol.typeOps sym)
              and cs = ConstSet.toList (Symbol.consts sym)
            in
              Print.sequence
                (ppList ppTypeOp prefix "-types" ots)
                (ppList ppConst prefix "-consts" cs)
            end
      in
        fn info =>
           let
             val Info {input,assumed,defined,axioms,thms} = info
           in
             Print.blockProgram Print.Consistent 0
               [ppSymbol ("input",input),
                ppSequentSet ppAssumption ("assumptions",assumed),
                Print.addNewline,
                ppSymbol ("defined",defined),
                ppSequentSet ppAxiom ("axioms",axioms),
                Print.addNewline,
                ppSequentSet ppTheorem ("theorems",thms)]
           end
      end;
end;

fun ppInfoWithGrammar grammar =
    let
      val Grammar
            {assumptionGrammar,
             axiomGrammar,
             theoremGrammar} = grammar

      val ppAssumptionWS = Sequent.ppWithGrammar assumptionGrammar

      val ppAxiomWS = Sequent.ppWithGrammar axiomGrammar

      val ppTheoremWS = Sequent.ppWithGrammar theoremGrammar
    in
      ppInfoWithShow ppAssumptionWS ppAxiomWS ppTheoremWS
    end;

fun ppWithGrammar grammar =
    let
      val ppIWS = ppInfoWithGrammar grammar
    in
      fn show => Print.ppMap toInfo (ppIWS show)
    end;

val ppWithShow = ppWithGrammar defaultGrammar;

fun toTextFile {show,summary,filename} =
    let
(*OpenTheoryTrace5
      val () = trace "entering Summary.toTextFile\n"
*)
      val lines = Print.toStream (ppWithShow show) summary

      val () = Stream.toTextFile {filename = filename} lines

(*OpenTheoryTrace5
      val () = trace "exiting Summary.toTextFile\n"
*)
    in
      ()
    end;

val pp = ppWithShow Show.default;

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

fun toHtmlConnective class title =
    let
      val attrs =
          Html.fromListAttrs
            [("class",class),
             ("title",title)]

      val inlines = [Html.Entity "#8870"]

      val conn = Html.Span (attrs,inlines)
    in
      fn (_,c) =>
         case c of
           "-" => conn
         | _ => raise Bug "Summary.toHtmlConnective"
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

val htmlGrammar =
    let
      val assumptionGrammar = htmlGrammarSequent "assumption" "Assumption"
      and axiomGrammar = htmlGrammarSequent "axiom" "Axiom"
      and theoremGrammar = htmlGrammarSequent "theorem" "Theorem"
    in
      Grammar
        {assumptionGrammar = assumptionGrammar,
         axiomGrammar = axiomGrammar,
         theoremGrammar = theoremGrammar}
    end;

fun toHtmlInfo toHtmlAssumptionWS toHtmlAxiomWS toHtmlTheoremWS show =
    let
      val toHtmlAssumption = toHtmlAssumptionWS show

      val toHtmlAxiom = toHtmlAxiomWS show

      val toHtmlTheorem = toHtmlTheoremWS show

      val toHtmlNames =
          let
            fun dest name =
                let
                  val (ns,n) = Name.dest name
                in
                  (Namespace.toList ns, n)
                end

            fun toHtmlName name = Name.toHtml (Name.mkGlobal name)

            fun toItem flows = Html.ListItem (Html.emptyAttrs,flows)

            fun toItemName name = toItem [Html.Inline (toHtmlName name)]

            fun toUlist names =
                let
                  val (tops,subs) = List.partition (null o fst) names

                  val items =
                      List.map (toItemName o snd) tops @
                      List.map toItem (categorize subs)
                in
                  Html.Ulist (Html.emptyAttrs,items)
                end

            and categorize subs =
                case subs of
                  [] => []
                | (ns,n) :: subs =>
                  let
                    val (h,t) = hdTl ns

                    val (hsubs,subs) =
                        List.partition (equal h o hd o fst) subs

                    val hsubs =
                        (t,n) :: List.map (fn (ns,n) => (tl ns, n)) hsubs
                  in
                    [Html.Inline (toHtmlName h),
                     Html.Block [toUlist hsubs]] ::
                    categorize subs
                  end
          in
            toUlist o map dest
          end

      fun toHtmlTypeOps name ots =
          if null ots then []
          else
            [Html.H2 [Html.Text (name ^ " Type Operators")],
             toHtmlNames (List.map TypeOp.name ots)]

      fun toHtmlConsts name cs =
          if null cs then []
          else
            [Html.H2 [Html.Text (name ^ " Constants")],
             toHtmlNames (List.map Const.name cs)]

      fun toHtmlSymbol name sym =
          let
            val ots = TypeOpSet.toList (Symbol.typeOps sym)
            and cs = ConstSet.toList (Symbol.consts sym)
          in
            toHtmlTypeOps name ots @
            toHtmlConsts name cs
          end

      fun toHtmlSequentSet toHtmlSeq name seqs =
          if SequentSet.null seqs then []
          else
            let
              val seqs = SequentSet.toList seqs
            in
              Html.H2 [Html.Text name] ::
              List.map toHtmlSeq seqs
            end
    in
      fn info =>
         let
           val Info {input,assumed,defined,axioms,thms} = info
         in
           toHtmlSymbol "Defined" defined @
           toHtmlSequentSet toHtmlAxiom "Axioms" axioms @
           toHtmlSequentSet toHtmlTheorem "Theorems" thms @
           toHtmlSymbol "Input" input @
           toHtmlSequentSet toHtmlAssumption "Assumptions" assumed
         end
    end;

fun toHtmlInfoWithGrammar grammar =
    let
      val Grammar
            {assumptionGrammar,
             axiomGrammar,
             theoremGrammar} = grammar

      val toHtmlAssumption = Sequent.toHtmlWithGrammar assumptionGrammar

      val toHtmlAxiom = Sequent.toHtmlWithGrammar axiomGrammar

      val toHtmlTheorem = Sequent.toHtmlWithGrammar theoremGrammar
    in
      toHtmlInfo toHtmlAssumption toHtmlAxiom toHtmlTheorem
    end;

fun toHtmlWithGrammar grammar =
    let
      val toHIWS = toHtmlInfoWithGrammar grammar
    in
      fn show => toHIWS show o toInfo
    end;

val toHtml = toHtmlWithGrammar htmlGrammar;

end
