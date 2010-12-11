(* ========================================================================= *)
(* THEORY SUMMARIES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Summary :> Summary =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type tracking assumptions/axioms of theorems.                           *)
(* ------------------------------------------------------------------------- *)

datatype axioms = Axioms of Sequent.sequent -> SequentSet.set;

val noAxioms = Axioms (K SequentSet.empty);

fun getAxioms (Axioms f) seq = f seq;

fun addAxioms axs (seq,acc) = SequentSet.union acc (getAxioms axs seq);

fun getListAxioms axs seqs =
    List.foldl (addAxioms axs) SequentSet.empty seqs;

fun getSetAxioms axs seqs =
    SequentSet.foldl (addAxioms axs) SequentSet.empty seqs;

fun fromThmsAxioms ths =
    let
      fun get seq =
          case Thms.peek ths seq of
            SOME th => Thm.axioms th
          | NONE => raise Bug "Summary.fromThmsAxioms.get"
    in
      Axioms get
    end;

(* ------------------------------------------------------------------------- *)
(* A type of theory summaries.                                               *)
(* ------------------------------------------------------------------------- *)

datatype summary' =
    Summary' of
      {requires : Sequents.sequents,
       provides : Sequents.sequents};

datatype summary =
    Summary of
      {summary' : summary',
       axioms : axioms};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk sum' =
    let
      val axioms = noAxioms
    in
      Summary
        {summary' = sum',
         axioms = axioms}
    end;

fun dest (Summary {summary' = x, ...}) = x;

fun axioms (Summary {axioms = x, ...}) = x;

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

      val axioms = fromThmsAxioms ths
    in
      Summary
        {summary' = sum',
         axioms = axioms}
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
(* Check summary.                                                            *)
(* ------------------------------------------------------------------------- *)

fun checkShow seqs =
    let
      fun add (c,acc) = NameSet.add acc (Const.name c)

      val cs = SequentSet.consts seqs

      val ns = ConstSet.foldl add NameSet.empty cs

      fun changed sh n =
          let
            val n' = Show.showName sh n
          in
            not (Name.equal n' n)
          end

      fun chk mp =
          let
            val sh = Show.fromList [mp]
          in
            if NameSet.exists (changed sh) ns then ()
            else warn ("redundant show: " ^ Show.toStringMapping mp)
          end
    in
      fn show => List.app chk (Show.toList show)
    end;

fun checkSequent show class seq =
    let
      val err =
          case TermAlphaSet.size (Sequent.hyp seq) of
            0 => NONE
          | 1 => SOME "hypothesis"
          | _ => SOME "hypotheses"

      val err =
          if Option.isSome err then err
          else
            case VarSet.size (Term.freeVars (Sequent.concl seq)) of
              0 => NONE
            | 1 => SOME "free variable"
            | _ => SOME "free variables"
    in
      case err of
        NONE => ()
      | SOME err =>
        let
          fun ppErr () =
              Print.blockProgram Print.Consistent 2
                [Print.ppString err,
                 Print.ppString " in ",
                 Print.ppString class,
                 Print.addNewline,
                 Sequent.ppWithShow show seq]
        in
          warn (Print.toString ppErr ())
        end
    end;

fun checkInfo show info =
    let
      val Info {assumed,axioms,thms,...} = info

      val seqs = SequentSet.unionList [assumed,axioms,thms]

      val () = checkShow seqs show

      val () = SequentSet.app (checkSequent show "assumption") assumed

      val () = SequentSet.app (checkSequent show "axiom") axioms

      val () = SequentSet.app (checkSequent show "theorem") thms
    in
      ()
    end;

fun check show sum = checkInfo show (toInfo sum);

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {assumptionGrammar : Sequent.grammar,
       axiomGrammar : Sequent.grammar,
       theoremGrammar : Sequent.grammar,
       showAxioms : bool};

val defaultGrammar =
    let
      val assumptionGrammar = Sequent.defaultGrammar
      and axiomGrammar = Sequent.defaultGrammar
      and theoremGrammar = Sequent.defaultGrammar
      and showAxioms = false
    in
      Grammar
        {assumptionGrammar = assumptionGrammar,
         axiomGrammar = axiomGrammar,
         theoremGrammar = theoremGrammar,
         showAxioms = showAxioms}
    end;

local
  val ppId = Print.ppBracket "(" ")" Print.ppInt;

  val ppIdSet =
      let
        fun spaceId i = Print.sequence (Print.addBreak 1) (ppId i)

        fun ppList l =
            case l of
              [] => Print.skip
            | h :: t => Print.program (ppId h :: List.map spaceId t)
      in
        Print.ppBracket "{" "}" (Print.ppMap IntSet.toList ppList)
      end;

  fun ppList ppX prefix name xs =
      if List.null xs then Print.skip
      else
        Print.sequence
          (Print.blockProgram Print.Inconsistent 2
             (Print.ppString prefix ::
              Print.ppString name ::
              Print.ppString ":" ::
              List.map (Print.sequence (Print.addBreak 1) o ppX) xs))
          Print.addNewline;

  fun ppSequentList ppSeq (name,seqs) =
      Print.blockProgram Print.Consistent 2
        (Print.ppString name ::
         Print.ppString ":" ::
         List.map (Print.sequence Print.addNewline o ppSeq) seqs);
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
        fn axs => fn info =>
           let
             val Info {input,assumed,defined,axioms,thms} = info

             val assumed = SequentSet.toList assumed
             and axioms = SequentSet.toList axioms
             and thms = SequentSet.toList thms

             val (_,axMap) =
                 let
                   val axSet = getListAxioms axs thms

                   fun add (seq,(n,m)) =
                       if not (SequentSet.member seq axSet) then (n,m)
                       else (n + 1, SequentMap.insert m (seq,n))
                 in
                   List.foldl add (1, SequentMap.new ()) (assumed @ axioms)
                 end

             fun ppAx ppSeq seq =
                 case SequentMap.peek axMap seq of
                   NONE => ppSeq seq
                 | SOME n =>
                   Print.blockProgram Print.Consistent 2
                     [ppId n,
                      Print.addBreak 1,
                      ppSeq seq]

             fun ppTh ppSeq seq =
                 let
                   fun add (ax,ids) =
                       case SequentMap.peek axMap ax of
                         SOME i => IntSet.add ids i
                       | NONE => raise Bug "Summary.ppInfoWithShow.ppTh.add"

                   val axSet = getAxioms axs seq

                   val ids = SequentSet.foldl add IntSet.empty axSet
                 in
                   if IntSet.null ids then ppSeq seq
                   else
                     Print.blockProgram Print.Consistent 2
                       [ppIdSet ids,
                        Print.addNewline,
                        ppSeq seq]
                 end
           in
             Print.blockProgram Print.Consistent 0
               [ppSymbol ("input",input),
                ppSequentList (ppAx ppAssumption) ("assumptions",assumed),
                Print.addNewline,
                ppSymbol ("defined",defined),
                ppSequentList (ppAx ppAxiom) ("axioms",axioms),
                Print.addNewline,
                ppSequentList (ppTh ppTheorem) ("theorems",thms)]
           end
      end;
end;

fun ppWithGrammar grammar =
    let
      val Grammar
            {assumptionGrammar,
             axiomGrammar,
             theoremGrammar,
             showAxioms} = grammar

      val ppAssumptionWS = Sequent.ppWithGrammar assumptionGrammar

      val ppAxiomWS = Sequent.ppWithGrammar axiomGrammar

      val ppTheoremWS = Sequent.ppWithGrammar theoremGrammar

      val ppIWS = ppInfoWithShow ppAssumptionWS ppAxiomWS ppTheoremWS
    in
      fn show =>
         let
           val ppI = ppIWS show
         in
           fn sum =>
              let
                val axs = if showAxioms then axioms sum else noAxioms

                val info = toInfo sum
              in
                ppI axs info
              end
         end
    end;

val ppWithShow = ppWithGrammar defaultGrammar;

val pp = ppWithShow Show.default;

fun toTextFileWithGrammar grammar =
    let
      val ppWS = ppWithGrammar grammar
    in
      fn {show,summary,filename} =>
         let
           val lines = Print.toStream (ppWS show) summary

           val () = Stream.toTextFile {filename = filename} lines
         in
           ()
         end
    end;

val toTextFile = toTextFileWithGrammar defaultGrammar;

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
      and showAxioms = false
    in
      Grammar
        {assumptionGrammar = assumptionGrammar,
         axiomGrammar = axiomGrammar,
         theoremGrammar = theoremGrammar,
         showAxioms = showAxioms}
    end;

local
  fun dest name =
      let
        val (ns,n) = Name.dest name
      in
        (Namespace.toList ns, n)
      end;

  fun toHtmlName name = Name.toHtml (Name.mkGlobal name)

  fun toItem flows = Html.ListItem (Html.emptyAttrs,flows)

  fun toItemName name = toItem [Html.Inline (toHtmlName name)]

  fun toUlist names =
      let
        val (tops,subs) = List.partition (List.null o fst) names

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
  fun toHtmlNames ns = toUlist (List.map dest ns);
end;

fun toHtmlInfo toHtmlAssumptionWS toHtmlAxiomWS toHtmlTheoremWS show =
    let
      val toHtmlAssumption = toHtmlAssumptionWS show

      val toHtmlAxiom = toHtmlAxiomWS show

      val toHtmlTheorem = toHtmlTheoremWS show

      fun toHtmlTypeOps name ots =
          if List.null ots then []
          else
            [Html.H2 [Html.Text (name ^ " Type Operators")],
             toHtmlNames (List.map TypeOp.name ots)]

      fun toHtmlConsts name cs =
          if List.null cs then []
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
             theoremGrammar,
             showAxioms = _} = grammar

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
