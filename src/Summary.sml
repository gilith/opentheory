(* ========================================================================= *)
(* THEORY SUMMARIES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Summary :> Summary =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type tracking theorem assumptions.                                      *)
(* ------------------------------------------------------------------------- *)

datatype assumptions = Assumptions of Sequent.sequent -> SequentSet.set;

val noAssumptions = Assumptions (K SequentSet.empty);

fun getAssumptions (Assumptions f) seq = f seq;

fun addAssumptions axs (seq,acc) = SequentSet.union acc (getAssumptions axs seq);

fun getListAssumptions axs seqs =
    List.foldl (addAssumptions axs) SequentSet.empty seqs;

fun getSetAssumptions axs seqs =
    SequentSet.foldl (addAssumptions axs) SequentSet.empty seqs;

fun fromThmsAssumptions ths =
    let
      fun get seq =
          case Thms.peek ths seq of
            SOME th => Thm.axioms th
          | NONE => raise Bug "Summary.fromThmsAssumptions.get"
    in
      Assumptions get
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
       assumptions : assumptions};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk sum' =
    let
      val assumptions = noAssumptions
    in
      Summary
        {summary' = sum',
         assumptions = assumptions}
    end;

fun dest (Summary {summary' = x, ...}) = x;

fun assumptions (Summary {assumptions = x, ...}) = x;

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

      val assumptions = fromThmsAssumptions ths
    in
      Summary
        {summary' = sum',
         assumptions = assumptions}
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
      {input : SymbolTable.table,
       assumed : SequentSet.set,
       defined : SymbolTable.table,
       axioms : SequentSet.set,
       thms : SequentSet.set};

local
  fun allSymbolsIn sym =
      let
        val ots = SymbolTable.typeOps sym
        and cs = SymbolTable.consts sym
      in
        fn seq =>
           TypeOpSet.subset (Sequent.typeOps seq) ots andalso
           ConstSet.subset (Sequent.consts seq) cs
      end;
in
  fun toInfo unsatisfiedAssumptions summary =
      let
(*OpenTheoryTrace5
        val () = trace "entering Summary.toInfo\n"
*)
        val Summary' {requires,provides} = dest summary

        val {undefined = inp, defined = def} =
            let
              val req = Sequents.symbol requires
              and prov = Sequents.symbol provides

              val sym = SymbolTable.union req prov
            in
              SymbolTable.partitionUndef sym
            end

        val (ass,ax) =
            let
              val req = Sequents.sequents requires
            in
              SequentSet.partition (allSymbolsIn inp) req
            end

        val ass =
            case unsatisfiedAssumptions of
              SOME f => SequentSet.filter f ass
            | NONE => ass

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
      fun addTypeOp (ot,acc) = NameSet.add acc (TypeOp.name ot)

      fun addConst (c,acc) = NameSet.add acc (Const.name c)

      val ns = NameSet.empty

      val ns = TypeOpSet.foldl addTypeOp ns (SequentSet.typeOps seqs)

      val ns = ConstSet.foldl addConst ns (SequentSet.consts seqs)

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
              Print.consistentBlock 2
                [Print.ppString err,
                 Print.ppString " in ",
                 Print.ppString class,
                 Print.newline,
                 Sequent.ppWithShow show seq]
        in
          warn (Print.toString ppErr ())
        end
    end;

local
  fun warnSequents class show seqs =
      let
        fun ppSeq seq =
            Print.sequence Print.newline (Sequent.ppWithShow show seq)

        val n = SequentSet.size seqs

        val class = class ^ (if n = 1 then "" else "s")

        fun ppSeqs () =
            Print.consistentBlock 2
              (Print.ppPrettyInt n ::
               Print.space ::
               Print.ppString class ::
               (if n = 1 then Print.skip else Print.ppString "s") ::
               Print.ppString ":" ::
               List.map ppSeq (SequentSet.toList seqs))

        val mesg = Print.toString ppSeqs ()

        val () = warn mesg
      in
        ()
      end;

  fun checkInfo unsat show info =
      let
        val Info {assumed,axioms,thms,...} = info

        val () =
            if SequentSet.null axioms then ()
            else warnSequents "axiom" show axioms

        val seqs = SequentSet.unionList [assumed,axioms,thms]

        val () = checkShow seqs show

        val () = SequentSet.app (checkSequent show "assumption") assumed

        val () = SequentSet.app (checkSequent show "theorem") thms

        val () =
            case unsat of
              NONE => ()
            | SOME p =>
              let
                val asses = SequentSet.filter p assumed
              in
                if SequentSet.null asses then ()
                else warnSequents "unsatisfied assumption" show asses
              end
      in
        ()
      end;
in
  fun check unsat show sum = checkInfo unsat show (toInfo NONE sum);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {assumptionGrammar : Sequent.grammar,
       axiomGrammar : Sequent.grammar,
       theoremGrammar : Sequent.grammar,
       ppTypeOp : Show.show -> TypeOp.typeOp Print.pp,
       ppConst : Show.show -> Const.const Print.pp,
       unsatisfiedAssumptions : (Sequent.sequent -> bool) option,
       showTheoremAssumptions : bool};

val defaultGrammar =
    let
      val assumptionGrammar = Sequent.defaultGrammar
      and axiomGrammar = Sequent.defaultGrammar
      and theoremGrammar = Sequent.defaultGrammar
      and ppTypeOp = TypeOp.ppWithShow
      and ppConst = Const.ppWithShow
      and unsatisfiedAssumptions = NONE
      and showTheoremAssumptions = false
    in
      Grammar
        {assumptionGrammar = assumptionGrammar,
         axiomGrammar = axiomGrammar,
         theoremGrammar = theoremGrammar,
         ppTypeOp = ppTypeOp,
         ppConst = ppConst,
         unsatisfiedAssumptions = unsatisfiedAssumptions,
         showTheoremAssumptions = showTheoremAssumptions}
    end;

local
  fun intersperse x =
      let
        fun f h t =
            case t of
              [] => [h]
            | h' :: t => h :: x :: f h' t
      in
        fn [] => []
         | h :: t => f h t
      end;

  val ppId = Print.ppBracket "(" ")" Print.ppInt;

  val ppIdSet =
      let
        fun spaceId i = Print.sequence Print.break (ppId i)

        fun ppList l =
            case l of
              [] => Print.skip
            | h :: t => Print.program (ppId h :: List.map spaceId t)
      in
        Print.ppBracket "{" "}" (Print.ppMap IntSet.toList ppList)
      end;

  fun ppList ppX prefix name xs =
      let
        val n = List.length xs
      in
        if n = 0 then []
        else
          [Print.inconsistentBlock 2
             (Print.ppPrettyInt n ::
              Print.ppString " " ::
              Print.ppString prefix ::
              Print.ppString name ::
              (if n = 1 then Print.skip else Print.ppString "s") ::
              Print.ppString ":" ::
              List.map (Print.sequence Print.break o ppX) xs)]
      end;

  fun ppSequentList ppSeq (name,seqs) =
      let
        val n = List.length seqs
      in
        if n = 0 then []
        else
          [Print.consistentBlock 2
             (Print.ppPrettyInt n ::
              Print.ppString " " ::
              Print.ppString name ::
              (if n = 1 then Print.skip else Print.ppString "s") ::
              Print.ppString ":" ::
              List.map (Print.sequence Print.newline o ppSeq) seqs)]
      end;
in
  fun ppInfoWithShow ppTypeOpWS ppConstWS ppAssumptionWS ppAxiomWS ppTheoremWS
                     isUnsat show =
      let
        val ppTypeOp = ppTypeOpWS show
        and ppConst = ppConstWS show
        and ppAssumption = ppAssumptionWS show
        and ppAxiom = ppAxiomWS show
        and ppTheorem = ppTheoremWS show

        fun ppSymbol (prefix,sym) =
            let
              val ots = TypeOpSet.toList (SymbolTable.typeOps sym)
              and cs = ConstSet.toList (SymbolTable.consts sym)
            in
              ppList ppTypeOp prefix " type operator" ots @
              ppList ppConst prefix " constant" cs
            end
      in
        fn asses => fn info =>
           let
             val Info {input,assumed,defined,axioms,thms} = info

             val assumed = SequentSet.toList assumed
             and axioms = SequentSet.toList axioms
             and thms = SequentSet.toList thms

             val (_,assMap) =
                 let
                   val assSet = getListAssumptions asses thms

                   fun add (seq,(n,m)) =
                       if not (SequentSet.member seq assSet) then (n,m)
                       else (n + 1, SequentMap.insert m (seq,n))
                 in
                   List.foldl add (1, SequentMap.new ()) (assumed @ axioms)
                 end

             fun ppAss ppSeq seq =
                 case SequentMap.peek assMap seq of
                   NONE => ppSeq seq
                 | SOME n =>
                   Print.consistentBlock 2
                     [ppId n,
                      Print.break,
                      ppSeq seq]

             fun ppTh ppSeq seq =
                 let
                   fun add (ass,ids) =
                       case SequentMap.peek assMap ass of
                         SOME i => IntSet.add ids i
                       | NONE => ids

                   val assSet = getAssumptions asses seq

                   val ids = SequentSet.foldl add IntSet.empty assSet
                 in
                   if IntSet.null ids then ppSeq seq
                   else
                     Print.consistentBlock 2
                       [ppIdSet ids,
                        Print.newline,
                        ppSeq seq]
                 end

             val assumptionClass =
                 if isUnsat then "unsatisfied assumption"
                 else "assumption"

             val blocks =
                 ppSymbol ("input",input) @
                 ppSequentList (ppAss ppAssumption) (assumptionClass,assumed) @
                 ppSymbol ("defined",defined) @
                 ppSequentList (ppAss ppAxiom) ("axiom",axioms) @
                 ppSequentList (ppTh ppTheorem) ("theorem",thms)
           in
             if List.null blocks then Print.skip
             else
               Print.consistentBlock 0
                 (intersperse Print.newline blocks)
           end
      end;
end;

fun ppWithGrammar grammar =
    let
      val Grammar
            {assumptionGrammar,
             axiomGrammar,
             theoremGrammar,
             ppTypeOp,
             ppConst,
             unsatisfiedAssumptions,
             showTheoremAssumptions} = grammar

      val ppAssumptionWS = Sequent.ppWithGrammar assumptionGrammar

      val ppAxiomWS = Sequent.ppWithGrammar axiomGrammar

      val ppTheoremWS = Sequent.ppWithGrammar theoremGrammar

      val ppIWS =
          ppInfoWithShow ppTypeOp ppConst ppAssumptionWS ppAxiomWS ppTheoremWS
          (Option.isSome unsatisfiedAssumptions)
    in
      fn show =>
         let
           val ppI = ppIWS show
         in
           fn sum =>
              let
                val asses =
                    if showTheoremAssumptions then assumptions sum
                    else noAssumptions

                val info = toInfo unsatisfiedAssumptions sum
              in
                ppI asses info
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
           "-" => [conn]
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

local
  fun ppTypeOp show =
      let
        val toHtml = TypeOp.toHtml show
      in
        fn ot =>
           let
             val vs = TypeOp.varsDef ot
           in
             Html.ppFixed (toHtml (ot,vs))
           end
      end;

  fun ppConst show =
      let
        val toHtml = Const.toHtml show
      in
        fn c =>
           let
             val ty = Const.typeOf c
           in
             Html.ppFixed (toHtml (c,ty))
           end
      end;
in
  val htmlGrammar =
      let
        val assumptionGrammar = htmlGrammarSequent "assumption" "Assumption"
        and axiomGrammar = htmlGrammarSequent "axiom" "Axiom"
        and theoremGrammar = htmlGrammarSequent "theorem" "Theorem"
        and unsatisfiedAssumptions = NONE
        and showTheoremAssumptions = false
      in
        Grammar
          {unsatisfiedAssumptions = unsatisfiedAssumptions,
           assumptionGrammar = assumptionGrammar,
           axiomGrammar = axiomGrammar,
           theoremGrammar = theoremGrammar,
           ppTypeOp = ppTypeOp,
           ppConst = ppConst,
           showTheoremAssumptions = showTheoremAssumptions}
      end;
end;

local
  fun fstNull (l,_) = List.null l;

  fun fstHdEqual x (l,_) =
      case l of
        [] => false
      | h :: _ => h = x;

  fun fstTl (l,x) = (tl l, x);

  fun dest (name,x) =
      let
        val (ns,n) = Name.dest name
      in
        (Namespace.toList ns @ [n], x)
      end;

  fun toHtml ppX x = Html.Raw (Print.toString ppX x)

  fun toHtmlNamespace ns name =
      let
        val class = "namespace"

        val title = Namespace.toString ns

        val attrs = Html.fromListAttrs [("class",class),("title",title)]

        val inlines = Name.toHtml name
      in
        Html.Span (attrs,inlines)
      end;

  fun toItem flows = Html.ListItem (Html.emptyAttrs,flows)
in
  fun toHtmlNames ppX =
      let
        fun toUlist stack names =
            let
(*OpenTheoryDebug
              val () = if not (List.null names) then ()
                       else raise Bug "Summary.toHtmlNames.toUlist"
*)
              val items = toItemList stack names
            in
              Html.Ulist (Html.emptyAttrs,items)
            end

        and toItemList stack subs =
            case subs of
              [] => []
            | (ns,x) :: subs =>
              let
                val (h,t) =
                    case ns of
                      [] => raise Bug "Summary.toHtmlNames.toItemList"
                    | h :: t => (h,t)

                val name = Name.mkGlobal h

                val hitem =
                    if List.null t then toHtml ppX x
                    else
                      let
                        val x = Namespace.fromList (List.revAppend (stack,[h]))
                      in
                        toHtmlNamespace x name
                      end

                val (hsubs,subs) = List.partition (fstHdEqual h) subs

                val hsubs = (t,x) :: List.map fstTl hsubs

                val hsubs = List.filter (not o fstNull) hsubs

                val sitem =
                    if List.null hsubs then []
                    else [Html.Block [toUlist (h :: stack) hsubs]]

                val item = toItem (Html.Inline [hitem] :: sitem)
              in
                item :: toItemList stack subs
              end
      in
        fn name => fn nxs =>
            let
              val n = length nxs

              val name = if n = 1 then name else name ^ "s"

              val title =
                  Print.toString Print.ppPrettyInt n ^ " " ^
                  String.map Char.toLower name

              val attrs = Html.singletonAttrs ("title",title)

              val header = Html.Span (attrs, [Html.Text name])
            in
              [Html.H2 [header],
               toUlist [] (List.map dest nxs)]
            end
      end;
end;

fun toHtmlInfo ppTypeOpWS ppConstWS
               toHtmlAssumptionWS toHtmlAxiomWS toHtmlTheoremWS
               show =
    let
      val ppTypeOp = ppTypeOpWS show
      and ppConst = ppConstWS show
      and toHtmlAssumption = toHtmlAssumptionWS show
      and toHtmlAxiom = toHtmlAxiomWS show
      and toHtmlTheorem = toHtmlTheoremWS show

      fun toHtmlTypeOps name ots =
          if List.null ots then []
          else
            let
              fun dest ot =
                  let
                    val n = TypeOp.showNameHtml Show.natural (ot,NONE)
                  in
                    (n,ot)
                  end
            in
              toHtmlNames ppTypeOp (name ^ " Type Operator") (List.map dest ots)
            end

      fun toHtmlConsts name cs =
          if List.null cs then []
          else
            let
              fun dest c =
                  let
                    val n = Const.showNameHtml Show.natural (c,NONE)
                  in
                    (n,c)
                  end
            in
              toHtmlNames ppConst (name ^ " Constant") (List.map dest cs)
            end

      fun toHtmlSymbol name sym =
          let
            val ots = TypeOpSet.toList (SymbolTable.typeOps sym)
            and cs = ConstSet.toList (SymbolTable.consts sym)
          in
            toHtmlTypeOps name ots @
            toHtmlConsts name cs
          end

      fun toHtmlSequentSet toHtmlSeq name verb classes seqs =
          if SequentSet.null seqs then []
          else
            let
              val n = SequentSet.size seqs

              val name = if n = 1 then name else name ^ "s"

              val title =
                  Print.toString Print.ppPrettyInt n ^ " " ^
                  String.map Char.toLower name ^ " " ^ verb

              val attrs =
                  Html.fromListAttrs
                    (("title",title) ::
                     List.map (fn c => ("class",c)) classes)

              val header = Html.Span (attrs, [Html.Text name])

              val seqs = SequentSet.toList seqs
            in
              Html.H2 [header] ::
              List.map toHtmlSeq seqs
            end
    in
      fn info =>
         let
           val Info {input,assumed,defined,axioms,thms} = info
         in
           toHtmlSymbol "Defined" defined @
           toHtmlSequentSet toHtmlAxiom "Axiom" "asserted" ["warning"] axioms @
           toHtmlSequentSet toHtmlTheorem "Theorem" "proved" [] thms @
           toHtmlSymbol "Input" input @
           toHtmlSequentSet toHtmlAssumption "Assumption" "made" [] assumed
         end
    end;

fun toHtmlInfoWithGrammar grammar =
    let
      val Grammar
            {assumptionGrammar,
             axiomGrammar,
             theoremGrammar,
             ppTypeOp,
             ppConst,
             unsatisfiedAssumptions = _,
             showTheoremAssumptions = _} = grammar

      val toHtmlAssumption = Sequent.toHtmlWithGrammar assumptionGrammar

      val toHtmlAxiom = Sequent.toHtmlWithGrammar axiomGrammar

      val toHtmlTheorem = Sequent.toHtmlWithGrammar theoremGrammar
    in
      toHtmlInfo ppTypeOp ppConst toHtmlAssumption toHtmlAxiom toHtmlTheorem
    end;

fun toHtmlWithGrammar grammar =
    let
      val Grammar {unsatisfiedAssumptions, ...} = grammar

      val toHIWS = toHtmlInfoWithGrammar grammar
    in
      fn show =>
         let
           val toHI = toHIWS show
         in
           fn sum => toHI (toInfo unsatisfiedAssumptions sum)
         end
    end;

val toHtml = toHtmlWithGrammar htmlGrammar;

end
