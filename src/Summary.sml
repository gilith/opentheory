(* ========================================================================= *)
(* THEORY SUMMARIES                                                          *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
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

fun addAssumptions axs (seq,acc) =
    SequentSet.union acc (getAssumptions axs seq);

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
(* Symbols.                                                                  *)
(* ------------------------------------------------------------------------- *)

fun symbol sum =
    let
      val req = requires sum
      and prov = provides sum

      val s1 = SymbolTable.symbols (Sequents.symbol req)
      and s2 = SymbolTable.symbols (Sequents.symbol prov)
    in
      SymbolSet.union s1 s2
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
(* A type of theory contexts.                                                *)
(* ------------------------------------------------------------------------- *)

datatype context =
    NoContext
  | Context of
      {groundedExternal : Symbol.symbol -> bool,
       satisfiedAssumption : Sequent.sequent -> bool};

val defaultContext = NoContext;

(* ------------------------------------------------------------------------- *)
(* A type of summary external symbols.                                       *)
(* ------------------------------------------------------------------------- *)

datatype externals =
    AllExternals of SymbolSet.set
  | ClassifiedExternals of
      {grounded : SymbolSet.set,
       ungrounded : SymbolSet.set};

fun mkExternals ctxt s =
    case ctxt of
      NoContext => AllExternals s
    | Context {groundedExternal = g, ...} =>
      let
        val (gs,us) = SymbolSet.partition g s
      in
        ClassifiedExternals {grounded = gs, ungrounded = us}
      end;

(* ------------------------------------------------------------------------- *)
(* A type of summary assumptions.                                            *)
(* ------------------------------------------------------------------------- *)

datatype assumptions =
    AllAssumptions of SequentSet.set
  | ClassifiedAssumptions of
      {satisfied : int,
       unsatisfied : SequentSet.set};

fun mkAssumptions ctxt ass =
    case ctxt of
      NoContext => AllAssumptions ass
    | Context {satisfiedAssumption = p, ...} =>
      let
        val unsat = SequentSet.filter (not o p) ass

        val sat = SequentSet.size ass - SequentSet.size unsat
      in
        ClassifiedAssumptions {satisfied = sat, unsatisfied = unsat}
      end;

(* ------------------------------------------------------------------------- *)
(* A type of theory summary information (for pretty printing).               *)
(* ------------------------------------------------------------------------- *)

datatype info =
    Info of
      {external : externals,
       assumed : assumptions,
       defined : SymbolSet.set,
       axioms : SequentSet.set,
       thms : SequentSet.set};

local
  fun allSymbolsExt ext =
      let
        val {typeOps = ts, consts = cs} = SymbolSet.categorize ext
      in
        fn seq =>
           TypeOpSet.subset (Sequent.typeOps seq) ts andalso
           ConstSet.subset (Sequent.consts seq) cs
      end;
in
  fun toInfo ctxt summary =
      let
(*OpenTheoryTrace5
        val () = trace "entering Summary.toInfo\n"
*)
        val Summary' {requires,provides} = dest summary

        val (ext,def) =
            let
              val req = SymbolTable.symbols (Sequents.symbol requires)
              and prov = SymbolTable.symbols (Sequents.symbol provides)
            in
              SymbolSet.partition Symbol.isUndef (SymbolSet.union req prov)
            end

        val (ass,ax) =
            let
              val req = Sequents.sequents requires
            in
              SequentSet.partition (allSymbolsExt ext) req
            end

        val ext = mkExternals ctxt ext

        val ass = mkAssumptions ctxt ass

        val ths = Sequents.sequents provides

(*OpenTheoryTrace5
        val () = trace "exiting Summary.toInfo\n"
*)
      in
        Info
          {external = ext,
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
      val errs = []

      val errs =
          case TermAlphaSet.size (Sequent.hyp seq) of
            0 => errs
          | 1 => "hypothesis" :: errs
          | _ => "hypotheses" :: errs

      val errs =
          case VarSet.size (Term.freeVars (Sequent.concl seq)) of
            0 => errs
          | 1 => "free variable" :: errs
          | _ => "free variables" :: errs
    in
      case errs of
        [] => ()
      | err :: errs =>
        let
          fun addErr b s (err,pps) =
              let
                val pps = Print.ppString s :: Print.break :: pps

                val pps = if b then Print.break :: pps else pps
              in
                Print.ppString err :: pps
              end

          fun ppErrs () =
              let
                val pps = addErr true "in" (err,[Print.ppString class])

                val (pps,errs) =
                    case errs of
                      [] => (pps,errs)
                    | err :: errs => (addErr true "and" (err,pps), errs)

                val pps = List.foldl (addErr false ",") pps errs
              in
                Print.inconsistentBlock 0 pps
              end

          fun ppErrsSeq () =
              Print.consistentBlock 2
                [ppErrs (),
                 Print.newline,
                 Sequent.ppWithShow show seq]
        in
          warn (Print.toString ppErrsSeq ())
        end
    end;

local
  fun warnNames class show ns =
      let
        fun ppName n =
            let
              val n = Show.showName show n
            in
              Print.sequence Print.break (Name.pp n)
            end

        val n = List.length ns

        fun ppNames () =
            Print.inconsistentBlock 2
              (Print.ppPrettyInt n ::
               Print.space ::
               Print.ppString class ::
               (if n = 1 then Print.skip else Print.ppString "s") ::
               Print.ppString ":" ::
               List.map ppName ns)

        val mesg = Print.toString ppNames ()

        val () = warn mesg
      in
        ()
      end;

  fun warnTypeOps class show ts =
      let
        val class = class ^ " type operator"

        val ns = List.map TypeOp.name (TypeOpSet.toList ts)
      in
        warnNames class show ns
      end;

  fun warnConsts class show cs =
      let
        val class = class ^ " constant"

        val ns = List.map Const.name (ConstSet.toList cs)
      in
        warnNames class show ns
      end;

  fun warnSequents class show seqs =
      let
        fun ppSeq seq =
            Print.sequence Print.newline (Sequent.ppWithShow show seq)

        val n = SequentSet.size seqs

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

  fun checkSequents chk show class seqs =
      if not chk then ()
      else SequentSet.app (checkSequent show class) seqs;

  fun checkInfo chks ctxt show info =
      let
        val {checkTheorems = chkSeqs} = chks
        and Info {external,assumed,axioms,thms,...} = info

        val external =
            case external of
              AllExternals sym => sym
            | ClassifiedExternals _ =>
              raise Bug "Summary.check.checkInfo.ClassifiedExternals"

        val assumed =
            case assumed of
              AllAssumptions seqs => seqs
            | ClassifiedAssumptions _ =>
              raise Bug "Summary.check.checkInfo.ClassifiedAssumptions"

        val () =
            if SequentSet.null axioms then ()
            else warnSequents "axiom" show axioms

        val seqs = SequentSet.unionList [assumed,axioms,thms]

        val () = checkShow seqs show

        val () =
            case mkExternals ctxt external of
              AllExternals _ => ()
            | ClassifiedExternals {ungrounded = ext, ...} =>
              let
                val {typeOps = ts, consts = cs} = SymbolSet.categorize ext

                val () =
                    if TypeOpSet.null ts then ()
                    else warnTypeOps "ungrounded external" show ts

                val () =
                    if ConstSet.null cs then ()
                    else warnConsts "ungrounded external" show cs
              in
                ()
              end

        val () =
            case mkAssumptions ctxt assumed of
              AllAssumptions ass =>
              checkSequents chkSeqs show "assumption" assumed
            | ClassifiedAssumptions {unsatisfied = ass, ...} =>
              if SequentSet.null ass then ()
              else warnSequents "unsatisfied assumption" show ass

        val () = checkSequents chkSeqs show "theorem" thms
      in
        ()
      end;
in
  fun check chks ctxt show sum =
      checkInfo chks ctxt show (toInfo NoContext sum);
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
       showTheoremAssumptions : bool};

val defaultGrammar =
    let
      val assumptionGrammar = Sequent.defaultGrammar
      and axiomGrammar = Sequent.defaultGrammar
      and theoremGrammar = Sequent.defaultGrammar
      and ppTypeOp = TypeOp.ppWithShow
      and ppConst = Const.ppWithShow
      and showTheoremAssumptions = false
    in
      Grammar
        {assumptionGrammar = assumptionGrammar,
         axiomGrammar = axiomGrammar,
         theoremGrammar = theoremGrammar,
         ppTypeOp = ppTypeOp,
         ppConst = ppConst,
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

  fun ppSatisfied n =
      if n = 0 then []
      else
        [Print.consistentBlock 2
           [Print.ppPrettyInt n,
            Print.ppString " satisfied assumption",
            (if n = 1 then Print.skip else Print.ppString "s"),
            Print.ppString ": hidden"]];

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
                     show =
      let
        val ppTypeOp = ppTypeOpWS show
        and ppConst = ppConstWS show
        and ppAssumption = ppAssumptionWS show
        and ppAxiom = ppAxiomWS show
        and ppTheorem = ppTheoremWS show

        fun ppSymbol (prefix,sym) =
            let
              val {typeOps = ts, consts = cs} = SymbolSet.categorize sym

              val ts = TypeOpSet.toList ts
              and cs = ConstSet.toList cs
            in
              ppList ppTypeOp prefix " type operator" ts @
              ppList ppConst prefix " constant" cs
            end
      in
        fn asses => fn info =>
           let
             val Info {external,assumed,defined,axioms,thms} = info

             val (external,uexternal) =
                 case external of
                   AllExternals ext => (ext,SymbolSet.empty)
                 | ClassifiedExternals {grounded,ungrounded} =>
                   (grounded,ungrounded)

             val (sat,assumed) =
                 case assumed of
                   AllAssumptions seqs => (NONE,seqs)
                 | ClassifiedAssumptions {satisfied,unsatisfied} =>
                   (SOME satisfied, unsatisfied)

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

             val assumptionBlocks =
                 let
                   val ppAssumed = ppSequentList (ppAss ppAssumption)
                 in
                   case sat of
                     NONE => ppAssumed ("assumption",assumed)
                   | SOME n =>
                     ppSatisfied n @
                     ppAssumed ("unsatisfied assumption",assumed)
                 end

             val blocks =
                 ppSymbol ("external",external) @
                 ppSymbol ("ungrounded external",uexternal) @
                 assumptionBlocks @
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
             showTheoremAssumptions} = grammar

      val ppAssumptionWS = Sequent.ppWithGrammar assumptionGrammar

      val ppAxiomWS = Sequent.ppWithGrammar axiomGrammar

      val ppTheoremWS = Sequent.ppWithGrammar theoremGrammar

      val ppInfoWS =
          ppInfoWithShow ppTypeOp ppConst ppAssumptionWS ppAxiomWS ppTheoremWS
    in
      fn context => fn show =>
         let
           val ppInfo = ppInfoWS show
         in
           fn sum =>
              let
                val asses =
                    if showTheoremAssumptions then assumptions sum
                    else noAssumptions

                val info = toInfo context sum
              in
                ppInfo asses info
              end
         end
    end;

val ppWithContext = ppWithGrammar defaultGrammar;

val ppWithShow = ppWithContext NoContext;

val pp = ppWithShow Show.default;

fun toTextFileWithGrammar grammar =
    let
      val ppWCS = ppWithGrammar grammar
    in
      fn {context,show,summary,filename} =>
         let
           val lines = Print.toStream (ppWCS context show) summary

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
        and showTheoremAssumptions = false
      in
        Grammar
          {assumptionGrammar = assumptionGrammar,
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
        fn name => fn classes => fn nxs =>
            let
              val n = length nxs

              val name = if n = 1 then name else name ^ "s"

              val title =
                  Print.toString Print.ppPrettyInt n ^ " " ^
                  String.map Char.toLower name

              val attrs =
                  Html.fromListAttrs
                    (("title",title) ::
                     List.map (fn c => ("class",c)) classes)

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

      fun toHtmlTypeOps name classes ots =
          if List.null ots then []
          else
            let
              fun dest ot =
                  let
                    val n = TypeOp.showNameHtml Show.natural (ot,NONE)
                  in
                    (n,ot)
                  end

              val name = name ^ " Type Operator"
            in
              toHtmlNames ppTypeOp name classes (List.map dest ots)
            end

      fun toHtmlConsts name classes cs =
          if List.null cs then []
          else
            let
              fun dest c =
                  let
                    val n = Const.showNameHtml Show.natural (c,NONE)
                  in
                    (n,c)
                  end

              val name = name ^ " Constant"
            in
              toHtmlNames ppConst name classes (List.map dest cs)
            end

      fun toHtmlSymbol name classes sym =
          let
            val {typeOps = ts, consts = cs} = SymbolSet.categorize sym

            val ts = TypeOpSet.toList ts
            and cs = ConstSet.toList cs
          in
            toHtmlTypeOps name classes ts @
            toHtmlConsts name classes cs
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
           val Info {external,assumed,defined,axioms,thms} = info

           val (uexternal,external) =
               case external of
                 AllExternals sym => (SymbolSet.empty,sym)
               | ClassifiedExternals {grounded = sym, ungrounded = usym} =>
                 (usym,sym)

           val assumptionBlocks =
               case assumed of
                 AllAssumptions seqs =>
                 toHtmlSequentSet toHtmlAssumption "Assumption" "made" [] seqs
               | ClassifiedAssumptions {satisfied = _, unsatisfied} =>
                 toHtmlSequentSet toHtmlAssumption
                   "Unsatisfied assumption" "made" [] unsatisfied
         in
           toHtmlSymbol "Defined" [] defined @
           toHtmlSymbol "Ungrounded External" ["warning"] uexternal @
           toHtmlSequentSet toHtmlAxiom "Axiom" "asserted" ["warning"] axioms @
           toHtmlSequentSet toHtmlTheorem "Theorem" "proved" [] thms @
           toHtmlSymbol "External" [] external @
           assumptionBlocks
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
             showTheoremAssumptions = _} = grammar

      val toHtmlAssumption = Sequent.toHtmlWithGrammar assumptionGrammar

      val toHtmlAxiom = Sequent.toHtmlWithGrammar axiomGrammar

      val toHtmlTheorem = Sequent.toHtmlWithGrammar theoremGrammar
    in
      toHtmlInfo ppTypeOp ppConst toHtmlAssumption toHtmlAxiom toHtmlTheorem
    end;

fun toHtmlWithGrammar grammar =
    let
      val toHIWS = toHtmlInfoWithGrammar grammar
    in
      fn context => fn show =>
         let
           val toHI = toHIWS show
         in
           fn sum => toHI (toInfo context sum)
         end
    end;

val toHtmlWithContext = toHtmlWithGrammar htmlGrammar;

val toHtml = toHtmlWithContext NoContext;

end
