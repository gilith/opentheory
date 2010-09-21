(* ========================================================================= *)
(* THEORY SUMMARIES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Summary :> Summary =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory summary.                                                 *)
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
  fun info summary =
      let
(*OpenTheoryTrace5
        val () = trace "entering Summary.info\n"
*)
        val Summary' {requires,provides} = dest summary

        val (inp,def) =
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
        val () = trace "exiting Summary.info\n"
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
in
  fun ppInfo show =
      let
        val ppTypeOp = TypeOp.ppWithShow show

        val ppConst = Const.ppWithShow show

        fun ppSymbol (prefix,sym) =
            let
              val ots = TypeOpSet.toList (Symbol.typeOps sym)
              and cs = ConstSet.toList (Symbol.consts sym)
            in
              Print.sequence
                (ppList ppTypeOp prefix "-types" ots)
                (ppList ppConst prefix "-consts" cs)
            end

        val ppSequent = Print.ppMap Thm.axiom (Thm.ppWithShow show)

        fun ppSequentSet (name,seqs) =
            let
              val seqs = SequentSet.toList seqs
            in
              Print.blockProgram Print.Consistent 2
                (Print.ppString name ::
                 Print.ppString ":" ::
                 map (Print.sequence Print.addNewline o ppSequent) seqs)
            end
      in
        fn sum =>
           let
             val Info {input,assumed,defined,axioms,thms} = sum
           in
             Print.blockProgram Print.Consistent 0
               [ppSymbol ("input",input),
                ppSequentSet ("assumptions",assumed),
                Print.addNewline,
                ppSymbol ("defined",defined),
                ppSequentSet ("axioms",axioms),
                Print.addNewline,
                ppSequentSet ("theorems",thms)]
           end
      end;
end;

fun ppWithShow show = Print.ppMap info (ppInfo show);

fun toHtmlInfo show =
    let
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

      val toHtmlSequent = Sequent.toHtml show

      fun toHtmlSequentSet name seqs =
          if SequentSet.null seqs then []
          else
            let
              val seqs = SequentSet.toList seqs
            in
              Html.H2 [Html.Text name] ::
              List.map toHtmlSequent seqs
            end
    in
      fn sum =>
         let
           val Info {input,assumed,defined,axioms,thms} = sum
         in
           toHtmlSymbol "Defined" defined @
           toHtmlSequentSet "Axioms" axioms @
           toHtmlSequentSet "Theorems" thms @
           toHtmlSymbol "Input" input @
           toHtmlSequentSet "Assumptions" assumed
         end
    end;

fun toHtml show = toHtmlInfo show o info;

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

end
