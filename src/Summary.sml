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

datatype summary =
    Summary of
      {requires : Context.context,
       provides : Context.context};

fun requires (Summary {requires = x, ...}) = x;

fun provides (Summary {provides = x, ...}) = x;

fun fromThmSet ths =
    let
      val requires = Context.fromSequentSet (ThmSet.axioms ths)

      val provides = Context.fromSequentSet (ThmSet.sequents ths)
    in
      Summary
        {requires = requires,
         provides = provides}
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
        val () = trace "entering Summary.fromThms\n"
*)
        val Summary {requires,provides} = summary

        val (inp,def) =
            let
              val req = Context.symbols requires
              val prov = Context.symbols provides
              val sym = Symbol.union req prov
            in
              Symbol.partitionUndef sym
            end

        val (ass,ax) =
            let
              val req = Context.sequents requires
            in
              SequentSet.partition (allSymbolsIn inp) req
            end

        val ths = Context.sequents provides

(*OpenTheoryTrace5
        val () = trace "exiting Summary.fromThms\n"
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
             (Print.addString prefix ::
              Print.addString name ::
              Print.addString ":" ::
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
                (Print.addString name ::
                 Print.addString ":" ::
                 map (Print.sequence Print.addNewline o ppSequent) seqs)
            end
      in
        fn sum =>
           let
             val Info {input,assumed,defined,axioms,thms} = sum
           in
             Print.blockProgram Print.Consistent 0
               [ppSymbol ("input",input),
                ppSequentSet ("assumed",assumed),
                Print.addNewline,
                ppSymbol ("defined",defined),
                ppSequentSet ("axioms",axioms),
                Print.addNewline,
                ppSequentSet ("thms",thms)]
           end
      end;
end;

fun ppWithShow show = Print.ppMap info (ppInfo show);

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
