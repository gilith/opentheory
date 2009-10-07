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
  fun ppNameList (name,names) =
      if null names then Print.skip
      else
        Print.sequence
          (Print.blockProgram Print.Inconsistent 2
             (Print.addString (name ^ ":") ::
              map (Print.sequence (Print.addBreak 1) o Name.pp) names))
          Print.addNewline;

  fun typeOpName (ot,l) = TypeOp.name ot :: l;

  fun constName (c,l) = Const.name c :: l;

  fun ppSymbol (prefix,sym) =
      let
        val ots = TypeOpSet.foldr typeOpName [] (Symbol.typeOps sym)
        val cs = ConstSet.foldr constName [] (Symbol.consts sym)
      in
        Print.sequence
          (ppNameList (prefix ^ "-types", ots))
          (ppNameList (prefix ^ "-consts", cs))
      end;

  val ppThm = Thm.pp;

  val ppSequent = Print.ppMap Thm.axiom ppThm;

  fun ppSequentSet (name,seqs) =
      let
        val seqs = SequentSet.toList seqs
      in
        Print.sequence
          (Print.blockProgram Print.Consistent 2
             (Print.addString (name ^ ":") ::
              map (Print.sequence (Print.addBreak 1) o ppSequent) seqs))
          Print.addNewline
      end;
in
  fun ppInfo sum =
      let
        val Info {input,assumed,defined,axioms,thms} = sum
      in
        Print.blockProgram Print.Consistent 0
          [ppSymbol ("input",input),
           ppSequentSet ("assumed",assumed),
           ppSymbol ("defined",defined),
           ppSequentSet ("axioms",axioms),
           ppSequentSet ("thms",thms)]
      end;
end;

val pp = Print.ppMap info ppInfo;

fun toTextFile {summary,filename} =
    let
(*OpenTheoryTrace5
      val () = trace "entering Summary.toTextFile\n"
*)
      val lines = Print.toStream pp summary

      val () = Stream.toTextFile {filename = filename} lines

(*OpenTheoryTrace5
      val () = trace "exiting Summary.toTextFile\n"
*)
    in
      ()
    end;

end
