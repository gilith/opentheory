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
      {input : Symbol.symbol,
       assumed : SequentSet.set,
       defined : Symbol.symbol,
       axioms : SequentSet.set,
       thms : ThmSet.set};

local
  fun classifyAxiom (seq,(inp,ass,def,ax)) =
      let
        val is_ass = true

        val ots = Sequent.typeOps seq
        val (inp_ots,def_ots) = TypeOpSet.partition TypeOp.isUndef ots
        val inp = Symbol.addTypeOpSet inp inp_ots
        val def = Symbol.addTypeOpSet def def_ots
        val is_ass = is_ass andalso TypeOpSet.null def_ots

        val cs = Sequent.consts seq
        val (inp_cs,def_cs) = ConstSet.partition Const.isUndef cs
        val inp = Symbol.addConstSet inp inp_cs
        val def = Symbol.addConstSet def def_cs
        val is_ass = is_ass andalso ConstSet.null def_cs
      in
        if is_ass then (inp, SequentSet.add ass seq, def, ax)
        else (inp, ass, def, SequentSet.add ax seq)
      end;
in
  fun fromThms thms =
      let
(*OpenTheoryTrace5
        val () = trace "entering Summary.fromThms\n"
*)
        val axioms = ThmSet.axioms thms

        val (input,assumed,defined,axioms) =
            SequentSet.foldl classifyAxiom
              (Symbol.empty,SequentSet.empty,Symbol.empty,SequentSet.empty)
              axioms

(*OpenTheoryTrace5
        val () = trace "exiting Summary.fromThms\n"
*)
      in
        Summary
          {input = input,
           assumed = assumed,
           defined = defined,
           axioms = axioms,
           thms = thms}
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

  fun ppThmSet (name,ths) = ppSequentSet (name, ThmSet.sequents ths);
in
  fun pp sum =
      let
        val Summary {input,assumed,defined,axioms,thms} = sum
      in
        Print.blockProgram Print.Consistent 0
          [ppSymbol ("input",input),
           ppSequentSet ("assumed",assumed),
           ppSymbol ("defined",defined),
           ppSequentSet ("axioms",axioms),
           ppThmSet ("thms",thms)]
      end;
end;

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
