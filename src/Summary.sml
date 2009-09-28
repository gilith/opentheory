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
  fun addTypeOps (inp,def,is_ass) seq =
      let
        val ots = Sequent.typeOps seq
        val (inp_ots,def_ots) = TypeOpSet.partition TypeOp.isUndef ots
        val inp = Symbol.addTypeOpSet inp inp_ots
        val def = Symbol.addTypeOpSet def def_ots
        val is_ass = is_ass andalso TypeOpSet.null def_ots
      in
        (inp,def,is_ass)
      end;

  fun addConsts (inp,def,is_ass) seq =
      let
        val cs = Sequent.consts seq
        val (inp_cs,def_cs) = ConstSet.partition Const.isUndef cs
        val inp = Symbol.addConstSet inp inp_cs
        val def = Symbol.addConstSet def def_cs
        val is_ass = is_ass andalso ConstSet.null def_cs
      in
        (inp,def,is_ass)
      end;

  fun addSequent (inp,def) seq =
      let
        val is_ass = true
        val (inp,def,is_ass) = addTypeOps (inp,def,is_ass) seq
        val (inp,def,is_ass) = addConsts (inp,def,is_ass) seq
      in
        (inp,def,is_ass)
      end;

  fun addAxiom (seq,(inp,ass,def,ax)) =
      let
        val (inp,def,is_ass) = addSequent (inp,def) seq
      in
        if is_ass then (inp, SequentSet.add ass seq, def, ax)
        else (inp, ass, def, SequentSet.add ax seq)
      end;

  fun addThm (th,(inp,def)) =
      let
        val (inp,def,_) = addSequent (inp,def) (Thm.sequent th)
      in
        (inp,def)
      end;
in
  fun fromThms ths =
      let
(*OpenTheoryTrace5
        val () = trace "entering Summary.fromThms\n"
*)
        val inp = Symbol.empty
        and def = Symbol.empty
        and ass = SequentSet.empty
        and ax = SequentSet.empty

        val (inp,ass,def,ax) =
            SequentSet.foldl addAxiom (inp,ass,def,ax) (ThmSet.axioms ths)

        val (inp,def) = ThmSet.foldl addThm (inp,def) ths

(*OpenTheoryTrace5
        val () = trace "exiting Summary.fromThms\n"
*)
      in
        Summary
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
