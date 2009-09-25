(* ========================================================================= *)
(* ARTICLE SUMMARIES                                                         *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Summary :> Summary =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of article summary.                                                *)
(* ------------------------------------------------------------------------- *)

datatype currency =
    Currency of
      {types : NameSet.set,
       consts : NameSet.set,
       thms : SequentSet.set};

datatype summary =
    Summary of
      {requires : currency,
       provides : currency};

val emptyCurrency =
    Currency
      {types = NameSet.empty,
       consts = NameSet.empty,
       thms = SequentSet.empty};

local
  fun splitThm (th,(req,prov)) =
      let
        val Thm.Thm {axioms,sequent} = Thm.dest th
        val req = SequentSet.union req axioms
        val prov = SequentSet.add prov sequent
      in
        (req,prov)
      end;
in
  fun fromThms set =
      let
(*OpenTheoryTrace5
        val () = trace "entering Summary.fromThms\n"
*)
        val reqThms = SequentSet.empty
        val provThms = SequentSet.empty
        val (reqThms,provThms) = ThmSet.foldl splitThm (reqThms,provThms) set

        val reqTypes = SequentSet.typeOps reqThms

        val reqConsts = SequentSet.consts reqThms

        val provTypes = SequentSet.typeOps provThms
        val provTypes = NameSet.difference provTypes reqTypes

        val provConsts = SequentSet.consts provThms
        val provConsts = NameSet.difference provConsts reqConsts

        val requires =
            Currency
              {types = reqTypes,
               consts = reqConsts,
               thms = reqThms}

        val provides =
            Currency
              {types = provTypes,
               consts = provConsts,
               thms = provThms}

(*OpenTheoryTrace5
        val () = trace "exiting Summary.fromThms\n"
*)
      in
        Summary
          {requires = requires,
           provides = provides}
      end;
end

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun ppNameSet (name,ns) =
    Print.blockProgram Print.Consistent 2
      (Print.addString (name ^ ":") ::
       map (Print.sequence Print.addNewline o Name.pp) (NameSet.toList ns));

fun ppThmSet thms =
    Print.blockProgram Print.Consistent 2
      (Print.addString "thms:" ::
       map (Print.sequence Print.addNewline o ppThm o Thm.axiom)
         (SequentSet.toList thms));

fun ppCurrency (name, Currency {types,consts,thms}) =
    Print.blockProgram Print.Consistent 0
      [Print.blockProgram Print.Consistent 2
         [Print.addString (name ^ " {"),
          (if NameSet.null types then Print.skip
           else Print.sequence Print.addNewline (ppNameSet ("types",types))),
          (if NameSet.null consts then Print.skip
           else Print.sequence Print.addNewline (ppNameSet ("consts",consts))),
          (if SequentSet.null thms then Print.skip
           else Print.sequence Print.addNewline (ppThmSet thms))],
       Print.addBreak 1,
       Print.addString "}"];

fun pp (Summary {requires,provides}) =
    Print.blockProgram Print.Consistent 0
      [ppCurrency ("REQUIRES",requires),
       Print.addNewline,
       ppCurrency ("PROVIDES",provides)];

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
