(* ========================================================================= *)
(* ARTICLE SUMMARIES                                                         *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Summary :> Summary =
struct

open Useful Syntax;

(* ------------------------------------------------------------------------- *)
(* A type of article summary.                                                *)
(* ------------------------------------------------------------------------- *)

datatype currency =
    Currency of
      {types : NameSet.set,
       consts : NameSet.set,
       thms : ThmSet.set};

datatype summary =
    Summary of
      {requires : currency,
       provides : currency};

val emptyCurrency =
    Currency
      {types = NameSet.empty,
       consts = NameSet.empty,
       thms = ThmSet.empty};

fun fromThms set =
    let
      val requires = emptyCurrency

      val provides = emptyCurrency
    in
      Summary
        {requires = requires,
         provides = provides}
    end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun ppNameSet (name,ns) =
    Print.blockProgram Print.Consistent 2
      (Print.addString (name ^ ":") ::
       map (Print.sequence Print.addNewline o Name.pp) (NameSet.toList ns));

fun ppCurrency (name, Currency {types,consts,thms}) =
    Print.blockProgram Print.Consistent 0
      [Print.addString (name ^ " {"),
       Print.blockProgram Print.Consistent 2
         [Print.addNewline,
          ppNameSet ("types",types)],
       Print.addNewline,
       Print.addString "}",
       Print.addNewline];

fun pp (Summary {requires,provides}) =
    Print.blockProgram Print.Consistent 0
      [ppCurrency ("REQUIRES",requires),
       Print.addNewline,
       ppCurrency ("PROVIDES",provides)];

fun toTextFile filename = Stream.toTextFile filename o Print.toStream pp;

end
