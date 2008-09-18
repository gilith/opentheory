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

fun ppNameSet p (name,ns) =
    (Parser.beginBlock p Parser.Consistent 2;
     Parser.addString p (name ^ ":");
     NameSet.app (fn n => (Parser.addNewline p; Name.pp p n)) ns;
     Parser.endBlock p);

fun ppCurrency p (name, Currency {types,consts,thms}) =
    (Parser.beginBlock p Parser.Consistent 0;
     Parser.addString p (name ^ " {");
     Parser.addNewline p;
     Parser.beginBlock p Parser.Consistent 2;
     ppNameSet p ("types",types);
     Parser.endBlock p;
     Parser.addNewline p;
     Parser.addString p "}";
     Parser.addNewline p;
     Parser.endBlock p);

fun pp p (Summary {requires,provides}) =
    (Parser.beginBlock p Parser.Consistent 0;
     ppCurrency p ("REQUIRES",requires);
     Parser.addNewline p;
     ppCurrency p ("PROVIDES",provides);
     Parser.endBlock p);

fun toTextFile filename = Stream.toTextFile filename o Parser.toStream pp;

end
