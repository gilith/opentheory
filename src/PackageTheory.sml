(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC USED IN PACKAGES                           *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageTheory :> PackageTheory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val theoryKeywordString = "theory";

(* ------------------------------------------------------------------------- *)
(* Types of package theory syntax.                                           *)
(* ------------------------------------------------------------------------- *)

type theory = PackageRequire.name Theory.theory;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppTheoryKeyword = Print.addString theoryKeywordString;

fun pp thy =
    let
      val thy =
          case thy of
            Theory.Sequence _ => thy
          | _ => Theory.Sequence [thy]
    in
      Print.blockProgram Print.Consistent 0
        [ppTheoryKeyword,
         Print.addString " ",
         Theory.pp PackageRequire.ppName thy]
    end;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val theoryKeywordParser = exactString theoryKeywordString;

  val theoryParser =
      (theoryKeywordParser ++ manySpace ++
       Theory.parser PackageRequire.parserName) >>
      (fn ((),((),thy)) =>
          case thy of
            Theory.Sequence [thy] => thy
          | _ => thy);

  val theorySpaceParser = theoryParser ++ manySpace >> fst;
in
  val parser = manySpace ++ theorySpaceParser >> snd;
end;

end
