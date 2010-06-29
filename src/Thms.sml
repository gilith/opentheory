(* ========================================================================= *)
(* THEOREMS AND THEIR SYMBOLS                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Thms :> Thms =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theorems and their symbols.                                     *)
(* ------------------------------------------------------------------------- *)

datatype thms =
    Thms of
      {ths : ThmSet.set,
       symbol : Symbol.symbol};

val empty =
    let
      val ths = ThmSet.empty
      and symbol = Symbol.empty
    in
      Thms
        {ths = ths,
         symbol = symbol}
    end;

fun thms (Thms {ths = x, ...}) = x;

fun symbol (Thms {symbol = x, ...}) = x;

fun size ths = ThmSet.size (thms ths);

(* ------------------------------------------------------------------------- *)
(* Adding theorems.                                                          *)
(* ------------------------------------------------------------------------- *)

fun add thms th =
    let
      val Thms {ths, symbol = sym} = thms

      val ths = ThmSet.add ths th

      val sym = Symbol.addSequent sym (Thm.sequent th)
    in
      Thms
        {ths = ths,
         symbol = sym}
    end;

local
  fun add1 (th,thms) = add thms th;
in
  fun addList thms thl = List.foldl add1 thms thl;

  fun addSet thms ths = ThmSet.foldl add1 thms ths;
end;

val singleton = add empty;

val fromList = addList empty;

val fromSet = addSet empty;

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

fun union thms1 thms2 =
    let
      val Thms {ths = ths1, symbol = sym1} = thms1
      and Thms {ths = ths2, symbol = sym2} = thms2

      val ths = ThmSet.union ths1 ths2

      val sym = Symbol.union sym1 sym2
    in
      Thms
        {ths = ths,
         symbol = sym}
    end;

local
  fun uncurriedUnion (thms1,thms2) = union thms1 thms2;
in
  fun unionList thmsl =
      case thmsl of
        [] => empty
      | thms :: thmsl => List.foldl uncurriedUnion thms thmsl;
end;

(* ------------------------------------------------------------------------- *)
(* Searching for theorems.                                                   *)
(* ------------------------------------------------------------------------- *)

fun search (Thms {ths,...}) seq =
    case ThmSet.peek ths (Thm.axiom seq) of
      NONE => NONE
    | SOME th => SOME (Rule.alpha seq th);

end
