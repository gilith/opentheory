(* ========================================================================= *)
(* OBJECT FUNCTION SIMULATIONS                                               *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectSimulation :> ObjectSimulation =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of object function simulations.                                    *)
(* ------------------------------------------------------------------------- *)

datatype simulation =
    Simulation of
      {seqs : (Thm.thm * ObjectProv.object) SequentMap.map,
       symbol : Symbol.symbol};

val empty =
    let
      val seqs = SequentMap.new ()

      val symbol = Symbol.empty
    in
      Simulation
        {seqs = seqs,
         symbol = symbol}
    end;

fun symbol (Simulation {symbol = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Adding thms simulated by a call object.                                   *)
(* ------------------------------------------------------------------------- *)

fun add sim (ths,obj) =
    let
      fun addSeq (th,seqs) = SequentMap.insert seqs (Thm.sequent th, (th,obj))

      val Simulation {seqs, symbol = sym} = sim

      val seqs = ThmSet.foldl addSeq seqs ths

      val sym = Symbol.addSequentSet sym (ThmSet.sequents ths)
    in
      Simulation {seqs = seqs, symbol = sym}
    end;

(* ------------------------------------------------------------------------- *)
(* Searching for theorems.                                                   *)
(* ------------------------------------------------------------------------- *)

fun search (Simulation {seqs,...}) seq =
    case SequentMap.peek seqs seq of
      NONE => NONE
    | SOME (th,obj) =>
      let
        val th = Rule.alpha seq th
      in
        SOME (th,obj)
      end;

end
