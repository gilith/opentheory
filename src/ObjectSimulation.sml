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
      {seqs : (Thm.thm * ObjectProv.object) SequentMap.map};

val empty =
    let
      val seqs = SequentMap.new ()
    in
      Simulation
        {seqs = seqs}
    end;

local
  fun addSym (seq,_,sym) = Symbol.addSequent sym seq;
in
  fun symbol (Simulation {seqs,...}) =
      SequentMap.foldl addSym Symbol.empty seqs;
end;

(* ------------------------------------------------------------------------- *)
(* Adding theorems simulated by a call object.                               *)
(* ------------------------------------------------------------------------- *)

fun add sim (ths,obj) =
    let
      fun addSeq (th,seqs) = SequentMap.insert seqs (Thm.sequent th, (th,obj))

      val Simulation {seqs} = sim

      val seqs = ThmSet.foldl addSeq seqs ths
    in
      Simulation {seqs = seqs}
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
