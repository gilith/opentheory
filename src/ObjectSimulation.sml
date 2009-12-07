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
       symbol : Symbol.symbol Lazy.lazy};

val empty =
    let
      val seqs = SequentMap.new ()

      val sym = Lazy.quickly Symbol.empty
    in
      Simulation
        {seqs = seqs,
         symbol = sym}
    end;

fun symbol (Simulation {symbol = x, ...}) = Lazy.force x;

(* ------------------------------------------------------------------------- *)
(* Adding theorems simulated by a call object.                               *)
(* ------------------------------------------------------------------------- *)

local
  fun addTh obj (th,(seqs,seql)) =
      let
        val seq = Thm.sequent th

        val seqs = SequentMap.insert seqs (seq,(th,obj))

        val seql = seq :: seql
      in
        (seqs,seql)
      end;

  fun addSym sym seql () = Symbol.addSequentList (Lazy.force sym) seql;
in
  fun add sim (ths,obj) =
      let
        val Simulation {seqs, symbol = sym} = sim

        val (seqs,seql) = ThmSet.foldl (addTh obj) (seqs,[]) ths

        val sym = Lazy.delay (addSym sym seql)
      in
        Simulation
          {seqs = seqs,
           symbol = sym}
      end;
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
