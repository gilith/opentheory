(* ========================================================================= *)
(* SIMULATING INFERENCE RULES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure Simulation :> Simulation =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of inference rule simulations.                                     *)
(* ------------------------------------------------------------------------- *)

datatype context =
    Context of
      {interpretation : Interpretation.interpretation,
       input : Object.object};

datatype result =
    Result of
      {input : Object.object option,
       thms : ThmSet.set};

datatype simulation = Simulation of context -> result;

(* ------------------------------------------------------------------------- *)
(* Simulation maps.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype simulations = Simulations of simulation NameMap.map;

val empty = Simulations (NameMap.new ());

fun peek (Simulations s) n = NameMap.peek s n;

local
  fun first (_,s) = SOME s;

  fun second (_,s) = SOME s;

  fun both ((n,_),_) =
      raise Error ("Simulation.union: rule name clash: " ^ Name.toString n);
in
  fun union (Simulations s1) (Simulations s2) =
      let
        val s =
            NameMap.merge
              {first = first,
               second = second,
               both = both} s1 s2
      in
        Simulations s
      end;
end;

local
  fun ins (s2,s1) = union s1 s2;
in
  fun unionList sl =
      case sl of
        [] => empty
      | s :: sl => List.foldl ins s sl;
end;

fun fromList l = Simulations (NameMap.fromList l);

end
