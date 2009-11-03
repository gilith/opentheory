(* ========================================================================= *)
(* SIMULATING INFERENCE RULES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Simulation :> Simulation =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Simulating primitive inference rules.                                     *)
(* ------------------------------------------------------------------------- *)

datatype data =
    Data of
      {interpretation : Interpretation.interpretation,
       input : Object.object,
       target : Sequent.sequent};

type result = Thm.thm;

type simulation = data -> result;

(* ------------------------------------------------------------------------- *)
(* Simulation maps.                                                          *)
(* ------------------------------------------------------------------------- *)

type simulations = simulation NameMap.map;

val empty : simulations = NameMap.new ();

local
  fun first (_,s) = SOME s;

  fun second (_,s) = SOME s;

  fun both ((n,_),_) =
      raise Error ("Simulation.union: rule name clash: " ^ Name.toString n);
in
  fun union s1 s2 : simulations =
      NameMap.merge
        {first = first,
         second = second,
         both = both} s1 s2;
end;

local
  fun ins (s2,s1) = union s1 s2;
in
  fun unionList sl =
      case sl of
        [] => empty
      | s :: sl => List.foldl ins s sl;
end;

end
