(* ========================================================================= *)
(* SIMULATING INFERENCE RULES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Simulation =
sig

(* ------------------------------------------------------------------------- *)
(* Simulating primitive inference rules.                                     *)
(* ------------------------------------------------------------------------- *)

datatype data =
    Data of
      {interpretation : Interpretation.interpretation,
       input : Object.object,
       target : Sequent.sequent}

type result = Thm.thm

type simulation = data -> result

(* ------------------------------------------------------------------------- *)
(* Simulation maps.                                                          *)
(* ------------------------------------------------------------------------- *)

type simulations = simulation NameMap.map

val empty : simulations

val union : simulations -> simulations -> simulations

val unionList : simulations list -> simulations

end
