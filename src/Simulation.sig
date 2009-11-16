(* ========================================================================= *)
(* SIMULATING INFERENCE RULES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Simulation =
sig

(* ------------------------------------------------------------------------- *)
(* A type of inference rule simulations.                                     *)
(* ------------------------------------------------------------------------- *)

datatype context =
    Context of
      {interpretation : Interpretation.interpretation,
       input : Object.object}

datatype result =
    Result of
      {input : Object.object option,
       thms : ThmSet.set}

datatype simulation = Simulation of context -> result

(* ------------------------------------------------------------------------- *)
(* Simulation maps.                                                          *)
(* ------------------------------------------------------------------------- *)

type simulations

val empty : simulations

val peek : simulations -> Name.name -> simulation option

val union : simulations -> simulations -> simulations

val unionList : simulations list -> simulations

val fromList : (Name.name * simulation) list -> simulations

end
