(* ========================================================================= *)
(* OBJECT FUNCTION SIMULATIONS                                               *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectSimulation =
sig

(* ------------------------------------------------------------------------- *)
(* A type of object function simulations.                                    *)
(* ------------------------------------------------------------------------- *)

type simulation

val empty : simulation

val symbol : simulation -> Symbol.symbol

(* ------------------------------------------------------------------------- *)
(* Adding theorems simulated by a call object.                               *)
(* ------------------------------------------------------------------------- *)

val add : simulation -> ThmSet.set * ObjectProv.object -> simulation

(* ------------------------------------------------------------------------- *)
(* Searching for theorems.                                                   *)
(* ------------------------------------------------------------------------- *)

val search :
    simulation -> Sequent.sequent -> (Thm.thm * ObjectProv.object) option

end
