(* ========================================================================= *)
(* SIMULATING THE HOL LIGHT THEOREM PROVER                                   *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature HolLight =
sig

(* ------------------------------------------------------------------------- *)
(* The HOL Light namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

val namespace : Namespace.namespace

(* ------------------------------------------------------------------------- *)
(* Simulations.                                                              *)
(* ------------------------------------------------------------------------- *)

val simulations :
    (Interpretation.interpretation -> Sequent.sequent ->
     Object.object -> Object.object) NameMap.map

end
