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

type mkTypeOp = context -> Name.name -> TypeOp.typeOp option

type mkConst = context -> Name.name -> Const.const option

type mkThm = context -> Sequent.sequent -> Thm.thm option

datatype simulation =
    Simulation of
      {mkTypeOp : mkTypeOp,
       mkConst : mkConst,
       mkThm : mkThm}

(* ------------------------------------------------------------------------- *)
(* Simulations that do nothing.                                              *)
(* ------------------------------------------------------------------------- *)

val skipMkTypeOp : mkTypeOp

val skipMkConst : mkConst

val skipMkThm : mkThm

val skip : simulation

(* ------------------------------------------------------------------------- *)
(* Applying simulations.                                                     *)
(* ------------------------------------------------------------------------- *)

val mkTypeOp : simulation -> mkTypeOp

val mkConst : simulation -> mkConst

val mkThm : simulation -> mkThm

(* ------------------------------------------------------------------------- *)
(* Simulation maps.                                                          *)
(* ------------------------------------------------------------------------- *)

type simulations = simulation NameMap.map

val empty : simulations

val union : simulations -> simulations -> simulations

val unionList : simulations list -> simulations

end
