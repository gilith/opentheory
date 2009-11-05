(* ========================================================================= *)
(* SIMULATING INFERENCE RULES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
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

type mkTypeOp = context -> Name.name -> TypeOp.typeOp option;

type mkConst = context -> Name.name -> Const.const option;

type mkThm = context -> Sequent.sequent -> Thm.thm option;

datatype simulation =
    Simulation of
      {mkTypeOp : mkTypeOp,
       mkConst : mkConst,
       mkThm : mkThm};

(* ------------------------------------------------------------------------- *)
(* Simulations that do nothing.                                              *)
(* ------------------------------------------------------------------------- *)

val skipMkTypeOp : mkTypeOp = fn _ => fn _ => NONE;

val skipMkConst : mkConst = fn _ => fn _ => NONE;

val skipMkThm : mkThm = fn _ => fn _ => NONE;

val skip =
    Simulation
      {mkTypeOp = skipMkTypeOp,
       mkConst = skipMkConst,
       mkThm = skipMkThm};

(* ------------------------------------------------------------------------- *)
(* Applying simulations.                                                     *)
(* ------------------------------------------------------------------------- *)

fun mkTypeOp (Simulation {mkTypeOp = x, ...}) = x;

fun mkConst (Simulation {mkConst = x, ...}) = x;

fun mkThm sim ctxt seq =
    let
      val Simulation {mkThm = mk, ...} = sim

      val result =
          case mk ctxt seq of
            SOME th => SOME (Rule.alpha seq th)
          | NONE => NONE
    in
      result
    end;

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
