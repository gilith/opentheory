(* ========================================================================= *)
(* THEORY CONTEXTS                                                           *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Context =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory contexts.                                                *)
(* ------------------------------------------------------------------------- *)

type context

val empty : context

val symbols : context -> Symbol.symbol

val sequents : context -> SequentSet.set

val addSequent : context -> Sequent.sequent -> context

val addSequentSet : context -> SequentSet.set -> context

val fromSequentSet : SequentSet.set -> context

end
