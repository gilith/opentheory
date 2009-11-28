(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Article =
sig

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

type article

val empty : article

val saved : article -> ThmSet.set

val savable : article -> bool

(* ------------------------------------------------------------------------- *)
(* Appending articles.                                                       *)
(* ------------------------------------------------------------------------- *)

val append : article -> article -> article

val concat : article list -> article

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile :
    {savable : bool,
     known : article,
     simulations : Simulation.simulations,
     interpretation : Interpretation.interpretation,
     filename : string} ->
    article

val toTextFile : {article : article, filename : string} -> unit

end
