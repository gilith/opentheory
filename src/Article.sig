(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Article =
sig

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

type article

val new : {savable : bool} -> article

val saved : article -> ThmSet.set

val summarize : article -> Summary.summary

val savable : article -> bool

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val appendTextFile :
    {known : article,
     simulations : ObjectRead.simulations,
     interpretation : Interpretation.interpretation,
     filename : string} ->
    article -> article

val fromTextFile :
    {savable : bool,
     known : article,
     simulations : ObjectRead.simulations,
     interpretation : Interpretation.interpretation,
     filename : string} ->
    article

val toTextFile : {article : article, filename : string} -> unit

end
