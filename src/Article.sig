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
    {filename : string,
     interpretation : Interpretation.interpretation} ->
    article -> article

val toTextFile : {article : article, filename : string} -> unit

end
