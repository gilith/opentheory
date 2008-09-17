(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Article =
sig

(* ------------------------------------------------------------------------- *)
(* Articles                                                                  *)
(* ------------------------------------------------------------------------- *)

type article

val saved : article -> ThmSet.set

(* ------------------------------------------------------------------------- *)
(* I/O                                                                       *)
(* ------------------------------------------------------------------------- *)

val fromTextFile :
    {known : ThmSet.set,
     interpretation : Interpretation.interpretation,
     filename : string} ->
    article

val toTextFile : {filename : string, article : article} -> unit

end
