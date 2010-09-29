(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Article =
sig

(* ------------------------------------------------------------------------- *)
(* Article filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

val isFilename : {filename : string} -> bool

val normalizeFilename : {filename : string} -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

type article

val empty : article

val thms : article -> Thms.thms

val savable : article -> bool

val objects : article -> ObjectThms.thms

(* ------------------------------------------------------------------------- *)
(* Merging articles.                                                         *)
(* ------------------------------------------------------------------------- *)

val union : article -> article -> article

val unionList : article list -> article

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile :
    {savable : bool,
     import : article,
     interpretation : Interpretation.interpretation,
     filename : string} ->
    article

val toTextFile : {article : article, filename : string} -> unit

end
