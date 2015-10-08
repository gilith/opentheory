(* ========================================================================= *)
(* ARTICLES OF PROOF IN HIGHER ORDER LOGIC                                   *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Article =
sig

(* ------------------------------------------------------------------------- *)
(* Article filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

val mkFilename : {base : string} -> {filename : string}

val destFilename : {filename : string} -> {base : string} option

val isFilename : {filename : string} -> bool

val normalizeFilename : {filename : string} -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

type article

val empty : article

val thms : article -> Thms.thms

val inference : article -> Inference.inference

val savable : article -> bool

(* ------------------------------------------------------------------------- *)
(* Merging articles.                                                         *)
(* ------------------------------------------------------------------------- *)

val union : article -> article -> article

val unionList : article list -> article

(* ------------------------------------------------------------------------- *)
(* Article summaries.                                                        *)
(* ------------------------------------------------------------------------- *)

val summary : article -> Summary.summary

(* ------------------------------------------------------------------------- *)
(* Article symbols.                                                          *)
(* ------------------------------------------------------------------------- *)

val symbols : article -> SymbolSet.set

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile :
    {savable : bool,
     import : article,
     interpretation : Interpretation.interpretation,
     filename : string} ->
    article

val toTextFile :
    {article : article,
     version : ArticleVersion.version,
     clearLocalNames : bool,
     filename : string} -> unit

end
