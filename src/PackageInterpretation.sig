(* ========================================================================= *)
(* PACKAGE INTERPRETATIONS                                                   *)
(* Copyright (c) 2016 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageInterpretation =
sig

(* ------------------------------------------------------------------------- *)
(* Interpretation filenames.                                                 *)
(* ------------------------------------------------------------------------- *)

val mkFilename : {base : string} -> {filename : string}

val destFilename : {filename : string} -> {base : string} option

val isFilename : {filename : string} -> bool

val normalizeFilename : {filename : string} -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* A type of theory package interpretations.                                 *)
(* ------------------------------------------------------------------------- *)

datatype interpretation =
    Interpretation of
      {rewrites : Interpretation.rewrite list,
       filenames : {filename : string} list}

(* ------------------------------------------------------------------------- *)
(* Interpretation files.                                                     *)
(* ------------------------------------------------------------------------- *)

val filenames : interpretation -> {filename : string} list

val filenamesList : interpretation list -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : interpretation * interpretation -> order

val equal : interpretation -> interpretation -> bool

(* ------------------------------------------------------------------------- *)
(* Realizing the theory package interpretation.                              *)
(* ------------------------------------------------------------------------- *)

val realize :
    {directory : string} -> interpretation -> Interpretation.interpretation

end
