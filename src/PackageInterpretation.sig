(* ========================================================================= *)
(* PACKAGE INTERPRETATIONS                                                   *)
(* Copyright (c) 2016 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageInterpretation =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package interpretations.                                 *)
(* ------------------------------------------------------------------------- *)

datatype interpretation =
    Interpretation of
      {rewrites : Interpretation.rewrite list,
       filenames : {filename : string} list}

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
