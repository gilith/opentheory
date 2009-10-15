(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY INSTANCES                                       *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Instance =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory instances.                                               *)
(* ------------------------------------------------------------------------- *)

type instance

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype instance' =
    Instance' of
      {package : Package.name option,
       interpretation : Interpretation.interpretation,
       import : instance list,
       theory : instance Theory.theory,
       article : Article.article}

val mk : instance' -> instance

val dest : instance -> instance'

val package : instance -> Package.name option

val interpretation : instance -> Interpretation.interpretation

val import : instance -> instance list

val theory : instance -> instance Theory.theory

(* ------------------------------------------------------------------------- *)
(* Articles read by the instance theory.                                     *)
(* ------------------------------------------------------------------------- *)

val theoryArticles :
    instance -> (Interpretation.interpretation * {filename : string}) list

(* ------------------------------------------------------------------------- *)
(* Instances imported by the theory.                                         *)
(* ------------------------------------------------------------------------- *)

val theoryImported : instance -> instance list

(* ------------------------------------------------------------------------- *)
(* Instance IDs.                                                             *)
(* ------------------------------------------------------------------------- *)

type id = int

val id : instance -> id

val equalId : id -> instance -> bool

val compare : instance * instance -> order

end
