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
      {requires : instance list,
       interpretation : Interpretation.interpretation,
       package : Package.name option,
       theory : instance Theory.theory,
       article : Article.article}

val mk : instance' -> instance

val dest : instance -> instance'

val requires : instance -> instance list

val interpretation : instance -> Interpretation.interpretation

val package : instance -> Package.name option

val theory : instance -> instance Theory.theory

val article : instance -> Article.article

(* ------------------------------------------------------------------------- *)
(* Articles read by the instance theory.                                     *)
(* ------------------------------------------------------------------------- *)

val theoryArticles :
    instance -> (Interpretation.interpretation * {filename : string}) list

(* ------------------------------------------------------------------------- *)
(* Instances imported by the theory.                                         *)
(* ------------------------------------------------------------------------- *)

val theoryImports : instance -> instance list

(* ------------------------------------------------------------------------- *)
(* Creating instances of theory packages.                                    *)
(* ------------------------------------------------------------------------- *)

val fromPackage :
    {directory : string,
     Package.requireName -> instance,
     interpretation : Interpretation.interpretation,
     package : Package.name option,
     contents : Package.package} ->
    instance

(* ------------------------------------------------------------------------- *)
(* Instance IDs.                                                             *)
(* ------------------------------------------------------------------------- *)

type id = int

val id : instance -> id

val equalId : id -> instance -> bool

val compare : instance * instance -> order

end
