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
       package : PackageName.name option,
       theory : instance Theory.theory,
       article : Article.article,
       thms : ThmSet.set,
       summary : Summary.summary}

val mk : instance' -> instance

val dest : instance -> instance'

val requires : instance -> instance list

val interpretation : instance -> Interpretation.interpretation

val package : instance -> PackageName.name option

val theory : instance -> instance Theory.theory

val article : instance -> Article.article

val thms : instance -> ThmSet.set

val summary : instance -> Summary.summary

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

val fromTheory :
    {savable : bool,
     requires : instance list,
     simulations : ObjectRead.simulations,
     importToInstance : 'a -> instance,
     interpretation : Interpretation.interpretation,
     directory : string,
     package : PackageName.name option,
     theory : 'a Theory.theory} ->
    instance

(* ------------------------------------------------------------------------- *)
(* Instance IDs.                                                             *)
(* ------------------------------------------------------------------------- *)

type id = int

val id : instance -> id

val equalId : id -> instance -> bool

val compare : instance * instance -> order

end
