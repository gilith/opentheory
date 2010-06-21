(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORIES                                               *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Theory =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theories.                                                       *)
(* ------------------------------------------------------------------------- *)

type theory

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype node =
    Article of
      {interpretation : Interpretation.interpretation,
       filename : string}
  | Package of
      {interpretation : Interpretation.interpretation,
       package : PackageName.name,
       theory : theory}
  | Union

datatype theory' =
    Theory' of
      {imports : theory list,
       node : node,
       article : Article.article}

val mk : theory' -> theory

val dest : theory -> theory'

val imports : theory -> theory list

val node : theory -> node

val article : theory -> Article.article

(* ------------------------------------------------------------------------- *)
(* Package theories.                                                         *)
(* ------------------------------------------------------------------------- *)

val packageNode : node -> PackageName.name option

val package : theory -> PackageName.name option

val isPackage : theory -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : theory Print.pp

(* ------------------------------------------------------------------------- *)
(* Theory IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

type id = int

val id : theory -> id

val equalId : id -> theory -> bool

val compare : theory * theory -> order

end
