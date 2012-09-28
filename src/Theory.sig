(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORIES                                               *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
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
       package : PackageNameVersion.nameVersion,
       theories : (PackageTheory.name * theory) list}
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
(* Article theories.                                                         *)
(* ------------------------------------------------------------------------- *)

val isArticleNode : node -> bool

val isArticle : theory -> bool

(* ------------------------------------------------------------------------- *)
(* Package theories.                                                         *)
(* ------------------------------------------------------------------------- *)

val destPackageNode : node -> PackageNameVersion.nameVersion option

val destPackage : theory -> PackageNameVersion.nameVersion option

val isPackage : theory -> bool

val existsArticleTheory : (PackageTheory.name * theory) list -> bool

val peekTheory :
    PackageTheory.name -> (PackageTheory.name * theory) list -> theory option

val mainTheory : (PackageTheory.name * theory) list -> theory

(* ------------------------------------------------------------------------- *)
(* Union theories.                                                           *)
(* ------------------------------------------------------------------------- *)

val isUnionNode : node -> bool

val isUnion : theory -> bool

(* ------------------------------------------------------------------------- *)
(* Primitive theories cannot be expanded.                                    *)
(* ------------------------------------------------------------------------- *)

val isPrimitiveNode : node -> bool

val isPrimitive : theory -> bool

(* ------------------------------------------------------------------------- *)
(* Creating PackageTheory nodes.                                             *)
(* ------------------------------------------------------------------------- *)

val toPackageTheoryNode : node -> PackageTheory.node

(* ------------------------------------------------------------------------- *)
(* Theory summaries.                                                         *)
(* ------------------------------------------------------------------------- *)

val summary : theory -> Summary.summary

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

val equal : theory -> theory -> bool

val ppId : id Print.pp

end
