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
       theories : theory list,
       main : theory}
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

val destPackageNode : node -> PackageName.name option

val destPackage : theory -> PackageName.name option

val isPackage : theory -> bool

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

val ppId : id Print.pp

end
