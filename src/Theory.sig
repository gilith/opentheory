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

datatype nested = Nested of (PackageTheory.name * theory) list

datatype node =
    Article of
      {filename : string}
  | Package of
      {package : PackageNameVersion.nameVersion,
       checksum : Checksum.checksum option,
       nested : nested}
  | Union

datatype theory' =
    Theory' of
      {imports : theory list,
       interpretation : Interpretation.interpretation,
       node : node,
       article : Article.article}

val mk : theory' -> theory

val dest : theory -> theory'

val imports : theory -> theory list

val interpretation : theory -> Interpretation.interpretation

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

(* ------------------------------------------------------------------------- *)
(* Union theories.                                                           *)
(* ------------------------------------------------------------------------- *)

val isUnionNode : node -> bool

val isUnion : theory -> bool

(* ------------------------------------------------------------------------- *)
(* Nested theories.                                                          *)
(* ------------------------------------------------------------------------- *)

val existsArticleNested : nested -> bool

val peekNested : PackageTheory.name -> nested -> theory option

val mainNested : nested -> theory

(* ------------------------------------------------------------------------- *)
(* Primitive theory packages cannot be replaced with their contents.         *)
(* ------------------------------------------------------------------------- *)

val isPrimitiveNode : node -> bool

val isPrimitive : theory -> bool

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
