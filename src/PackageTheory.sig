(* ========================================================================= *)
(* PACKAGE THEORY SYNTAX                                                     *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageTheory =
sig

(* ------------------------------------------------------------------------- *)
(* Types of package theory syntax.                                           *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name

datatype node =
    Article of
      {interpretation : PackageInterpretation.interpretation,
       filename : string}
  | Include of
      {interpretation : PackageInterpretation.interpretation,
       package : PackageNameVersion.nameVersion,
       checksum : Checksum.checksum option}
  | Union

datatype theory =
    Theory of
      {name : name,
       imports : name list,
       node : node}

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name : theory -> name

val imports : theory -> name list

val node : theory -> node

(* ------------------------------------------------------------------------- *)
(* Generating fresh theory names.                                            *)
(* ------------------------------------------------------------------------- *)

val variantName : {avoid : PackageNameSet.set} -> name -> name

(* ------------------------------------------------------------------------- *)
(* The main theory.                                                          *)
(* ------------------------------------------------------------------------- *)

val mainName : name

val isMainName : name -> bool

val isMain : theory -> bool

val emptyMain : theory -> bool

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val articleNode : node -> {filename : string} option

val isArticleNode : node -> bool

val article : theory -> {filename : string} option

val articles : theory list -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Interpretations.                                                          *)
(* ------------------------------------------------------------------------- *)

val interpretationNode : node -> PackageInterpretation.interpretation option

val interpretation : theory -> PackageInterpretation.interpretation option

val interpretations : theory list -> PackageInterpretation.interpretation list

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val includeNode :
    node ->
    (PackageNameVersion.nameVersion * Checksum.checksum option) option

val destInclude :
    theory ->
    (PackageNameVersion.nameVersion * Checksum.checksum option) option

val includes :
    theory list ->
    (PackageNameVersion.nameVersion * Checksum.checksum option) list

val updateIncludeNode :
    (PackageNameVersion.nameVersion -> Checksum.checksum option ->
     (PackageNameVersion.nameVersion * Checksum.checksum option) option) ->
    node -> node option

val updateInclude :
    (PackageNameVersion.nameVersion -> Checksum.checksum option ->
     (PackageNameVersion.nameVersion * Checksum.checksum option) option) ->
    theory -> theory option

val updateIncludes :
    (PackageNameVersion.nameVersion -> Checksum.checksum option ->
     (PackageNameVersion.nameVersion * Checksum.checksum option) option) ->
    theory list -> theory list option

(* ------------------------------------------------------------------------- *)
(* Union dependencies.                                                       *)
(* ------------------------------------------------------------------------- *)

val isUnionNode : node -> bool

val destUnion : theory -> name list option

val isUnion : theory -> bool

(* ------------------------------------------------------------------------- *)
(* Topological sort of theories.                                             *)
(* ------------------------------------------------------------------------- *)

type index

val toListIndex : index -> theory list

val fromListIndex : theory list -> index

val peekIndex : index -> name -> theory option

val getIndex : index -> name -> theory

val memberIndex : name -> index -> bool

val mainIndex : index -> theory

val sortIndex : {parents : theory -> name list} -> index -> theory list

val sortImports : theory list -> theory list

val sortUnion : theory list -> theory list

val sortedImports : theory list -> bool

val sortedUnion : theory list -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppFilename : {filename : string} Print.pp

val pp : theory Print.pp

val ppList : theory list Print.pp

val toStringFilename : {filename : string} -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parserFilename : (char, {filename : string}) Parse.parser

val parser : (char,theory) Parse.parser

val fromStringFilename : string -> {filename : string}

end
