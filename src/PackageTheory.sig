(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC USED IN PACKAGES                           *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageTheory =
sig

(* ------------------------------------------------------------------------- *)
(* Types of package theory syntax.                                           *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name

datatype node =
    Article of
      {interpretation : Interpretation.interpretation,
       filename : string}
  | Package of
      {interpretation : Interpretation.interpretation,
       package : PackageNameVersion.nameVersion}
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

val mkName : {avoid : PackageNameSet.set} -> name -> name

(* ------------------------------------------------------------------------- *)
(* The main theory.                                                          *)
(* ------------------------------------------------------------------------- *)

val mainName : name

val isMainName : name -> bool

val isMain : theory -> bool

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val articleNode : node -> {filename : string} option

val isArticleNode : node -> bool

val article : theory -> {filename : string} option

val articles : theory list -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val packageNode : node -> PackageNameVersion.nameVersion option

val package : theory -> PackageNameVersion.nameVersion option

val packages : theory list -> PackageNameVersion.nameVersion list

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

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppName : name Print.pp

val ppFilename : {filename : string} Print.pp

val pp : theory Print.pp

val ppList : theory list Print.pp

val toStringName : name -> string

val toStringFilename : {filename : string} -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parserName : (char,name) Parse.parser

val parserFilename : (char, {filename : string}) Parse.parser

val parser : (char,theory) Parse.parser

val fromStringFilename : string -> {filename : string}

end
