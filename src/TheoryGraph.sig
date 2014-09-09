(* ========================================================================= *)
(* THEORY GRAPHS                                                             *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature TheoryGraph =
sig

(* ------------------------------------------------------------------------- *)
(* Ancestor theories.                                                        *)
(* ------------------------------------------------------------------------- *)

val parents : Theory.theory -> TheorySet.set

val ancestors : Theory.theory -> TheorySet.set  (* not including self *)

(* ------------------------------------------------------------------------- *)
(* Primitive theory packages cannot be replaced with their contents.         *)
(* ------------------------------------------------------------------------- *)

val primitives : Theory.theory -> TheorySet.set

val visiblePrimitives : Theory.theory -> Theory.theory list

(* ------------------------------------------------------------------------- *)
(* Theory summaries.                                                         *)
(* ------------------------------------------------------------------------- *)

val summary : Theory.theory -> PackageSummary.summary

(* ------------------------------------------------------------------------- *)
(* Theory environments.                                                      *)
(* ------------------------------------------------------------------------- *)

type environment

val emptyEnvironment : environment

val peekEnvironment :
    environment -> PackageTheory.name -> Theory.theory option

val insertEnvironment :
    environment -> PackageTheory.name * Theory.theory -> environment

val nestedEnvironment : environment -> Theory.nested

val mainEnvironment : environment -> Theory.theory

(* ------------------------------------------------------------------------- *)
(* A type of theory graphs.                                                  *)
(* ------------------------------------------------------------------------- *)

type graph

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : {savable : bool} -> graph

val theories : graph -> TheorySet.set

val member : Theory.theory -> graph -> bool

(* ------------------------------------------------------------------------- *)
(* Adding theories.                                                          *)
(* ------------------------------------------------------------------------- *)

val add : graph -> Theory.theory -> graph

(* ------------------------------------------------------------------------- *)
(* Looking up theories by package name.                                      *)
(* ------------------------------------------------------------------------- *)

val lookup : graph -> PackageNameVersion.nameVersion -> TheorySet.set

(* ------------------------------------------------------------------------- *)
(* Finding matching theories.                                                *)
(* ------------------------------------------------------------------------- *)

datatype specification =
    Specification of
      {imports : TheorySet.set,
       interpretation : Interpretation.interpretation,
       nameVersion : PackageNameVersion.nameVersion,
       checksum : Checksum.checksum option}

val match : graph -> specification -> TheorySet.set

(* ------------------------------------------------------------------------- *)
(* Importing theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

val importNode :
    PackageFinder.finder -> graph ->
    {directory : string,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     nodeImports : TheorySet.set,
     node : PackageTheory.node} ->
    graph * Theory.theory

val importTheory :
    PackageFinder.finder -> graph -> environment ->
    {directory : string,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     theory : PackageTheory.theory} ->
    graph * environment * Theory.theory

val importTheories :
    PackageFinder.finder -> graph ->
    {directory : string,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     theories : PackageTheory.theory list} ->
    graph * environment

val importPackageInformation :
    PackageFinder.finder -> graph ->
    {directory : string,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     nameVersion : PackageNameVersion.nameVersion,
     checksum : Checksum.checksum option,
     information : PackageInformation.information} ->
    graph * Theory.theory

val importPackage :
    PackageFinder.finder -> graph ->
    {imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     package : Package.package} ->
    graph * Theory.theory

val import :
    PackageFinder.finder -> graph -> specification -> graph * Theory.theory

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : graph Print.pp

end
