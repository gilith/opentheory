(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY GRAPHS                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Graph =
sig

(* ------------------------------------------------------------------------- *)
(* Ancestor theories.                                                        *)
(* ------------------------------------------------------------------------- *)

val parents : Theory.theory -> TheorySet.set

val ancestors : Theory.theory -> TheorySet.set  (* not including self *)

val primitives : Theory.theory -> TheorySet.set

val visiblePrimitives : Theory.theory -> Theory.theory list

(* ------------------------------------------------------------------------- *)
(* Theory environments.                                                      *)
(* ------------------------------------------------------------------------- *)

type environment

val emptyEnvironment : environment

val peekEnvironment :
    environment -> PackageTheory.name -> Theory.theory option

val insertEnvironment :
    environment -> PackageTheory.name * Theory.theory -> environment

val theoriesEnvironment : environment -> Theory.theory list

val mainEnvironment : environment -> Theory.theory

(***
(* ------------------------------------------------------------------------- *)
(* Packaging theories.                                                       *)
(* ------------------------------------------------------------------------- *)

val packageTheory :
    {expand : Theory.theory -> bool} ->
    Theory.theory -> PackageTheory.theory list
***)

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

val lookup : graph -> PackageName.name -> TheorySet.set

(* ------------------------------------------------------------------------- *)
(* Finding matching theories.                                                *)
(* ------------------------------------------------------------------------- *)

datatype specification =
    Specification of
      {imports : TheorySet.set,
       interpretation : Interpretation.interpretation,
       name : PackageName.name}

val match : graph -> specification -> TheorySet.set

(* ------------------------------------------------------------------------- *)
(* An importer is used to import theory packages into a graph.               *)
(* ------------------------------------------------------------------------- *)

type importer

val applyImporter :
    importer -> graph -> specification -> graph * Theory.theory

val fromFinderImporter : PackageFinder.finder -> importer

(* ------------------------------------------------------------------------- *)
(* Importing theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

val importNode :
    importer -> graph ->
    {directory : string,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     node : PackageTheory.node} ->
    graph * Theory.theory

val importTheory :
    importer -> graph -> environment ->
    {directory : string,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     theory : PackageTheory.theory} ->
    graph * environment * Theory.theory

val importTheories :
    importer -> graph ->
    {directory : string,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     theories : PackageTheory.theory list} ->
    graph * environment

val importPackage :
    importer -> graph ->
    {directory : string,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     name : PackageName.name,
     package : Package.package} ->
    graph * Theory.theory

val importPackageInfo :
    importer -> graph ->
    {imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     info : PackageInfo.info} ->
    graph * Theory.theory

val importPackageName :
    PackageFinder.finder -> graph -> specification -> graph * Theory.theory

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : graph Print.pp

end
