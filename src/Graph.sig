(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY GRAPHS                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Graph =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory graphs.                                                  *)
(* ------------------------------------------------------------------------- *)

type graph

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : graph

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
(* Ancestor theories.                                                        *)
(* ------------------------------------------------------------------------- *)

val parents : Theory.theory -> TheorySet.set

val ancestors : Theory.theory -> TheorySet.set  (* not including self *)

(* ------------------------------------------------------------------------- *)
(* Finding matching theories.                                                *)
(* ------------------------------------------------------------------------- *)

val match :
    graph ->
    {savable : bool,
     importsAtLeast : TheorySet.set,
     interpretationEquivalentTo : Interpretation.interpretation,
     package : PackageName.name} ->
    TheorySet.set

(* ------------------------------------------------------------------------- *)
(* Importing theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

val importTheory :
    graph ->
    {savable : bool,
     imports : TheorySet.set,
     simulations : Simulation.simulations,
     importToTheory : PackageRequire.name -> Theory.theory,
     interpretation : Interpretation.interpretation,
     package : PackageName.name option,
     directory : string,
     theory : PackageTheory.theory} ->
    graph * Theory.theory

val matchImportPackageName :
    graph ->
    {finder : PackageFinder.finder,
     savable : bool,
     simulations : Simulation.simulations,
     importsAtLeast : TheorySet.set,
     interpretationEquivalentTo : Interpretation.interpretation,
     package : PackageName.name} ->
    graph * Theory.theory

val importPackageName :
    graph ->
    {finder : PackageFinder.finder,
     savable : bool,
     simulations : Simulation.simulations,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     package : PackageName.name} ->
    graph * Theory.theory

val importPackage :
    graph ->
    {finder : PackageFinder.finder,
     savable : bool,
     simulations : Simulation.simulations,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     package : Package.package} ->
    graph * Theory.theory

val importContents :
    graph ->
    {finder : PackageFinder.finder,
     savable : bool,
     simulations : Simulation.simulations,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     package : PackageName.name option,
     directory : string,
     contents : PackageContents.contents} ->
    graph * Theory.theory

val importRequire :
    graph ->
    {finder : PackageFinder.finder,
     savable : bool,
     simulations : Simulation.simulations,
     imports : TheorySet.set,
     interpretation : Interpretation.interpretation,
     requireNameToTheory : PackageRequire.name -> Theory.theory,
     require : PackageRequire.require} ->
    graph * Theory.theory

(* ------------------------------------------------------------------------- *)
(* Compiling theories to package requirements.                               *)
(* ------------------------------------------------------------------------- *)

val mkRequires : TheorySet.set -> PackageRequire.require TheoryMap.map

end
