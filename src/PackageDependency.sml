(* ========================================================================= *)
(* PACKAGE DEPENDENCY GRAPHS                                                 *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageDependency :> PackageDependency =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of package dependency graphs.                                      *)
(* ------------------------------------------------------------------------- *)

datatype dependency =
    Dependency of
      {packages : PackageNameVersionSet.set,
       includes : PackageNameVersionGraph.graph,
       requires : PackageNameVersionGraph.graph,
       requiresMissing : PackageNameVersionSet.set,
       subtheories : PackageNameVersionGraph.graph,
       subtheoriesMissing : PackageNameVersionSet.set};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val packages = PackageNameVersionSet.empty
      and includes = PackageNameVersionGraph.empty
      and requires = PackageNameVersionGraph.empty
      and requiresMissing = PackageNameVersionSet.empty
      and subtheories = PackageNameVersionGraph.empty
      and subtheoriesMissing = PackageNameVersionSet.empty
    in
      Dependency
        {packages = packages,
         includes = includes,
         requires = requires,
         requiresMissing = requiresMissing,
         subtheories = subtheories,
         subtheoriesMissing = subtheoriesMissing}
    end;

fun packages (Dependency {packages = x, ...}) = x;

fun size dep = PackageNameVersionSet.size (packages dep);

fun member namever dep = PackageNameVersionSet.member namever (packages dep);

(* ------------------------------------------------------------------------- *)
(* Adding package dependencies.                                              *)
(* ------------------------------------------------------------------------- *)

fun addInfo latest dep info =
    let
      val namever = PackageInfo.nameVersion info
    in
      if member namever dep then dep
      else
        let
          fun partReq (name,(reqs,missing)) =
              case latest name of
                SOME nv => (PackageNameVersionSet.add reqs nv, missing)
              | NONE => (reqs, name :: missing)

          fun addEdge (nv,graph) =
              PackageNameVersionGraph.addEdge graph (nv,namever)

          fun addEdges graph nvs =
              PackageNameVersionSet.foldl addEdge graph nvs

          val name = PackageNameVersion.name namever

          val incs = PackageInfo.includes info
          and reqs = PackageInfo.requires info

          val (reqs,missing) =
              List.foldl partReq (PackageNameVersionSet.empty,[]) reqs

          val reqsOK = List.null missing

          val subs =
              PackageNameVersionSet.filter
                (PackageNameVersion.isStrictPrefixName namever)
                (PackageNameVersionSet.union incs reqs)

          val subsOK =
              not (List.exists (PackageName.isStrictPrefix name) missing)

          val Dependency
                {packages,
                 includes,
                 requires,
                 requiresMissing,
                 subtheories,
                 subtheoriesMissing} = dep

          val packages = PackageNameVersionSet.add packages namever

          val includes = addEdges includes incs

          val requires = addEdges requires reqs

          val requiresMissing =
              if reqsOK then requiresMissing
              else PackageNameVersionSet.add requiresMissing namever

          val subtheories = addEdges subtheories subs

          val subtheoriesMissing =
              if subsOK then subtheoriesMissing
              else PackageNameVersionSet.add subtheoriesMissing namever
        in
          Dependency
            {packages = packages,
             includes = includes,
             requires = requires,
             requiresMissing = requiresMissing,
             subtheories = subtheories,
             subtheoriesMissing = subtheoriesMissing}
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

fun requiresInstalled dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.requiresInstalled: unknown package"
*)
      val Dependency {requiresMissing = s, ...} = dep
    in
      not (PackageNameVersionSet.member namever s)
    end;

fun requires dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.requires: unknown package"
*)
      val Dependency {requires = g, ...} = dep
    in
      PackageNameVersionGraph.parents g namever
    end;

fun requiredBy dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.requiredBy: unknown package"
*)
      val Dependency {requires = g, ...} = dep
    in
      PackageNameVersionGraph.children g namever
    end;

fun isRequired dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.isRequired: unknown package"
*)
      val Dependency {requires = g, ...} = dep

      val chil = PackageNameVersionGraph.children g namever
    in
      not (PackageNameVersionSet.null chil)
    end;

(* ------------------------------------------------------------------------- *)
(* Included packages.                                                        *)
(* ------------------------------------------------------------------------- *)

fun includes dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.includes: unknown package"
*)
      val Dependency {includes = g, ...} = dep
    in
      PackageNameVersionGraph.parents g namever
    end;

fun includedBy dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.includedBy: unknown package"
*)
      val Dependency {includes = g, ...} = dep
    in
      PackageNameVersionGraph.children g namever
    end;

fun isIncluded dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.isIncluded: unknown package"
*)
      val Dependency {includes = g, ...} = dep

      val chil = PackageNameVersionGraph.children g namever
    in
      not (PackageNameVersionSet.null chil)
    end;

(* ------------------------------------------------------------------------- *)
(* Subtheory packages.                                                       *)
(* ------------------------------------------------------------------------- *)

fun subtheoriesInstalled dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.subtheoriesInstalled: unknown package"
*)
      val Dependency {subtheoriesMissing = s, ...} = dep
    in
      not (PackageNameVersionSet.member namever s)
    end;

fun subtheories dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.subtheories: unknown package"
*)
      val Dependency {subtheories = g, ...} = dep
    in
      PackageNameVersionGraph.parents g namever
    end;

fun subtheoryOf dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.subtheoryOf: unknown package"
*)
      val Dependency {subtheories = g, ...} = dep
    in
      PackageNameVersionGraph.children g namever
    end;

fun isSubtheory dep namever =
    let
(*OpenTheoryDebug
      val _ = member namever dep orelse
              raise Bug "PackageDependency.isSubtheory: unknown package"
*)
      val Dependency {subtheories = g, ...} = dep

      val chil = PackageNameVersionGraph.children g namever
    in
      not (PackageNameVersionSet.null chil)
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppSet n s =
      Print.inconsistentBlock 2
        [Print.ppString n,
         Print.space,
         Print.ppString "=",
         Print.break,
         Print.ppString (PackageNameVersionSet.toString s)];

  fun ppGraph n g =
      Print.inconsistentBlock 2
        [Print.ppString n,
         Print.space,
         Print.ppString "=",
         Print.break,
         Print.ppString (PackageNameVersionGraph.toString g)];
in
  fun pp dep =
      let
        val Dependency
              {packages,
               includes,
               requires,
               requiresMissing,
               subtheories,
               subtheoriesMissing} = dep
      in
        Print.consistentBlock 1
          [Print.ppString "{",
           ppSet "packages" packages,
           Print.ppString ",",
           Print.break,
           ppGraph "includes" includes,
           Print.ppString ",",
           Print.break,
           ppGraph "requires" requires,
           Print.ppString ",",
           Print.break,
           ppSet "packages with uninstalled requires" requiresMissing,
           Print.ppString ",",
           Print.break,
           ppGraph "subtheories" subtheories,
           Print.ppString ",",
           Print.break,
           ppSet "packages with uninstalled subtheories" subtheoriesMissing,
           Print.ppString "}"]
      end;
end;

val toString = Print.toString pp;

end
