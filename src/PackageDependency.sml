(* ========================================================================= *)
(* PACKAGE DEPENDENCY GRAPHS                                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageDependency :> PackageDependency =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of package dependency graphs.                                      *)
(* ------------------------------------------------------------------------- *)

datatype dependency =
    Dependency of
      {includes : PackageNameVersionGraph.graph,
       requires : PackageNameVersionGraph.graph,
       subtheories : PackageNameVersionGraph.graph}

(* ------------------------------------------------------------------------- *)
(* Constructors.                                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val includes = PackageNameVersionGraph.empty
      and requires = PackageNameVersionGraph.empty
      and subtheories = PackageNameVersionGraph.empty
    in
      Dependency
        {includes = includes,
         requires = requires,
         subtheories = subtheories}
    end;

(* ------------------------------------------------------------------------- *)
(* Adding package dependencies.                                              *)
(* ------------------------------------------------------------------------- *)

fun addInfo latest dep info =
    let
      val incs = PackageInfo.includes info
      and reqs = List.mapPartial latest (PackageInfo.requires info)
    in
      if PackageNameVersionSet.null incs andalso List.null reqs then dep
      else
        let
          val namever = PackageInfo.nameVersion info

          fun addEdge (nv,graph) =
              PackageNameVersionGraph.addEdge graph (nv,namever)

          fun addEdges graph nvs =
              PackageNameVersionSet.foldl addEdge graph nvs

          val reqs = PackageNameVersionSet.fromList reqs

          val subs =
              PackageNameVersionSet.filter
                (PackageNameVersion.isStrictPrefixName namever)
                (PackageNameVersionSet.union incs reqs)

          val Dependency {includes,requires,subtheories} = dep

          val includes = addEdges includes incs
          and requires = addEdges requires reqs
          and subtheories = addEdges subtheories subs
        in
          Dependency
            {includes = includes,
             requires = requires,
             subtheories = subtheories}
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Querying package dependencies.                                            *)
(* ------------------------------------------------------------------------- *)

fun includes (Dependency {includes = x, ...}) =
    PackageNameVersionGraph.parents x;

fun includedBy (Dependency {includes = x, ...}) =
    PackageNameVersionGraph.children x;

fun requires (Dependency {requires = x, ...}) =
    PackageNameVersionGraph.parents x;

fun requiredBy (Dependency {requires = x, ...}) =
    PackageNameVersionGraph.children x;

fun subtheories (Dependency {subtheories = x, ...}) =
    PackageNameVersionGraph.parents x;

fun subtheoryOf (Dependency {subtheories = x, ...}) =
    PackageNameVersionGraph.children x;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppGraph s g =
      Print.inconsistentBlock 2
        [Print.ppString s,
         Print.space,
         Print.ppString "=",
         Print.break,
         Print.ppString (PackageNameVersionGraph.toString g)];
in
  fun pp (Dependency {includes,requires,subtheories}) =
      Print.consistentBlock 1
        [Print.ppString "{",
         ppGraph "includes" includes,
         Print.ppString ",",
         Print.break,
         ppGraph "requires" requires,
         Print.ppString ",",
         Print.break,
         ppGraph "subtheories" subtheories,
         Print.ppString "}"];
end;

val toString = Print.toString pp;

end
