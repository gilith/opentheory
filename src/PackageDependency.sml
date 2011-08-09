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

datatype dependency = Dependency of PackageNameVersionGraph.graph;

(* ------------------------------------------------------------------------- *)
(* Constructors.                                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val graph = PackageNameVersionGraph.empty
    in
      Dependency graph
    end;

(* ------------------------------------------------------------------------- *)
(* Adding package dependencies.                                              *)
(* ------------------------------------------------------------------------- *)

fun addInfo dep info =
    let
      val pars = PackageInfo.packages info
    in
      if PackageNameVersionSet.null pars then dep
      else
        let
          val namever = PackageInfo.nameVersion info

          fun add (p,graph) = PackageNameVersionGraph.addEdge graph (p,namever)

          val Dependency graph = dep

          val graph = PackageNameVersionSet.foldl add graph pars
        in
          Dependency graph
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Dependencies in the installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

fun parents (Dependency graph) namever =
    PackageNameVersionGraph.parents graph namever;

fun children (Dependency graph) namever =
    PackageNameVersionGraph.children graph namever;

fun ancestors (Dependency graph) namever =
    PackageNameVersionGraph.ancestors graph namever;

fun descendents (Dependency graph) namever =
    PackageNameVersionGraph.descendents graph namever;

(* Set versions *)

fun ancestorsSet (Dependency graph) set =
    PackageNameVersionGraph.ancestorsSet graph set;

fun descendentsSet (Dependency graph) set =
    PackageNameVersionGraph.descendentsSet graph set;

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

fun installOrder dep = PackageNameVersionSet.postOrder (parents dep);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun toString (Dependency graph) = PackageNameVersionGraph.toString graph;

end
