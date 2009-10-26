(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY GRAPHS                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Graph :> Graph =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory instances.                                               *)
(* ------------------------------------------------------------------------- *)

datatype graph =
    Graph of
      {instances : InstanceSet.set,
       packages : InstanceSet.set PackageNameMap.map};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val instances = InstanceSet.empty

      val packages = PackageNameMap.new ()
    in
      Graph
        {instances = instances,
         packages = packages}
    end;

fun instances (Graph {instances = x, ...}) = x;

fun member inst graph = InstanceSet.member inst (instances graph);

(* ------------------------------------------------------------------------- *)
(* Adding instances.                                                         *)
(* ------------------------------------------------------------------------- *)

fun lookupPackages packages package =
    Option.getOpt (PackageNameMap.peek packages package, InstanceSet.empty);

fun add graph inst =
    let
(*OpenTheoryDebug
      val insts = Instance.requires inst @ Instance.theoryImports inst

      val _ = List.all (fn i => member i graph) insts orelse
              raise Bug "Graph.add: parent instance not in graph"
*)

      val Graph {instances,packages} = graph

      val instances = InstanceSet.add instances inst

      val packages =
          case Instance.package inst of
            NONE => packages
          | SOME p =>
            let
              val s = lookupPackages packages p

              val s = InstanceSet.add s inst
            in
              PackageNameMap.insert packages (p,s)
            end
    in
      Graph
        {instances = instances,
         packages = packages}
    end;

(* ------------------------------------------------------------------------- *)
(* Looking up theory instances by package name.                              *)
(* ------------------------------------------------------------------------- *)

fun lookup (Graph {packages,...}) package =
    lookupPackages packages package;

(* ------------------------------------------------------------------------- *)
(* Finding matching theory instances.                                        *)
(* ------------------------------------------------------------------------- *)

fun match graph spec =
    let
      val {requires = req, interpretation = int, package = pkg} = spec
    in
      raise Bug "Graph.match: not implemented"
    end;

end
