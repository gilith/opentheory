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
       packages : InstanceSet.set StringMap.map};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val instances = InstanceSet.empty

      val packages = StringMap.new ()
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
    Option.getOpt (StringMap.peek packages package, InstanceSet.empty);

fun add graph inst =
    let
(*OpenTheoryDebug
      val imp = Instance.import inst @ Instance.theoryImported inst

      val _ = List.all (fn i => member i graph) imp orelse
              raise Bug "Graph.add: imported instance not in graph"
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
              StringMap.insert packages (p,s)
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

fun match graph req =
    let
      val {package, interpretation = int, import = imp} = req
    in
      raise Bug "Graph.match: not implemented"
    end;

end
