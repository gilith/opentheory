(* ========================================================================= *)
(* DIRECTED GRAPHS WITH A FIXED VERTEX TYPE                                  *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature VertexGraph =
sig

(* ------------------------------------------------------------------------- *)
(* A type of directed graph vertices.                                        *)
(* ------------------------------------------------------------------------- *)

type vertex

val compareVertex : vertex * vertex -> order

val equalVertex : vertex -> vertex -> bool

(* ------------------------------------------------------------------------- *)
(* A type of directed graph edges.                                           *)
(* ------------------------------------------------------------------------- *)

type edge = vertex * vertex

val compareEdge : edge * edge -> order

val equalEdge : edge -> edge -> bool

(* ------------------------------------------------------------------------- *)
(* A type of finite sets.                                                    *)
(* ------------------------------------------------------------------------- *)

type set

(* ------------------------------------------------------------------------- *)
(* A type of directed graphs.                                                *)
(* ------------------------------------------------------------------------- *)

type graph

(* ------------------------------------------------------------------------- *)
(* Constructors.                                                             *)
(* ------------------------------------------------------------------------- *)

val empty : graph

val singletonVertex : vertex -> graph

val singletonEdge : edge -> graph

val fromVertexList : vertex list -> graph

val fromEdgeList : edge list -> graph

(* ------------------------------------------------------------------------- *)
(* Destructors.                                                              *)
(* ------------------------------------------------------------------------- *)

val vertices : graph -> set

val vertexList : graph -> vertex list

val edgeList : graph -> edge list

(* ------------------------------------------------------------------------- *)
(* Graph size.                                                               *)
(* ------------------------------------------------------------------------- *)

val nullVertices : graph -> bool

val nullEdges : graph -> bool

val size : graph -> {vertices : int, edges : int}

val sizeVertices : graph -> int

val sizeEdges : graph -> int

(* ------------------------------------------------------------------------- *)
(* Querying.                                                                 *)
(* ------------------------------------------------------------------------- *)

val parents : graph -> vertex -> set

val children : graph -> vertex -> set

val ancestors : graph -> vertex -> set

val descendents : graph -> vertex -> set

(* Set versions *)

val ancestorsSet : graph -> set -> set

val descendentsSet : graph -> set -> set

(* ------------------------------------------------------------------------- *)
(* Adding.                                                                   *)
(* ------------------------------------------------------------------------- *)

val addVertex : graph -> vertex -> graph

val addVertexList : graph -> vertex list -> graph

val addEdge : graph -> edge -> graph

val addEdgeList : graph -> edge list -> graph

val addParents : graph -> set * vertex -> graph

val addChildren : graph -> vertex * set -> graph

(* ------------------------------------------------------------------------- *)
(* Depth-first search.                                                       *)
(* ------------------------------------------------------------------------- *)

type ordering

val preOrder : graph -> set -> ordering

val postOrder : graph -> set -> ordering

val preOrdered : graph -> vertex list -> bool

val postOrdered : graph -> vertex list -> bool

(* ------------------------------------------------------------------------- *)
(* Strongly connected components.                                            *)
(* ------------------------------------------------------------------------- *)

val preOrderSCC : graph -> set -> set list

val postOrderSCC : graph -> set -> set list

val preOrderedSCC : graph -> set list -> bool

val postOrderedSCC : graph -> set list -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val toString : graph -> string

end
