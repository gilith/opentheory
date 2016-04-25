(* ========================================================================= *)
(* DIRECTED GRAPHS WITH A FIXED VERTEX TYPE                                  *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

functor VertexGraph (
  structure KM : KeyMap
  and ES : ElementSet
  sharing type KM.key = ES.element
  sharing type KM.map = ES.map
) :> VertexGraph
where type vertex = ES.element
and type set = ES.set
and type ordering = ES.ordering =
struct

(* ------------------------------------------------------------------------- *)
(* A type of directed graph vertices.                                        *)
(* ------------------------------------------------------------------------- *)

type vertex = ES.element;

val compareVertex = ES.compareElement;

val equalVertex = ES.equalElement;

(* ------------------------------------------------------------------------- *)
(* A type of directed graph edges.                                           *)
(* ------------------------------------------------------------------------- *)

type edge = vertex * vertex;

fun compareEdge ((x1,y1),(x2,y2)) =
    case compareVertex (x1,x2) of
      LESS => LESS
    | EQUAL => compareVertex (y1,y2)
    | GREATER => GREATER;

fun equalEdge (x1,y1) (x2,y2) = equalVertex x1 x2 andalso equalVertex y1 y2;

(* ------------------------------------------------------------------------- *)
(* A type of finite sets.                                                    *)
(* ------------------------------------------------------------------------- *)

type set = ES.set;

(* ------------------------------------------------------------------------- *)
(* A type of finite maps.                                                    *)
(* ------------------------------------------------------------------------- *)

type 'a map = 'a KM.map;

(* ------------------------------------------------------------------------- *)
(* A type of directed graphs.                                                *)
(* ------------------------------------------------------------------------- *)

datatype graph =
    Graph of
      {vertices : set,
       edges : int,
       parents : set map,
       children : set map};

(* ------------------------------------------------------------------------- *)
(* Constructors.                                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val vertices = ES.empty
      and edges = 0
      and parents = KM.new ()
      and children = KM.new ()
    in
      Graph
        {vertices = vertices,
         edges = edges,
         parents = parents,
         children = children}
    end;

(* ------------------------------------------------------------------------- *)
(* Destructors.                                                              *)
(* ------------------------------------------------------------------------- *)

fun vertices (Graph {vertices = x, ...}) = x;

fun vertexList graph = ES.toList (vertices graph);

local
  fun addChild p (c,acc) = (p,c) :: acc;

  fun addParent (p,cs,acc) = ES.foldr (addChild p) acc cs;
in
  fun edgeList graph =
      let
        val Graph {parents,...} = graph
      in
         KM.foldr addParent [] parents
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Graph size.                                                               *)
(* ------------------------------------------------------------------------- *)

fun sizeVertices (Graph {vertices = x, ...}) = ES.size x;

fun sizeEdges (Graph {edges = x, ...}) = x;

fun nullVertices graph = sizeVertices graph = 0;

fun nullEdges graph = sizeEdges graph = 0;

fun size graph = {vertices = sizeVertices graph, edges = sizeEdges graph};

(* ------------------------------------------------------------------------- *)
(* Querying.                                                                 *)
(* ------------------------------------------------------------------------- *)

fun parents (Graph {parents = pars, ...}) v =
    case KM.peek pars v of
      SOME ps => ps
    | NONE => ES.empty;

fun children (Graph {children = chil, ...}) v =
    case KM.peek chil v of
      SOME cs => cs
    | NONE => ES.empty;

fun ancestorsSet graph = ES.close (parents graph);

fun ancestors graph v = ancestorsSet graph (parents graph v);

fun descendentsSet graph = ES.close (children graph);

fun descendents graph v = descendentsSet graph (children graph v);

(* ------------------------------------------------------------------------- *)
(* Adding.                                                                   *)
(* ------------------------------------------------------------------------- *)

fun addVertex graph vertex =
    let
      val Graph {vertices,edges,parents,children} = graph
    in
      if ES.member vertex vertices then graph
      else
        let
          val vertices = ES.add vertices vertex
        in
          Graph
            {vertices = vertices,
             edges = edges,
             parents = parents,
             children = children}
        end
    end;

fun addEdge graph (p,c) =
    let
      val graph = addVertex graph p

      val graph = addVertex graph c

      val ps = parents graph c
    in
      if ES.member p ps then graph
      else
        let
          val cs = children graph p

          val Graph {vertices,edges,parents,children} = graph

          val edges = edges + 1
          and parents = KM.insert parents (c, ES.add ps p)
          and children = KM.insert children (p, ES.add cs c)
        in
          Graph
            {vertices = vertices,
             edges = edges,
             parents = parents,
             children = children}
        end
    end;

fun singletonVertex v = addVertex empty v;

fun singletonEdge p_c = addEdge empty p_c;

local
  fun add (v,graph) = addVertex graph v;
in
  fun addVertexList graph vs = List.foldl add graph vs;
end;

fun fromVertexList vs = addVertexList empty vs;

local
  fun add (p_c,graph) = addEdge graph p_c;
in
  fun addEdgeList graph pcs = List.foldl add graph pcs;
end;

fun fromEdgeList pcs = addEdgeList empty pcs;

local
  fun add c (p,graph) = addEdge graph (p,c);
in
  fun addParents graph (ps,c) = ES.foldl (add c) graph ps;
end;

local
  fun add p (c,graph) = addEdge graph (p,c);
in
  fun addChildren graph (p,cs) = ES.foldl (add p) graph cs;
end;

(* ------------------------------------------------------------------------- *)
(* Depth-first search.                                                       *)
(* ------------------------------------------------------------------------- *)

type ordering = ES.ordering;

fun preOrder graph = ES.preOrder (children graph);

fun postOrder graph = ES.postOrder (children graph);

fun preOrdered graph = ES.preOrdered (children graph);

fun postOrdered graph = ES.postOrdered (children graph);

(* ------------------------------------------------------------------------- *)
(* Strongly connected components.                                            *)
(* ------------------------------------------------------------------------- *)

fun preOrderSCC graph = ES.preOrderSCC (children graph);

fun postOrderSCC graph = ES.postOrderSCC (children graph);

fun preOrderedSCC graph = ES.preOrderedSCC (children graph);

fun postOrderedSCC graph = ES.postOrderedSCC (children graph);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun toString graph =
    "{" ^ Int.toString (sizeVertices graph) ^
    "," ^ Int.toString (sizeEdges graph) ^
    "}";

end

structure IntGraph =
VertexGraph (structure KM = IntMap and ES = IntSet);

structure IntPairGraph =
VertexGraph (structure KM = IntPairMap and ES = IntPairSet);

structure StringGraph =
VertexGraph (structure KM = StringMap and ES = StringSet);
