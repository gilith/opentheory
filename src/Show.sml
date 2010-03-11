(* ========================================================================= *)
(* IMPORTING NAMESPACES FOR PRINTING PURPOSES                                *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Show :> Show =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of namespace mapping.                                              *)
(* ------------------------------------------------------------------------- *)

datatype mapping =
    NamespaceMapping of Namespace.namespace * Namespace.namespace;

(* ------------------------------------------------------------------------- *)
(* A type of namespace import map.                                           *)
(* ------------------------------------------------------------------------- *)

datatype show =
    Show of
      {subshows : show StringMap.map,
       rewrite : Namespace.namespace option};

local
  fun dumpRewrite acc ns rewrite =
      case rewrite of
        NONE => acc
      | SOME rw => NamespaceMapping (Namespace.fromList (rev ns), rw) :: acc;

  fun dumpShow acc ns show =
      let
        val Show {subshows,rewrite} = show

        val acc = StringMap.foldr (dumpSubshow ns) acc subshows

        val acc = dumpRewrite acc ns rewrite
      in
        acc
      end

  and dumpSubshow ns (n,show,acc) = dumpShow acc (n :: ns) show;
in
  val toList = dumpShow [] [];
end;

(* ------------------------------------------------------------------------- *)
(* Subshows.                                                                 *)
(* ------------------------------------------------------------------------- *)

val emptySubshows : show StringMap.map = StringMap.new ();

(* ------------------------------------------------------------------------- *)
(* The empty map.                                                            *)
(* ------------------------------------------------------------------------- *)

val natural =
    let
      val subshows = emptySubshows
      and rewrite = NONE
    in
      Show
        {subshows = subshows,
         rewrite = rewrite}
    end;

(* ------------------------------------------------------------------------- *)
(* Adding mappings.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun addPrim show ns rw =
      case ns of
        [] =>
        let
          val subshows = emptySubshows
          and rewrite = SOME rw
        in
          Show
            {subshows = subshows,
             rewrite = rewrite}
        end
      | n :: ns =>
        let
          val Show {subshows,rewrite} = show

          val subshow =
              case StringMap.peek subshows n of
                SOME s => s
              | NONE => natural

          val subshow = addPrim subshow ns rw

          val subshows = StringMap.insert subshows (n,subshow)
        in
          Show
            {subshows = subshows,
             rewrite = rewrite}
        end;
in
  fun addNamespace show (n1,n2) =
      addPrim show (Namespace.toList n1) n2;
end;

fun add show m =
    case m of
      NamespaceMapping n1_n2 => addNamespace show n1_n2;

local
  fun add1 (m,show) = add show m;
in
  val addList = List.foldl add1;
end;

val fromList = addList natural;

(* ------------------------------------------------------------------------- *)
(* Mapping namespaces.                                                       *)
(* ------------------------------------------------------------------------- *)

local
  fun peekPrim show ns =
      let
        val Show {subshows,rewrite} = show
      in
        case ns of
          [] => rewrite
        | n :: ns =>
          case peekSubPrim subshows n ns of
            SOME rw => SOME rw
          | NONE =>
            case rewrite of
              SOME rw => SOME (Namespace.append rw (Namespace.fromList ns))
            | NONE => NONE
      end

  and peekSubPrim subshows n ns =
      case StringMap.peek subshows n of
        SOME show => peekPrim show ns
      | NONE => NONE;
in
  fun peekNamespace show n =
      peekPrim show (Namespace.toList n);
end;

fun showNamespace show n =
    case peekNamespace show n of
      SOME n => n
    | NONE => n;

fun showName show n =
    let
      val (ns,s) = Name.dest n
    in
      case peekNamespace show ns of
        SOME ns => Name.mk (ns,s)
      | NONE => n
    end;

end
