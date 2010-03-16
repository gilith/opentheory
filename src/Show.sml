(* ========================================================================= *)
(* IMPORTING NAMESPACES FOR PRINTING PURPOSES                                *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Show :> Show =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val asKeywordString = "as"
and showTagName = "show";

(* ------------------------------------------------------------------------- *)
(* A type of mappings.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype mapping =
    NamespaceMapping of Namespace.namespace * Namespace.namespace;

fun toStringMapping m =
    case m of
      NamespaceMapping (n1,n2) =>
      Namespace.quotedToString n1 ^
      (if Namespace.isGlobal n2 then ""
       else " " ^ asKeywordString ^ " " ^ Namespace.quotedToString n2);

fun toTagMapping m =
    let
      val name = showTagName

      val value = toStringMapping m

      val tag' = Tag.Tag' {name = name, value = value}
    in
      Tag.mk tag'
    end;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val asKeywordParser = exactString asKeywordString;

  val asKeywordSpaceParser =
      (asKeywordParser ++ manySpace) >> (fn ((),()) => ());

  val namespaceSpaceParser =
      (Namespace.quotedParser ++ manySpace) >> (fn (n,()) => n);

  val asNamespaceParser =
      (asKeywordSpaceParser ++ namespaceSpaceParser) >> (fn ((),n) => n);

  val possibleAsNamespaceParser =
      optional asNamespaceParser >>
      (fn n => Option.getOpt (n,Namespace.global));

  val namespaceMappingParser =
      (namespaceSpaceParser ++ possibleAsNamespaceParser) >>
      NamespaceMapping;

  val mappingParser =
      namespaceMappingParser;
in
  val parserMapping = (manySpace ++ mappingParser) >> (fn ((),m) => m);
end;

fun fromStringMapping s =
    Parse.fromString parserMapping s
    handle Parse.NoParse => raise Error "Show.fromStringMapping";

(* ------------------------------------------------------------------------- *)
(* A type of mapping collections.                                            *)
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
(* Subshows operations.                                                      *)
(* ------------------------------------------------------------------------- *)

val emptySubshows : show StringMap.map = StringMap.new ();

(* ------------------------------------------------------------------------- *)
(* The empty mapping.                                                        *)
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
(* Mapping names.                                                            *)
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

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty-printing.                                              *)
(* ------------------------------------------------------------------------- *)

fun toTags show = map toTagMapping (toList show);

local
  fun destTag tag =
      let
        val Tag.Tag' {name,value} = Tag.dest tag
      in
        if name = showTagName then SOME value else NONE
      end;
in
  fun fromTags tags =
      let
        val vs = List.mapPartial destTag tags

        val ms = map fromStringMapping vs
      in
        fromList ms
      end;
end;

(* ------------------------------------------------------------------------- *)
(* The default mapping.                                                      *)
(* ------------------------------------------------------------------------- *)

val default =
    let
      fun openNamespace ns =
          NamespaceMapping (Namespace.fromList ns, Namespace.global)
    in
      (fromList o map openNamespace)
      [["Data","Bool"]]
    end;

end
