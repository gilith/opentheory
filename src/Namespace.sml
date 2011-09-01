(* ========================================================================= *)
(* OPENTHEORY NAMESPACES                                                     *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Namespace :> Namespace =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val globalString = "<global>"
and quoteChar = #"\""
and separatorChar = #".";

(* ------------------------------------------------------------------------- *)
(* A type of namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype namespace = Namespace of string list;

fun append (Namespace n1) (Namespace n2) = Namespace (n1 @ n2);

fun toList (Namespace n) = n;

fun fromList n = Namespace n;

fun fromString s = fromList [s];

(* ------------------------------------------------------------------------- *)
(* The top-level namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

val global = Namespace [];

fun isGlobal (Namespace n) = List.null n;

(* ------------------------------------------------------------------------- *)
(* Nested namespaces (i.e., everything except the top-level).                *)
(* ------------------------------------------------------------------------- *)

fun mkNested (Namespace ns, n) = Namespace (ns @ [n]);

fun destNested (Namespace ns) =
    case List.rev ns of
      [] => raise Error "Namespace.destNested"
    | n :: ns => (Namespace (List.rev ns), n);

val isNested = can destNested;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

fun compareComponent (s1,s2) =
    case (String.size s1 = 0, String.size s2 = 0) of
      (true,true) => EQUAL
    | (true,false) => LESS
    | (false,true) => GREATER
    | (false,false) =>
      let
        val c1 = String.sub (s1,0)
        and c2 = String.sub (s2,0)
      in
        case (Char.isAlphaNum c1, Char.isAlphaNum c2) of
          (true,false) => GREATER
        | (false,true) => LESS
        | (false,false) => String.compare (s1,s2)
        | (true,true) =>
          case (Char.isUpper c1, Char.isUpper c2) of
            (true,true) => String.compare (s1,s2)
          | (true,false) => GREATER
          | (false,true) => LESS
          | (false,false) => String.compare (s1,s2)
      end;

fun compare (Namespace n1, Namespace n2) =
    lexCompare compareComponent (n1,n2);

(* ------------------------------------------------------------------------- *)
(* Rewriting namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

local
  fun stripPrefix xs ys =
      case xs of
        [] => SOME ys
      | x :: xs =>
        case ys of
          [] => NONE
        | y :: ys => if x = y then stripPrefix xs ys else NONE;
in
  fun rewrite (Namespace xs, Namespace ys) (Namespace ns) =
      case stripPrefix xs ns of
        NONE => NONE
      | SOME ns => SOME (Namespace (ys @ ns));
end;

(* ------------------------------------------------------------------------- *)
(* The standard namespace.                                                   *)
(* ------------------------------------------------------------------------- *)

val bool = fromList ["Data","Bool"]
and list = fromList ["Data","List"]
and option = fromList ["Data","Option"]
and pair = fromList ["Data","Pair"]
and natural = fromList ["Number","Natural"]
and set = fromList ["Set"];

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

val escapeChars = [quoteChar,separatorChar];

local
  val ppQuote = Print.ppChar quoteChar;

  val ppSeparator = Print.ppChar separatorChar;

  fun ppComponents ppC cs =
      case cs of
        [] => Print.skip
      | c :: cs =>
        Print.program (ppC c :: List.map (Print.sequence ppSeparator o ppC) cs);

  val ppComponent = Print.ppEscapeString {escape = escapeChars};
in
  fun pp (n as Namespace ns) =
      if isGlobal n then Print.ppString globalString
      else ppComponents Print.ppString ns;

  fun ppQuoted (Namespace ns) =
      Print.program [ppQuote, ppComponents ppComponent ns, ppQuote];
end;

val toString = Print.toString pp;

val quotedToString = Print.toString ppQuoted;

local
  fun toHtmlComponent c =
      case c of
        "\\" => [Html.Entity "lambda"]
      | "\\lnot" => [Html.Entity "not"]
      | "<=" => [Html.Entity "le"]
      | "<" => [Html.Entity "lt"]
      | ">=" => [Html.Entity "ge"]
      | ">" => [Html.Entity "gt"]
      | "/\\" => [Html.Entity "and"]
      | "\\/" => [Html.Entity "or"]
      | "==>" => [Html.Entity "rArr"]
      | "<=>" => [Html.Entity "hArr"]
      | "!" => [Html.Entity "forall"]
      | "?" => [Html.Entity "exist"]
      | "?!" => [Html.Entity "exist", Html.Text "!"]
      | "->" => [Html.Entity "rarr"]
      | "{}" => [Html.Entity "empty"]
      | "intersect" => [Html.Entity "cap"]
      | "in" => [Html.Entity "isin"]
      | "properSubset" => [Html.Entity "sub"]
      | "subset" => [Html.Entity "sube"]
      | "union" => [Html.Entity "cup"]
      | _ => [Html.Text c];

  val globalHtml = [Html.Text globalString];

  val separatorHtml = [Html.Text (str separatorChar)];

  fun add (c,acc) =
      let
        val acc = separatorHtml @ acc

        val acc = toHtmlComponent c @ acc
      in
        acc
      end;
in
  fun toHtml (Namespace ns) =
      case List.rev ns of
        [] => globalHtml
      | c :: cs => List.foldl add (toHtmlComponent c) cs;
end;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val quoteParser = exactChar quoteChar;

  val separatorParser = exactChar separatorChar;

  val componentParser = escapeString {escape = escapeChars};

  val separatorComponentParser = (separatorParser ++ componentParser) >> snd;

  val parser =
      (componentParser ++ many separatorComponentParser) >>
      (fn ("",[]) => global
        | (n,ns) => Namespace (n :: ns));
in
  val quotedParser =
      (quoteParser ++ parser ++ quoteParser) >> (fn (_,(x,_)) => x);
end;

end

structure NamespaceOrdered =
struct type t = Namespace.namespace val compare = Namespace.compare end

structure NamespaceMap = KeyMap (NamespaceOrdered)

structure NamespaceSet =
struct

local
  structure S = ElementSet (NamespaceMap);
in
  open S;
end;

val pp =
    Print.ppMap toList
      (Print.ppBracket "{" "}" (Print.ppOpList "," Namespace.pp));

end
