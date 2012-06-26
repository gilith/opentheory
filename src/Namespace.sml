(* ========================================================================= *)
(* OPENTHEORY NAMESPACES                                                     *)
(* Copyright (c) 2004 Joe Hurd, distributed under the MIT license            *)
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
(* Namespace components.                                                     *)
(* ------------------------------------------------------------------------- *)

type component = string;

(* A total ordering *)

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

(* Standard syntax *)

val iffSyntaxComponent = "<=>"
and lambdaSyntaxComponent = "\\";

(* Standard latex symbols *)

val backslashLatexComponent = "\\backslash"
and botLatexComponent = "\\bot"
and capLatexComponent = "\\cap"
and circLatexComponent = "\\circ"
and crossLatexComponent = "\\cross"
and cupLatexComponent = "\\cup"
and emptysetLatexComponent = "\\emptyset"
and inLatexComponent = "\\in"
and lambdaLatexComponent = "\\lambda"
and lnotLatexComponent = "\\lnot"
and subsetLatexComponent = "\\subset"
and subseteqLatexComponent = "\\subseteq"
and topLatexComponent = "\\top";

(* Standard namespaces *)

val boolNamespaceComponent = "Bool"
and dataNamespaceComponent = "Data"
and functionNamespaceComponent = "Function"
and listNamespaceComponent = "List"
and naturalNamespaceComponent = "Natural"
and numberNamespaceComponent = "Number"
and optionNamespaceComponent = "Option"
and pairNamespaceComponent = "Pair"
and probabilityNamespaceComponent = "Probability"
and randomNamespaceComponent = "Random"
and setNamespaceComponent = "Set";

(* Standard type operators *)

val boolTypeOpComponent = "bool"
and funTypeOpComponent = "->"
and indTypeOpComponent = "ind"
and listTypeOpComponent = "list"
and naturalTypeOpComponent = "natural"
and optionTypeOpComponent = "option"
and pairTypeOpComponent = "*"
and randomTypeOpComponent = "random"
and sumTypeOpComponent = "+";

(* Standard constants *)

val addConstComponent = "+"
and bitConstComponent = "bit"
and bit0ConstComponent = "bit0"
and bit1ConstComponent = "bit1"
and composeConstComponent = "o"
and caseConstComponent = "case"
and condConstComponent = "cond"
and conjConstComponent = "/\\"
and consConstComponent = "::"
and differenceConstComponent = "difference"
and disjConstComponent = "\\/"
and emptyConstComponent = "{}"
and eqConstComponent = "="
and existsConstComponent = "?"
and existsUniqueConstComponent = "?!"
and falseConstComponent = "F"
and forallConstComponent = "!"
and fromNaturalConstComponent = "fromNatural"
and fromPredicateConstComponent = "fromPredicate"
and geConstComponent = ">="
and gtConstComponent = ">"
and idConstComponent = "id"
and impConstComponent = "==>"
and intersectConstComponent = "intersect"
and leConstComponent = "<="
and lengthConstComponent = "length"
and ltConstComponent = "<"
and memberConstComponent = "member"
and minimalConstComponent = "minimal"
and negConstComponent = "~"
and nilConstComponent = "[]"
and noneConstComponent = "none"
and pairConstComponent = ","
and properSubsetConstComponent = "properSubset"
and selectConstComponent = "select"
and someConstComponent = "some"
and splitConstComponent = "split"
and subsetConstComponent = "subset"
and subtractConstComponent = "-"
and sucConstComponent = "suc"
and trueConstComponent = "T"
and unionConstComponent = "union"
and zeroConstComponent = "zero";

(* Parsing and pretty printing *)

local
  val escapeChars = [quoteChar,separatorChar];
in
  val ppComponent = Print.ppEscapeString {escape = escapeChars};

  val parserComponent = Parse.escapeString {escape = escapeChars};
end;

local
  val componentMap =
      StringMap.fromList
        [(* Text symbols *)
         ("<=", [Html.Entity "le"]),
         ("<", [Html.Entity "lt"]),
         (">=", [Html.Entity "ge"]),
         (">", [Html.Entity "gt"]),
         ("^", [Html.Entity "uarr"]),
         ("<-", [Html.Entity "larr"]),
         ("->", [Html.Entity "rarr"]),
         ("<==", [Html.Entity "lArr"]),
         ("==>", [Html.Entity "rArr"]),
         ("<=>", [Html.Entity "hArr"]),
         ("/\\", [Html.Entity "and"]),
         ("\\/", [Html.Entity "or"]),
         (* Latex symbols *)
         (backslashLatexComponent, [Html.Text "\\"]),
         (botLatexComponent, [Html.Entity "perp"]),
         (capLatexComponent, [Html.Entity "cap"]),
         (circLatexComponent, [Html.Entity "#8728"]),
         (crossLatexComponent, [Html.Entity "times"]),
         (cupLatexComponent, [Html.Entity "cup"]),
         (emptysetLatexComponent, [Html.Entity "empty"]),
         (inLatexComponent, [Html.Entity "isin"]),
         (lambdaLatexComponent, [Html.Entity "lambda"]),
         (lnotLatexComponent, [Html.Entity "not"]),
         (subsetLatexComponent, [Html.Entity "sub"]),
         (subseteqLatexComponent, [Html.Entity "sube"]),
         (topLatexComponent, [Html.Entity "#8868"]),
         (* Quantifier symbols *)
         (existsConstComponent, [Html.Entity "exist"]),
         (existsUniqueConstComponent, [Html.Entity "exist", Html.Text "!"]),
         (forallConstComponent, [Html.Entity "forall"])];
in
  fun toHtmlComponent c =
      case StringMap.peek componentMap c of
        SOME h => h
      | NONE => [Html.Text c];
end;

(* ------------------------------------------------------------------------- *)
(* A type of namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype namespace = Namespace of component list;

fun append (Namespace n1) (Namespace n2) = Namespace (n1 @ n2);

fun toList (Namespace n) = n;

fun fromList n = Namespace n;

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

(* Global *)

local
  fun mkGlobal c = mkNested (global,c);
in
  val data = mkGlobal dataNamespaceComponent
  and function = mkGlobal functionNamespaceComponent
  and number = mkGlobal numberNamespaceComponent
  and probability = mkGlobal probabilityNamespaceComponent
  and set = mkGlobal setNamespaceComponent;
end;

(* Data *)

local
  fun mkData c = mkNested (data,c);
in
  val bool = mkData boolNamespaceComponent
  and list = mkData listNamespaceComponent
  and option = mkData optionNamespaceComponent
  and pair = mkData pairNamespaceComponent;
end;

(* Number *)

local
  fun mkNumber c = mkNested (number,c);
in
  val natural = mkNumber naturalNamespaceComponent;
end;

(* Probability *)

local
  fun mkProbability c = mkNested (probability,c);
in
  val random = mkProbability randomNamespaceComponent;
end;

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

local
  val ppQuote = Print.ppChar quoteChar;

  val ppSeparator = Print.ppChar separatorChar;

  fun ppComponents ppC cs =
      case cs of
        [] => Print.skip
      | c :: cs =>
        Print.program (ppC c :: List.map (Print.sequence ppSeparator o ppC) cs);
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

  val separatorComponentParser = (separatorParser ++ parserComponent) >> snd;

  val parser =
      (parserComponent ++ many separatorComponentParser) >>
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
