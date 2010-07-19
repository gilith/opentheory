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

val quoteChar = #"\""
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

fun isGlobal (Namespace n) = null n;

(* ------------------------------------------------------------------------- *)
(* Nested namespaces (i.e., everything except the top-level).                *)
(* ------------------------------------------------------------------------- *)

fun mkNested (Namespace ns, n) = Namespace (ns @ [n]);

fun destNested (Namespace ns) =
    case rev ns of
      [] => raise Error "Namespace.destNested"
    | n :: ns => (Namespace (rev ns), n);

val isNested = can destNested;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

fun compare (Namespace n1, Namespace n2) = lexCompare String.compare (n1,n2);

(* ------------------------------------------------------------------------- *)
(* Rewriting namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

local
  fun stripPrefix [] ys = SOME ys
    | stripPrefix (x :: xs) ys =
      case ys of
        [] => NONE
      | y :: ys => if x = y then stripPrefix xs ys else NONE;
in
  fun rewrite (Namespace xs, Namespace ys) (n as Namespace ns) =
      case stripPrefix xs ns of
        NONE => n
      | SOME ns => Namespace (ys @ ns);
end;

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
        Print.program (ppC c :: map (Print.sequence ppSeparator o ppC) cs);

  val ppComponent = Print.ppEscapeString {escape = escapeChars};
in
  fun pp (n as Namespace ns) =
      if isGlobal n then Print.ppString "<global>"
      else ppComponents Print.ppString ns;

  fun ppQuoted (Namespace ns) =
      Print.program [ppQuote, ppComponents ppComponent ns, ppQuote];
end;

val toString = Print.toString pp;

val quotedToString = Print.toString ppQuoted;

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
