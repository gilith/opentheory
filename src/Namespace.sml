(* ========================================================================= *)
(* OPENTHEORY NAMESPACES                                                     *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Namespace :> Namespace =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of namespaces.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype namespace = Namespace of string list;

(* ------------------------------------------------------------------------- *)
(* The top level namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

val global = Namespace [];

fun isGlobal (Namespace n) = null n;

(* ------------------------------------------------------------------------- *)
(* Nested namespaces (i.e., everything except the top level).                *)
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

local
  fun escapeChar c =
      case c of
        #"\\" => "\\\\"
      | #"\"" => "\\\""
      | #"." => "\\."
      | #"\n" => "\\n"
      | #"\t" => "\\t"
      | _ => str c;

  val escapeString = String.translate escapeChar;

  fun dotify ns = join "." ns;
in
  fun toString (n as Namespace ns) =
      if isGlobal n then "<global>" else dotify ns;

  fun quotedToString (n as Namespace ns) =
      let
        val s = if isGlobal n then "" else dotify (map escapeString ns)
      in
        "\"" ^ s ^ "\""
      end;
end;

val pp = Print.ppMap toString Print.ppString;

val ppQuoted = Print.ppMap quotedToString Print.ppString;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val isSpecialChar = C mem (explode "\\\".");

  fun isEscapedChar c =
      case c of
        #"\n" => true
      | #"\t" => true
      | _ => isSpecialChar c;

  val escapeParser =
      some isSpecialChar ||
      (exactChar #"n" >> K #"\n") ||
      (exactChar #"t" >> K #"\t");

  val componentCharParser =
      ((exactChar #"\\" ++ escapeParser) >> snd) ||
      some (not o isEscapedChar);

  val componentParser = many componentCharParser >> implode;

  val dotComponentParser = (exactChar #"." ++ componentParser) >> snd;

  val parser =
      (componentParser ++ many dotComponentParser) >>
      (fn ("",[]) => global
        | (n,ns) => Namespace (n :: ns));
in
  val quotedParser =
      (exactChar #"\"" ++ parser ++ exactChar #"\"") >> (fn (_,(x,_)) => x);
end;

end
