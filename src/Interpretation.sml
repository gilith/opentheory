(* ========================================================================= *)
(* INTERPRETING OPENTHEORY NAMES                                             *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Interpretation :> Interpretation =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val rewriteString = "->";

val terminatorString = ";";

(* ------------------------------------------------------------------------- *)
(* A type of rewrite rules for names.                                        *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    NamespaceRewrite of Namespace.namespace * Namespace.namespace
  | TypeRewrite of Name.name * Name.name
  | ConstRewrite of Name.name * Name.name
  | RulespaceRewrite of Namespace.namespace * Namespace.namespace
  | RuleRewrite of Name.name * Name.name

fun interpretNamespaceRewrite r n =
    case r of
      NamespaceRewrite x_y => Namespace.rewrite x_y n
    | _ => n;

fun interpretTypeRewrite r n =
    case r of
      NamespaceRewrite x_y => Name.rewrite x_y n
    | TypeRewrite x_y => Name.replace x_y n
    | _ => n;

fun interpretConstRewrite r n =
    case r of
      NamespaceRewrite x_y => Name.rewrite x_y n
    | ConstRewrite x_y => Name.replace x_y n
    | _ => n;

fun interpretRulespaceRewrite r n =
    case r of
      RulespaceRewrite x_y => Namespace.rewrite x_y n
    | _ => n;

fun interpretRuleRewrite r n =
    case r of
      RulespaceRewrite x_y => Name.rewrite x_y n
    | RuleRewrite x_y => Name.replace x_y n
    | _ => n;

local
  fun ppX2 prefix ppX (x1,x2) =
      Print.blockProgram Print.Inconsistent 2
        [Print.addString prefix,
         Print.addBreak 1,
         ppX x1,
         Print.addString " ",
         Print.addString rewriteString,
         Print.addBreak 1,
         ppX x2];

  fun ppNamespace2 prefix = ppX2 prefix Namespace.pp;

  fun ppName2 prefix = ppX2 prefix Name.pp;
in
  fun ppRewrite r =
      case r of
        NamespaceRewrite x_y => ppNamespace2 "namespace" x_y
      | TypeRewrite x_y => ppName2 "type" x_y
      | ConstRewrite x_y => ppName2 "const" x_y
      | RulespaceRewrite x_y => ppNamespace2 "rulespace" x_y
      | RuleRewrite x_y => ppName2 "rule" x_y;
end;

fun ppTerminatedRewrite r =
    Print.program
      [ppRewrite r,
       Print.addString terminatorString];

fun ppRewriteList1 r rs =
    case rs of
      [] => ppTerminatedRewrite r
    | r' :: rs =>
      Print.program
        [ppTerminatedRewrite r,
         Print.addNewline,
         ppRewriteList1 r' rs];

fun ppRewriteList l =
    case l of
      [] => Print.skip
    | r :: rs => ppRewriteList1 r rs;

val toStringRewrite = Print.toString ppRewrite;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val rewriteChars = explode rewriteString;

  val space = many (some Char.isSpace) >> K ();

  fun xyParser prefix xParser =
      (exactList (explode prefix) ++ space ++
       xParser ++ space ++ exactList rewriteChars ++ space ++
       xParser ++ space ++ exact #";") >>
      (fn (_,(_,(x,(_,(_,(_,(y,_))))))) => (x,y));
in
  val parserRewrite =
      xyParser "namespace" Namespace.quotedParser >> NamespaceRewrite ||
      xyParser "type" Name.quotedParser >> TypeRewrite ||
      xyParser "const" Name.quotedParser >> ConstRewrite ||
      xyParser "rulespace" Namespace.quotedParser >> RulespaceRewrite ||
      xyParser "rule" Name.quotedParser >> RuleRewrite;

  val spacedParserRewrite =
      (space ++ parserRewrite ++ space) >> (fn ((),(t,())) => t);
end;

(* ------------------------------------------------------------------------- *)
(* A type of interpretations (bad pun on interpreting art (article files)).  *)
(* ------------------------------------------------------------------------- *)

datatype interpretation = Interpretation of rewrite list;

val natural = Interpretation [];

fun singleton rw = Interpretation [rw];

fun append (Interpretation l1) (Interpretation l2) = Interpretation (l1 @ l2);

fun concat1 int ints =
    case ints of
      [] => int
    | int' :: ints => append int (concat1 int' ints);

fun concat ints =
    case ints of
      [] => natural
    | int :: ints => concat1 int ints;

(* ------------------------------------------------------------------------- *)
(* Translating OpenTheory names.                                             *)
(* ------------------------------------------------------------------------- *)

local
  fun interpret rewrX (Interpretation l) x = foldl (fn (rw,x) => rewrX rw x) x l;
in
  val interpretNamespace = interpret interpretNamespaceRewrite;

  val interpretType = interpret interpretTypeRewrite;

  val interpretConst = interpret interpretConstRewrite;

  val interpretRulespace = interpret interpretRulespaceRewrite;

  val interpretRule = interpret interpretRuleRewrite;
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp (Interpretation l) =
    Print.block Print.Consistent 0 (ppRewriteList l);

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;
in
  val parser = many spacedParserRewrite >> Interpretation;

  val parser' =
      atLeastOne spacedParserRewrite >> (fn rs => [Interpretation rs]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {filename,interpretation} =
    Stream.toTextFile {filename = filename} (Print.toStream pp interpretation);

local
  (* Comment lines *)

  fun isComment l =
      case List.find (not o Char.isSpace) l of
        NONE => true
      | SOME #"#" => true
      | _ => false;
in
  fun fromTextFile {filename} =
      let
        (* Estimating parse error line numbers *)

        val lines = Stream.fromTextFile {filename = filename}

        val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
      in
        (let
           (* The character stream *)

           val chars = Stream.filter (not o isComment) chars

           val chars = Parse.everything Parse.any chars

           (* The interpretation stream *)

           val ints = Parse.everything parser' chars
         in
           concat (Stream.toList ints)
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in interpretation file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

end
