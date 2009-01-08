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

(* ------------------------------------------------------------------------- *)
(* A type of rewrite rules for names.                                        *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    RewriteNamespace of Namespace.namespace * Namespace.namespace
  | RewriteType of Name.name * Name.name
  | RewriteConst of Name.name * Name.name
  | RewriteRulespace of Namespace.namespace * Namespace.namespace
  | RewriteRule of Name.name * Name.name;

fun rewriteNamespace r n =
    case r of
      RewriteNamespace x_y => Namespace.rewrite x_y n
    | _ => n;

fun rewriteType r n =
    case r of
      RewriteNamespace x_y => Name.rewrite x_y n
    | RewriteType x_y => Name.replace x_y n
    | _ => n;

fun rewriteConst r n =
    case r of
      RewriteNamespace x_y => Name.rewrite x_y n
    | RewriteConst x_y => Name.replace x_y n
    | _ => n;

fun rewriteRulespace r n =
    case r of
      RewriteRulespace x_y => Namespace.rewrite x_y n
    | _ => n;

fun rewriteRule r n =
    case r of
      RewriteRulespace x_y => Name.rewrite x_y n
    | RewriteRule x_y => Name.replace x_y n
    | _ => n;

local
  fun xyToString prefix xToString (x,y) =
      prefix ^ " " ^ xToString x ^ " " ^
      rewriteString ^ " " ^ xToString y ^ ";";

  fun xyNameToString prefix x_y =
      xyToString prefix Name.toString x_y;
in
  fun rewriteToString r =
      case r of
        RewriteNamespace x_y => xyToString "namespace" Namespace.toString x_y
      | RewriteType x_y => xyNameToString "type" x_y
      | RewriteConst x_y => xyNameToString "const" x_y
      | RewriteRulespace x_y => xyToString "rulespace" Namespace.toString x_y
      | RewriteRule x_y => xyNameToString "rule" x_y;
end;

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
  val rewriteParser =
      xyParser "namespace" Namespace.quotedParser >> RewriteNamespace ||
      xyParser "type" Name.quotedParser >> RewriteType ||
      xyParser "const" Name.quotedParser >> RewriteConst ||
      xyParser "rulespace" Namespace.quotedParser >> RewriteRulespace ||
      xyParser "rule" Name.quotedParser >> RewriteRule;

  val spacedRewriteParser =
      (space ++ rewriteParser ++ space) >> (fn ((),(t,())) => [t]);
end;

(* ------------------------------------------------------------------------- *)
(* A type of interpretations (bad pun on interpreting art (article files)).  *)
(* ------------------------------------------------------------------------- *)

datatype interpretation = Interpretation of rewrite list;

val natural = Interpretation [];

fun append (Interpretation l1) (Interpretation l2) = Interpretation (l1 @ l2);

(* ------------------------------------------------------------------------- *)
(* Translating OpenTheory names.                                             *)
(* ------------------------------------------------------------------------- *)

fun interpret rewrX (Interpretation l) x = foldl (fn (rw,x) => rewrX rw x) x l;

val interpretNamespace = interpret rewriteNamespace
and interpretType = interpret rewriteType
and interpretConst = interpret rewriteConst
and interpretRulespace = interpret rewriteRulespace
and interpretRule = interpret rewriteRule;

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun rewrToString rw = rewriteToString rw ^ "\n";
in
  fun toString (Interpretation l) = String.concat (map rewrToString l);

  fun toStringStream (Interpretation l) =
      Stream.map rewrToString (Stream.fromList l);
end;

fun toTextFile {filename,interpretation} =
    Stream.toTextFile {filename = filename} (toStringStream interpretation);

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

           (* The rewrite stream *)

           val rewrites = Parse.everything spacedRewriteParser chars

           val rewrites = Stream.toList rewrites
         in
           Interpretation rewrites
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in interpretation file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

end
