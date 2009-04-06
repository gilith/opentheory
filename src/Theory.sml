(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC                                            *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Theory :> Theory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theories.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype theory =
    Local of theory * theory
  | Block of theory list
  | Article of {filename : string}
  | Interpret of Interpretation.interpretation;

val empty = Block [];

fun compile {savable} thy = raise Bug "Theory.compile: not implemented";

fun toArticle thy = compile {savable = true} thy;

fun toSummary thy = Article.summarize (compile {savable = false} thy);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun ppBlock ppX x =
    Print.blockProgram Print.Consistent 0
      [Print.addString "{",
       Print.blockProgram Print.Consistent 2
         [Print.addBreak 1,
          ppX x],
       Print.addString "}",
       Print.addNewline];

fun pp thy =
    case thy of
      Local (thy1,thy2) =>
      Print.blockProgram Print.Consistent 0
        [Print.addString "local ",
         pp thy1,
         Print.addBreak 1,
         Print.addString "in ",
         pp thy2]
    | Block thys => ppBlock ppList thys
    | Article {filename} =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "article",
         Print.addBreak 1,
         Print.addString "\"",
         Print.addString filename,
         Print.addString "\";"]
    | Interpret int =>
      Print.blockProgram Print.Consistent 0
        [Print.addString "interpret ",
         ppBlock Interpretation.pp int]

and ppList thys =
    case thys of
      [] => Print.skip
    | thy :: thys => ppList1 thy thys

and ppList1 thy thys =
    case thys of
      [] => pp thy
    | thy' :: thys =>
      Print.program
        [pp thy,
         Print.addBreak 1,
         ppList1 thy' thys];

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val space = many (some Char.isSpace) >> K ();

  val space1 = atLeastOne (some Char.isSpace) >> K ();

  fun keywordParser s = exactList (explode s) >> K ();

  val localKeywordParser = keywordParser "local"
  and inKeywordParser = keywordParser "in"
  and openBlockParser = keywordParser "{"
  and closeBlockParser = keywordParser "}"
  and articleKeywordParser = keywordParser "article"
  and terminatorParser = keywordParser ";"
  and quoteParser = keywordParser "\""
  and interpretKeywordParser = keywordParser "interpret";

  val quotedFilenameParser =
      let
        fun isFilenameChar c = c <> #"\n" andalso c <> #"\""

        val filenameParser = atLeastOne (some isFilenameChar)
      in
        (quoteParser ++ filenameParser ++ quoteParser) >>
        (fn ((),(f,())) => {filename = implode f})
      end;

  fun theoryParser inp =
      (localParser ||
       blockParser ||
       articleParser ||
       interpretParser) inp

  and localParser inp =
      ((localKeywordParser ++ space1 ++ theoryParser ++ space ++
        inKeywordParser ++ space1 ++ theoryParser) >>
       (fn ((),((),(t1,((),((),((),t2)))))) => Local (t1,t2))) inp

  and blockParser inp =
      ((openBlockParser ++ space ++ many theorySpaceParser ++
        closeBlockParser) >>
       (fn ((),((),(ts,()))) => Block ts)) inp

  and articleParser inp =
      ((articleKeywordParser ++ space ++ quotedFilenameParser ++
        space ++ terminatorParser) >>
       (fn ((),((),(f,((),())))) => Article f)) inp

  and interpretParser inp =
      ((interpretKeywordParser ++ space ++ openBlockParser ++ space ++
        Interpretation.parser ++ space ++ closeBlockParser) >>
       (fn ((),((),((),((),(i,((),())))))) => Interpret i)) inp

  and theorySpaceParser inp = ((theoryParser ++ space) >> fst) inp;
in
  val parser = (space ++ theorySpaceParser) >> snd;

  val parser' = parser >> (fn thy => [thy]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {filename,theory} =
    Stream.toTextFile {filename = filename} (Print.toStream pp theory);

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

           (* The theory stream *)

           val thys = Parse.everything parser' chars
         in
           Block (Stream.toList thys)
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in theory file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

end
