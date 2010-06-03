(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE SYNTAX                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageContents :> PackageContents =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val theoryKeywordString = "theory";

(* ------------------------------------------------------------------------- *)
(* Types of theory package syntax.                                           *)
(* ------------------------------------------------------------------------- *)

datatype contents =
    Contents of
      {tags : Tag.tag list,
       requires : PackageRequire.require list,
       theory : PackageTheory.theory};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun tags (Contents {tags = x, ...}) = x;

fun requires (Contents {requires = x, ...}) = x;

fun theory (Contents {theory = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun articles c =
    let
      val Contents {requires = reqs, theory = thy, ...} = c

      val fs = PackageRequire.articles reqs
    in
      case PackageTheory.article thy of
        SOME f => fs @ [f]
      | NONE => fs
    end;

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun packages c =
    let
      val Contents {requires = reqs, theory = thy, ...} = c

      val ps = PackageRequire.packages reqs
    in
      case PackageTheory.package thy of
        SOME p => ps @ [p]
      | NONE => ps
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppTheoryKeyword = Print.addString theoryKeywordString;

fun ppBlock ppL l =
    if null l then Print.skip
    else
      Print.program
        [ppL l,
         Print.addNewline,
         Print.addNewline];

fun ppTheory thy =
    Print.blockProgram Print.Consistent 0
      [ppTheoryKeyword,
       Print.addString " ",
       PackageTheory.pp thy];

fun pp pkg =
    let
      val Contents {tags, requires = reqs, theory = thy} = pkg
    in
      Print.blockProgram Print.Consistent 0
        [ppBlock Tag.ppList tags,
         ppBlock PackageRequire.ppList reqs,
         ppTheory thy]
    end;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val theoryKeywordParser = exactString theoryKeywordString;

  val theorySpaceParser = theoryKeywordParser ++ PackageTheory.parser >> snd;

  val packageSpaceParser =
      (Tag.parserList ++
       PackageRequire.parserList ++
       theorySpaceParser) >>
      (fn (ts,(rs,th)) => Contents {tags = ts, requires = rs, theory = th});
in
  val parser = manySpace ++ packageSpaceParser >> snd;

  val parser' = parser >> (fn pkg => [pkg]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {contents,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp contents);

fun fromTextFile {filename} =
    let
      (* Estimating parse error line numbers *)

      val lines = Stream.fromTextFile {filename = filename}

      val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
    in
      (let
         (* The character stream *)

         val chars = Stream.filter (not o Interpretation.isCommentLine) chars

         val chars = Parse.everything Parse.any chars

         (* The package stream *)

         val pkgs = Parse.everything parser' chars
       in
         case Stream.toList pkgs of
           [] => raise Error "missing theory block"
         | [pkg] => pkg
         | _ :: _ :: _ => raise Error "multiple theory blocks"
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in package file \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

end
