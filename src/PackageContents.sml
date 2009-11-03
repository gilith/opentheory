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

val closeBlockString = "}"
and openBlockString = "{"
and theoryKeywordString = "theory";

(* ------------------------------------------------------------------------- *)
(* Types of theory package syntax.                                           *)
(* ------------------------------------------------------------------------- *)

datatype contents =
    Contents of
      {tags : Tag.tag list,
       requires : PackageRequire.require list,
       theory : PackageTheory.theory};

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppCloseBlock = Print.addString closeBlockString
and ppOpenBlock = Print.addString openBlockString
and ppTheoryKeyword = Print.addString theoryKeywordString;

fun ppBlock ppX x =
    Print.blockProgram Print.Consistent 0
      [Print.blockProgram Print.Consistent 2
         [ppOpenBlock,
          Print.addBreak 1,
          ppX x],
       Print.addBreak 1,
       ppCloseBlock];

fun pp pkg =
    let
      val Contents {tags,requires,theory} = pkg
    in
      Print.blockProgram Print.Consistent 0
        [Tag.ppList tags,
         PackageRequire.ppList requires,
         Print.addNewline,
         PackageTheory.pp theory]
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

  val closeBlockParser = exactString closeBlockString
  and openBlockParser = exactString openBlockString
  and theoryKeywordParser = exactString theoryKeywordString;

  val theoryParser =
      (theoryKeywordParser ++ manySpace ++
       openBlockParser ++
       PackageTheory.parser ++
       closeBlockParser) >>
      (fn ((),((),((),(t,())))) => t);

  val theorySpaceParser = theoryParser ++ manySpace >> fst;

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
