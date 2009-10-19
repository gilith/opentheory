(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE SYNTAX                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Package :> Package =
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

type theory = PackageRequire.name Theory.theory;

datatype package =
    Package of
      {tags : Tag.tag list,
       requires : PackageRequire.require list,
       theory : theory};

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

val ppTheory = Theory.pp PackageRequire.ppName;

fun pp pkg =
    let
      val Package {tags,requires,theory} = pkg
    in
      Print.blockProgram Print.Consistent 0
        [Tag.ppList tags,
         PackageRequire.ppList requires,
         Print.addNewline,
         ppTheory theory]
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

  val identifierParser =
      let
        fun isInitialChar c = Char.isLower c

        fun isIdentifierChar c = Char.isAlphaNum c
      in
        (some isInitialChar ++ many (some isIdentifierChar)) >>
        (fn (c,cs) => implode (c :: cs))
      end;

  val fieldParser = identifierParser;

  val valueParser =
      let
        fun isValueChar c = c <> #"\n"
      in
        many (some isValueChar) >> implode
      end;

  val requireNameParser = identifierParser;

  val nameParser = identifierParser;

  val tagParser =
      (fieldParser ++ manySpace ++ colonParser ++ manySpace ++ valueParser) >>
      (fn (f,((),((),((),v)))) => Tag {field = f, value = v});

  val tagSpaceParser = tagParser ++ manySpace >> fst;

  val packageConstraintParser =
      (packageKeywordParser ++ manySpace ++
       colonParser ++ manySpace ++
       nameParser) >>
      (fn ((),((),((),((),p)))) => PackageConstraint p);

  val interpretConstraintParser =
      (interpretKeywordParser ++ manySpace ++
       colonParser ++ manySpace ++
       Interpretation.parserRewrite) >>
      (fn ((),((),((),((),r)))) => InterpretConstraint r);

  val requireConstraintParser =
      (requireKeywordParser ++ manySpace ++
       colonParser ++ manySpace ++
       requireNameParser) >>
      (fn ((),((),((),((),r)))) => RequireConstraint r);

  val constraintParser =
      packageConstraintParser ||
      interpretConstraintParser ||
      requireConstraintParser;

  val constraintSpaceParser = constraintParser ++ manySpace >> fst;

  val requireParser =
      (requireKeywordParser ++ atLeastOneSpace ++
       requireNameParser ++ manySpace ++
       openBlockParser ++ manySpace ++
       many constraintSpaceParser ++ closeBlockParser) >>
      (fn ((),((),(n,((),((),((),(cs,()))))))) => mkRequire (n,cs));

  val requireSpaceParser = requireParser ++ manySpace >> fst;

  val theoryParser =
      (theoryKeywordParser ++ manySpace ++
       openBlockParser ++
       Theory.parser requireNameParser ++
       closeBlockParser) >>
      (fn ((),((),((),(t,())))) => t);

  val theorySpaceParser = theoryParser ++ manySpace >> fst;

  val packageParser =
      (many tagSpaceParser ++
       many requireSpaceParser ++
       theorySpaceParser) >>
      (fn (ts,(rs,th)) => Package {tags = ts, requires = rs, theory = th});

  val packageSpaceParser = packageParser ++ manySpace >> fst;
in
  val parserTag = manySpace ++ tagSpaceParser >> snd;

  val parserRequireName = requireNameParser;

  val parserName = nameParser;

  val parserRequire = manySpace ++ requireSpaceParser >> snd;

  val parserTheory = manySpace ++ theorySpaceParser >> snd;

  val parser = manySpace ++ packageSpaceParser >> snd;

  val parser' = parser >> (fn pkg => [pkg]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {package,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp package);

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
