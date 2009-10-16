(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE SYNTAX                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Package :> Package =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Types of theory package syntax.                                           *)
(* ------------------------------------------------------------------------- *)

datatype tag =
    Tag of
      {field : string,
       value : string};

type requireName = string;

type name = string;

datatype require =
    Require of
      {name : requireName,
       requires : requireName list,
       interpretation : Interpretation.interpretation,
       package : name};

type theory = requireName Theory.theory;

datatype package =
    Package of
      {tags : tag list,
       requires : require list,
       theory : theory};

(* ------------------------------------------------------------------------- *)
(* Require block constraints.                                                *)
(* ------------------------------------------------------------------------- *)

datatype constraint =
    RequireConstraint of string
  | InterpretConstraint of Interpretation.rewrite
  | PackageConstraint of string;

fun destRequireConstraint c =
    case c of
      RequireConstraint r => SOME r
    | _ => NONE;

fun destInterpretConstraint c =
    case c of
      InterpretConstraint r => SOME r
    | _ => NONE;

fun destPackageConstraint c =
    case c of
      PackageConstraint p => SOME p
    | _ => NONE;

fun mkRequire (name,cs) =
    let
      val requires = List.mapPartial destRequireConstraint cs

      val rws = List.mapPartial destInterpretConstraint cs

      val interpretation = Interpretation.fromRewriteList rws

      val package =
          case List.mapPartial destPackageConstraint cs of
            [] => raise Error "no package specified in require block"
          | [p] => p
          | _ :: _ :: _ =>
            raise Error "multiple packages specified in require block"
    in
      Require
        {name = name,
         requires = requires,
         interpretation = interpretation,
         package = package}
    end;

fun destRequire req =
    let
      val Require {name,requires,interpretation,package} = req

      val reqs = map RequireConstraint requires

      val rws = Interpretation.toRewriteList interpretation

      val ints = map InterpretConstraint rws

      val cs = reqs @ ints @ [PackageConstraint package]
    in
      (name,cs)
    end;

val ppName = Print.ppString;

fun ppConstraint c =
    case c of
      PackageConstraint p =>
      Print.sequence
        (Print.addString "package: ")
        (ppName p)
    | InterpretConstraint r =>
      Print.sequence
        (Print.addString "interpret: ")
        (Interpretation.ppRewrite r)
    | RequireConstraint r =>
      Print.sequence
        (Print.addString "require: ")
        (Print.addString r);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun ppBlock ppX x =
    Print.blockProgram Print.Consistent 0
      [Print.blockProgram Print.Consistent 2
         [Print.addString "{",
          Print.addBreak 1,
          ppX x],
       Print.addBreak 1,
       Print.addString "}"];

fun ppTag tag =
    let
      val Tag {field,value} = tag
    in
      Print.program
        [Print.addString field,
         Print.addString ": ",
         Print.addString value]
    end;

fun ppTagList tags =
    Print.blockProgram Print.Consistent 0
      (map (fn t => Print.sequence (ppTag t) Print.addNewline) tags);

fun ppConstraintList cs =
    Print.blockProgram Print.Consistent 0
      (map (fn c => Print.sequence (ppConstraint c) Print.addNewline) cs);

val ppRequireName = Print.ppString;

fun ppRequire req =
    let
      val (name,cs) = destRequire req
    in
      Print.blockProgram Print.Consistent 0
        [Print.addString "require ",
         ppRequireName name,
         Print.addString " ",
         ppBlock ppConstraintList cs]
    end;

val ppRequireList =
    let
      fun ppReq req =
          Print.program [Print.addNewline, ppRequire req, Print.addNewline]
    in
      Print.blockProgram Print.Consistent 0 o map ppReq
    end;

val ppTheory = Theory.pp ppRequireName;

fun pp pkg =
    let
      val Package {tags,requires,theory} = pkg
    in
      Print.blockProgram Print.Consistent 0
        [ppTagList tags,
         ppRequireList requires,
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

  val closeBlockParser = exactString "}"
  and colonParser = exactString ":"
  and interpretKeywordParser = exactString "interpret"
  and newlineParser = exactString "\n"
  and openBlockParser = exactString "{"
  and packageKeywordParser = exactString "package"
  and quoteParser = exactString "\""
  and requireKeywordParser = exactString "require"
  and theoryKeywordParser = exactString "theory";

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
