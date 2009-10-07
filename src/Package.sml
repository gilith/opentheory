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

datatype require =
    Require of
      {name : string,
       package : string,
       interpretation : Interpretation.interpretation,
       import : string list};

datatype theory =
    Local of theory * theory
  | Sequence of theory list
  | Article of {filename : string}
  | Interpret of Interpretation.interpretation * theory
  | Import of {require : string};

datatype package =
    Package of
      {tags : tag list,
       requires : require list,
       theory : theory};

(* ------------------------------------------------------------------------- *)
(* Require block constraints.                                                *)
(* ------------------------------------------------------------------------- *)

datatype constraint =
    PackageConstraint of string
  | InterpretConstraint of Interpretation.rewrite
  | ImportConstraint of string;

fun destPackageConstraint c =
    case c of
      PackageConstraint p => SOME p
    | _ => NONE;

fun destInterpretConstraint c =
    case c of
      InterpretConstraint r => SOME r
    | _ => NONE;

fun destImportConstraint c =
    case c of
      ImportConstraint r => SOME r
    | _ => NONE;

fun mkRequire (name,cs) =
    let
      val package =
          case List.mapPartial destPackageConstraint cs of
            [] => raise Error "no package specified in require block"
          | [p] => p
          | _ :: _ :: _ =>
            raise Error "multiple packages specified in require block"

      val rws = List.mapPartial destInterpretConstraint cs

      val interpretation = Interpretation.fromRewriteList rws

      val import = List.mapPartial destImportConstraint cs
    in
      Require
        {name = name,
         package = package,
         interpretation = interpretation,
         import = import}
    end;

fun destRequire req =
    let
      val Require {name,package,interpretation,import} = req

      val rws = Interpretation.toRewriteList interpretation

      val int = map InterpretConstraint rws

      val imp = map ImportConstraint import

      val cs = PackageConstraint package :: int @ imp
    in
      (name,cs)
    end;

fun ppConstraint c =
    case c of
      PackageConstraint p =>
      Print.sequence
        (Print.addString "package: ")
        (Print.addString p)
    | InterpretConstraint r =>
      Print.sequence
        (Print.addString "interpret: ")
        (Interpretation.ppRewrite r)
    | ImportConstraint r =>
      Print.sequence
        (Print.addString "import: ")
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

fun ppRequire req =
    let
      val (name,cs) = destRequire req
    in
      Print.blockProgram Print.Consistent 0
        [Print.addString "require ",
         Print.addString name,
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

fun ppTheory thy =
    case thy of
      Local (thy1,thy2) =>
      Print.blockProgram Print.Consistent 0
        [Print.addString "local ",
         ppTheory thy1,
         Print.addString " in",
         Print.addBreak 1,
         ppTheory thy2]
    | Sequence thys =>
      ppBlock ppTheoryList thys
    | Article {filename} =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "article",
         Print.addBreak 1,
         Print.addString "\"",
         Print.addString filename,
         Print.addString "\";"]
    | Interpret (int,thy) =>
      Print.blockProgram Print.Consistent 0
        [Print.addString "interpret ",
         ppBlock Interpretation.pp int,
         Print.addString " in",
         Print.addBreak 1,
         ppTheory thy]
    | Import {require} =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "import",
         Print.addBreak 1,
         Print.addString require,
         Print.addString ";"]

and ppTheoryList thys =
    case thys of
      [] => Print.skip
    | thy :: thys =>
      Print.program
        (ppTheory thy ::
         map (Print.sequence (Print.addBreak 1) o ppTheory) thys);

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

  val articleKeywordParser = exactString "article"
  and closeBlockParser = exactString "}"
  and colonParser = exactString ":"
  and importKeywordParser = exactString "import"
  and inKeywordParser = exactString "in"
  and interpretKeywordParser = exactString "interpret"
  and localKeywordParser = exactString "local"
  and newlineParser = exactString "\n"
  and openBlockParser = exactString "{"
  and packageKeywordParser = exactString "package"
  and quoteParser = exactString "\""
  and requireKeywordParser = exactString "require"
  and terminatorParser = exactString ";";

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

  val packageNameParser = identifierParser;

  val quotedFilenameParser =
      let
        fun isFilenameChar c = c <> #"\n" andalso c <> #"\""

        val filenameParser = atLeastOne (some isFilenameChar)
      in
        (quoteParser ++ filenameParser ++ quoteParser) >>
        (fn ((),(f,())) => {filename = implode f})
      end;

  val tagParser =
      (fieldParser ++ manySpace ++ colonParser ++ manySpace ++ valueParser) >>
      (fn (f,((),((),((),v)))) => Tag {field = f, value = v});

  val tagSpaceParser = tagParser ++ manySpace >> fst;

  val packageConstraintParser =
      (packageKeywordParser ++ manySpace ++
       colonParser ++ manySpace ++
       packageNameParser) >>
      (fn ((),((),((),((),p)))) => PackageConstraint p);

  val interpretConstraintParser =
      (interpretKeywordParser ++ manySpace ++
       colonParser ++ manySpace ++
       Interpretation.parserRewrite) >>
      (fn ((),((),((),((),r)))) => InterpretConstraint r);

  val importConstraintParser =
      (importKeywordParser ++ manySpace ++
       colonParser ++ manySpace ++
       requireNameParser) >>
      (fn ((),((),((),((),r)))) => ImportConstraint r);

  val constraintParser =
      packageConstraintParser ||
      interpretConstraintParser ||
      importConstraintParser;

  val constraintSpaceParser = constraintParser ++ manySpace >> fst;

  val requireParser =
      (requireKeywordParser ++ atLeastOneSpace ++
       requireNameParser ++ manySpace ++
       openBlockParser ++ manySpace ++
       many constraintSpaceParser ++ closeBlockParser) >>
      (fn ((),((),(n,((),((),((),(cs,()))))))) => mkRequire (n,cs));

  val requireSpaceParser = requireParser ++ manySpace >> fst;

  fun theoryParser inp =
      (localParser ||
       sequenceParser ||
       articleParser ||
       interpretParser ||
       importParser) inp

  and localParser inp =
      ((localKeywordParser ++ atLeastOneSpace ++ theoryParser ++ manySpace ++
        inKeywordParser ++ atLeastOneSpace ++ theoryParser) >>
       (fn ((),((),(t1,((),((),((),t2)))))) => Local (t1,t2))) inp

  and sequenceParser inp =
      ((openBlockParser ++ manySpace ++ many theorySpaceParser ++
        closeBlockParser) >>
       (fn ((),((),(ts,()))) => Sequence ts)) inp

  and articleParser inp =
      ((articleKeywordParser ++ manySpace ++ quotedFilenameParser ++
        manySpace ++ terminatorParser) >>
       (fn ((),((),(f,((),())))) => Article f)) inp

  and interpretParser inp =
      ((interpretKeywordParser ++ manySpace ++ openBlockParser ++ manySpace ++
        Interpretation.parser ++ manySpace ++ closeBlockParser ++
        inKeywordParser ++ atLeastOneSpace ++ theoryParser) >>
       (fn ((),((),((),((),(i,((),((),((),((),t))))))))) => Interpret (i,t))) inp

  and importParser inp =
      ((importKeywordParser ++ atLeastOneSpace ++ requireNameParser ++
        manySpace ++ terminatorParser) >>
       (fn ((),((),(r,((),())))) => Import {require = r})) inp

  and theorySpaceParser inp = (theoryParser ++ manySpace >> fst) inp;

  val packageParser =
      (many tagSpaceParser ++
       many requireSpaceParser ++
       theorySpaceParser) >>
      (fn (ts,(rs,th)) => Package {tags = ts, requires = rs, theory = th});

  val packageSpaceParser = packageParser ++ manySpace >> fst;
in
  val parserTag = manySpace ++ tagSpaceParser >> snd;

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
