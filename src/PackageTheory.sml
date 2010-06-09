(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC USED IN PACKAGES                           *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageTheory :> PackageTheory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val articleKeywordString = "article"
and closeBlockString = "}"
and importKeywordString = "import"
and interpretKeywordString = "interpret"
and openBlockString = "{"
and packageKeywordString = "package"
and quoteString = "\""
and separatorString = ":";

(* ------------------------------------------------------------------------- *)
(* Types of package theory syntax.                                           *)
(* ------------------------------------------------------------------------- *)

type name = PackageBase.base

datatype theory =
    Theory of
      {imports : name list,
       node : PackageNode.node}

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun imports (Theory {imports = x, ...}) = x;

fun node (Theory {node = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun article thy = PackageNode.article (node thy);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun package thy = PackageNode.package (node thy);

(* ------------------------------------------------------------------------- *)
(* Theory constraints.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype constraint =
    ArticleConstraint of {filename : string}
  | ImportConstraint of name
  | InterpretConstraint of Interpretation.rewrite
  | PackageConstraint of PackageName.name;

fun destArticleConstraint c =
    case c of
      ArticleConstraint f => SOME f
    | _ => NONE;

fun destImportConstraint c =
    case c of
      ImportConstraint r => SOME r
    | _ => NONE;

fun destInterpretConstraint c =
    case c of
      InterpretConstraint r => SOME r
    | _ => NONE;

fun destPackageConstraint c =
    case c of
      PackageConstraint p => SOME p
    | _ => NONE;

fun destArticleConstraints cs = List.mapPartial destArticleConstraint cs;

fun destImportConstraints cs = List.mapPartial destImportConstraint cs;

fun destInterpretConstraints cs = List.mapPartial destInterpretConstraint cs;

fun destPackageConstraints cs = List.mapPartial destPackageConstraint cs;

fun mkTheory cs =
    let
      val imports = destImportConstraints cs

      val rws = destInterpretConstraints cs

      val node =
          case (destArticleConstraints cs, destPackageConstraints cs) of
            ([],[]) =>
            if null rws then PackageNode.Union
            else raise Error "interpret has no effect in union theory block"
          | (_ :: _, _ :: _) =>
            raise Error "conflicting article and package in theory block"
          | (_ :: _ :: _, []) =>
            raise Error "multiple articles in theory block"
          | ([], _ :: _ :: _) =>
            raise Error "multiple packages in theory block"
          | ([{filename}],[]) =>
            let
              val int = Interpretation.fromRewriteList rws
            in
              PackageNode.Article
                {interpretation = int,
                 filename = filename}
            end
          | ([],[p]) =>
            let
              val int = Interpretation.fromRewriteList rws
            in
              PackageNode.Package
                {interpretation = int,
                 package = p}
            end
    in
      Theory
        {imports = imports,
         node = node}
    end;

fun destTheory thy =
    let
      val Theory {imports,node} = thy

      val ics = map ImportConstraint imports

      val ncs =
          case node of
            PackageNode.Article {interpretation = int, filename = f} =>
            let
              val rws = Interpretation.toRewriteList int
            in
              map InterpretConstraint rws @ [ArticleConstraint {filename = f}]
            end
          | PackageNode.Package {interpretation = int, package = p} =>
            let
              val rws = Interpretation.toRewriteList int
            in
              map InterpretConstraint rws @ [PackageConstraint p]
            end
          | PackageNode.Union =>
            []
    in
      ics @ ncs
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppArticleKeyword = Print.addString articleKeywordString
and ppCloseBlock = Print.addString closeBlockString
and ppImportKeyword = Print.addString importKeywordString
and ppInterpretKeyword = Print.addString interpretKeywordString
and ppOpenBlock = Print.addString openBlockString
and ppPackageKeyword = Print.addString packageKeywordString
and ppQuote = Print.addString quoteString
and ppSeparator = Print.addString separatorString;

fun ppBlock ppX x =
    Print.blockProgram Print.Consistent 0
      [Print.blockProgram Print.Consistent 2
         [ppOpenBlock,
          Print.addBreak 1,
          ppX x],
       Print.addBreak 1,
       ppCloseBlock];

val ppName = PackageBase.pp;

fun ppQuotedFilename {filename} =
    Print.program
      [ppQuote,
       Print.addString filename,
       ppQuote];

local
  fun ppNameValue ppN ppV =
      Print.program
        [ppN,
         ppSeparator,
         Print.addString " ",
         ppV];
in
  fun ppConstraint c =
      case c of
        ArticleConstraint f =>
        ppNameValue ppArticleKeyword (ppQuotedFilename f)
      | ImportConstraint r =>
        ppNameValue ppImportKeyword (ppName r)
      | InterpretConstraint r =>
        ppNameValue ppInterpretKeyword (Interpretation.ppRewrite r)
      | PackageConstraint p =>
        ppNameValue ppPackageKeyword (PackageName.pp p);
end;

fun ppConstraintList cs =
    case cs of
      [] => Print.skip
    | c :: cs =>
      Print.blockProgram Print.Consistent 0
        (ppConstraint c ::
         map (Print.sequence Print.addNewline o ppConstraint) cs);

fun pp thy =
    let
      val cs = destTheory thy
    in
      ppBlock ppConstraintList cs
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

  val articleKeywordParser = exactString articleKeywordString
  and closeBlockParser = exactString closeBlockString
  and importKeywordParser = exactString importKeywordString
  and interpretKeywordParser = exactString interpretKeywordString
  and openBlockParser = exactString openBlockString
  and packageKeywordParser = exactString packageKeywordString
  and quoteParser = exactString quoteString
  and separatorParser = exactString separatorString;

  val nameParser = PackageBase.parser;

  val quotedFilenameParser =
      let
        fun isFilenameChar c = c <> #"\n" andalso c <> #"\""

        val filenameParser = atLeastOne (some isFilenameChar)
      in
        (quoteParser ++ filenameParser ++ quoteParser) >>
        (fn ((),(f,())) => {filename = implode f})
      end;

  val articleConstraintParser =
      (articleKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       quotedFilenameParser) >>
      (fn ((),((),((),((),f)))) => ArticleConstraint f);

  val importConstraintParser =
      (importKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       nameParser) >>
      (fn ((),((),((),((),r)))) => ImportConstraint r);

  val interpretConstraintParser =
      (interpretKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       Interpretation.parserRewrite) >>
      (fn ((),((),((),((),r)))) => InterpretConstraint r);

  val packageConstraintParser =
      (packageKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       PackageName.parser) >>
      (fn ((),((),((),((),p)))) => PackageConstraint p);

  val constraintParser =
      articleConstraintParser ||
      importConstraintParser ||
      interpretConstraintParser ||
      packageConstraintParser;

  val constraintSpaceParser = constraintParser ++ manySpace >> fst;

  val theoryParser =
      (openBlockParser ++ manySpace ++
       many constraintSpaceParser ++
       closeBlockParser) >>
      (fn ((),((),(cs,()))) => mkTheory cs);

  val theorySpaceParser = theoryParser ++ manySpace >> fst;
in
  val parserName = nameParser;

  val parser = manySpace ++ theorySpaceParser >> snd;
end;

end
