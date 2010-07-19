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
and closeBlockChar = #"}"
and importKeywordString = "import"
and interpretKeywordString = "interpret"
and openBlockChar = #"{"
and packageKeywordString = "package"
and quoteChar = #"\""
and separatorChar = #":";

(* ------------------------------------------------------------------------- *)
(* Types of package theory syntax.                                           *)
(* ------------------------------------------------------------------------- *)

type name = PackageBase.base;

datatype node =
    Article of
      {interpretation : Interpretation.interpretation,
       filename : string}
  | Package of
      {interpretation : Interpretation.interpretation,
       package : PackageName.name}
  | Union;

datatype theory =
    Theory of
      {name : name,
       imports : name list,
       node : node};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun name (Theory {name = x, ...}) = x;

fun imports (Theory {imports = x, ...}) = x;

fun node (Theory {node = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Generating fresh theory names.                                            *)
(* ------------------------------------------------------------------------- *)

fun mkName {avoid} =
    let
      fun memberAvoid name = PackageBaseSet.member name avoid
    in
      PackageBase.mkName {avoid = memberAvoid}
    end;

(* ------------------------------------------------------------------------- *)
(* The main theory.                                                          *)
(* ------------------------------------------------------------------------- *)

val mainName = PackageBase.main;

fun isMainName name = PackageBase.equal name mainName;

fun isMain thy = isMainName (name thy);

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun articleNode node =
    case node of
      Article {filename = f, ...} => SOME {filename = f}
    | _ => NONE;

fun article thy = articleNode (node thy);

fun articles thys = List.mapPartial article thys;

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun packageNode node =
    case node of
      Package {package = p, ...} => SOME p
    | _ => NONE;

fun package thy = packageNode (node thy);

fun packages thys = List.mapPartial package thys;

(* ------------------------------------------------------------------------- *)
(* Topological sort of theories.                                             *)
(* ------------------------------------------------------------------------- *)

local
  fun toMap thyl =
      let
        fun ins (thy,(m,l)) =
            let
              val n = name thy
              and ts = imports thy

              val m = PackageBaseMap.insert m (n,(ts,thy))

              val l = n :: l
            in
              (m,l)
            end

        val thys_namel as (thys,_) =
            List.foldl ins (PackageBaseMap.new (), []) thyl

        fun check (n,(ts,_)) =
            case List.find (fn t => not (PackageBaseMap.inDomain t thys)) ts of
              NONE => ()
            | SOME t =>
              raise Error ("theory block \"" ^ PackageBase.toString n ^ "\" " ^
                           "imports unknown \"" ^ PackageBase.toString t ^ "\"")

        val () = PackageBaseMap.app check thys
      in
        thys_namel
      end;

  fun sortMap requires (dealt,dealtset) (stack,stackset) work =
      case work of
        [] =>
        (case stack of
           [] => rev dealt
         | (r,(req,work,stackset)) :: stack =>
           let
             val dealt = req :: dealt
             val dealtset = PackageBaseSet.add dealtset r
           in
             sortMap requires (dealt,dealtset) (stack,stackset) work
           end)
      | r :: work =>
        if PackageBaseSet.member r dealtset then
          sortMap requires (dealt,dealtset) (stack,stackset) work
        else if PackageBaseSet.member r stackset then
          let
            fun notR (r',_) = not (PackageBase.equal r' r)

            val l = map fst (takeWhile notR stack)
            val l = r :: rev (r :: l)
            val err = join " -> " (map PackageBase.toString l)
          in
            raise Error ("circular dependency:\n" ^ err)
          end
        else
          let
            val (rs,req) =
                case PackageBaseMap.peek requires r of
                  SOME rs_req => rs_req
                | NONE => raise Bug "PackageRequire.sort"

            val stack = (r,(req,work,stackset)) :: stack

            val stackset = PackageBaseSet.add stackset r

            val work = rs
          in
            sortMap requires (dealt,dealtset) (stack,stackset) work
          end;
in
  fun sort reqs =
      let
        val (reqs,work) = toMap reqs

        val dealt = []
        val dealtset = PackageBaseSet.empty

        val stack = []
        val stackset = PackageBaseSet.empty
      in
        sortMap reqs (dealt,dealtset) (stack,stackset) work
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("PackageTheory.sort: " ^ err);
*)
end;

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

fun mkTheory (name,cs) =
    let
      val imports = destImportConstraints cs

      val rws = destInterpretConstraints cs

      val node =
          case (destArticleConstraints cs, destPackageConstraints cs) of
            ([],[]) =>
            if null rws then Union
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
              Article
                {interpretation = int,
                 filename = filename}
            end
          | ([],[p]) =>
            let
              val int = Interpretation.fromRewriteList rws
            in
              Package
                {interpretation = int,
                 package = p}
            end
    in
      Theory
        {name = name,
         imports = imports,
         node = node}
    end;

fun destTheory thy =
    let
      val Theory {name,imports,node} = thy

      val ics = map ImportConstraint imports

      val ncs =
          case node of
            Article {interpretation = int, filename = f} =>
            let
              val rws = Interpretation.toRewriteList int
            in
              map InterpretConstraint rws @ [ArticleConstraint {filename = f}]
            end
          | Package {interpretation = int, package = p} =>
            let
              val rws = Interpretation.toRewriteList int
            in
              map InterpretConstraint rws @ [PackageConstraint p]
            end
          | Union =>
            []
    in
      (name, ics @ ncs)
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppArticleKeyword = Print.ppString articleKeywordString
and ppCloseBlock = Print.ppChar closeBlockChar
and ppImportKeyword = Print.ppString importKeywordString
and ppInterpretKeyword = Print.ppString interpretKeywordString
and ppOpenBlock = Print.ppChar openBlockChar
and ppPackageKeyword = Print.ppString packageKeywordString
and ppQuote = Print.ppChar quoteChar
and ppSeparator = Print.ppChar separatorChar;

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
       Print.ppString filename,
       ppQuote];

local
  fun ppNameValue ppN ppV =
      Print.program
        [ppN,
         ppSeparator,
         Print.ppString " ",
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
      val (n,cs) = destTheory thy
    in
      Print.blockProgram Print.Consistent 0
        (ppName n ::
         Print.addString " " ::
         (if null cs then
            [ppOpenBlock,
             Print.addString " ",
             ppCloseBlock]
          else
            [Print.blockProgram Print.Consistent 2
               [ppOpenBlock,
                Print.addNewline,
                ppConstraintList cs],
             Print.addNewline,
             ppCloseBlock]))
    end;

fun ppList thys =
    case thys of
      [] => Print.skip
    | thy :: thys =>
      let
        fun ppThy t = Print.program [Print.addNewline, Print.addNewline, pp t]
      in
        Print.blockProgram Print.Consistent 0
          (pp thy :: map ppThy thys)
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
  and closeBlockParser = exactChar closeBlockChar
  and importKeywordParser = exactString importKeywordString
  and interpretKeywordParser = exactString interpretKeywordString
  and openBlockParser = exactChar openBlockChar
  and packageKeywordParser = exactString packageKeywordString
  and quoteParser = exactChar quoteChar
  and separatorParser = exactChar separatorChar;

  val nameParser = PackageBase.parser;

  val filenameParser =
      let
        val fileParser = escapeString {escape = [quoteChar]}
      in
        (quoteParser ++ fileParser ++ quoteParser) >>
        (fn ((),(f,())) => {filename = f})
      end;

  val articleConstraintParser =
      (articleKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       filenameParser) >>
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

  val blockParser =
      (openBlockParser ++ manySpace ++
       many constraintSpaceParser ++
       closeBlockParser) >>
      (fn ((),((),(cs,()))) => cs);

  val theoryParser =
      (nameParser ++ manySpace ++
       blockParser) >>
      (fn (n,((),cs)) => mkTheory (n,cs));

  val theorySpaceParser = theoryParser ++ manySpace >> fst;
in
  val parserName = nameParser;

  val parserFilename = filenameParser;

  val parser = manySpace ++ theorySpaceParser >> snd;

  val parserList = manySpace ++ many theorySpaceParser >> snd;
end;

fun fromStringFilename s =
    Parse.fromString parserFilename s
    handle Parse.NoParse =>
      raise Error ("bad filename format: " ^ s);

end
