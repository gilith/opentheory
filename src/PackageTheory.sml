(* ========================================================================= *)
(* PACKAGE THEORY SYNTAX                                                     *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageTheory :> PackageTheory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val articleKeywordString = "article"
and checksumKeywordString = "checksum"
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

type name = PackageName.name;

datatype node =
    Article of
      {interpretation : Interpretation.interpretation,
       filename : string}
  | Include of
      {interpretation : Interpretation.interpretation,
       package : PackageNameVersion.nameVersion,
       checksum : Checksum.checksum option}
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

fun variantName {avoid} =
    let
      fun memberAvoid name = PackageNameSet.member name avoid
    in
      PackageName.variantName {avoid = memberAvoid}
    end;

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun articleNode node =
    case node of
      Article {filename = f, ...} => SOME {filename = f}
    | _ => NONE;

fun isArticleNode node = Option.isSome (articleNode node);

fun article thy = articleNode (node thy);

fun articles thys = List.mapPartial article thys;

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun includeNode node =
    case node of
      Include {package = nv, checksum = c, ...} => SOME (nv,c)
    | _ => NONE;

fun destInclude thy = includeNode (node thy);

fun includes thys = List.mapPartial destInclude thys;

fun updateIncludeNode f node =
    case node of
      Include {interpretation = i, package = nv, checksum = c} =>
      (case f nv c of
         SOME (nv,c) =>
         SOME (Include {interpretation = i, package = nv, checksum = c})
       | NONE => NONE)
    | _ => NONE;

fun updateInclude f thy =
    let
      val Theory {name,imports,node} = thy
    in
      case updateIncludeNode f node of
        SOME node => SOME (Theory {name = name, imports = imports, node = node})
      | NONE => NONE
    end;

fun updateIncludes f =
    let
      fun update thys =
          case thys of
            [] => NONE
          | thy :: thys =>
            let
              val thy' = updateInclude f thy
              and thys' = update thys
            in
              case thys' of
                SOME thys => SOME (Option.getOpt (thy',thy) :: thys)
              | NONE =>
                case thy' of
                  NONE => NONE
                | SOME thy => SOME (thy :: thys)
            end
    in
      update
    end;

(* ------------------------------------------------------------------------- *)
(* Union dependencies.                                                       *)
(* ------------------------------------------------------------------------- *)

fun isUnionNode node =
    case node of
      Union => true
    | _ => false;

fun isUnion thy = isUnionNode (node thy);

fun destUnion thy = if isUnion thy then SOME (imports thy) else NONE;

fun importsUnion thy =
    case destUnion thy of
      SOME ts => ts
    | NONE => [];

fun isEmpty thy =
    case destUnion thy of
      SOME ts => List.null ts
    | NONE => false;

(* ------------------------------------------------------------------------- *)
(* The main theory.                                                          *)
(* ------------------------------------------------------------------------- *)

val mainName = PackageName.mainTheory;

fun isMainName name = PackageName.equal name mainName;

fun isMain thy = isMainName (name thy);

fun emptyMain thy = isEmpty thy andalso isMain thy;

(* ------------------------------------------------------------------------- *)
(* Topological sort of theories.                                             *)
(* ------------------------------------------------------------------------- *)

datatype index = Index of theory list * theory PackageNameMap.map;

fun toListIndex (Index (thyl,_)) = thyl;

fun peekIndex (Index (_,idxm)) n = PackageNameMap.peek idxm n;

fun memberIndex n (Index (_,idxm)) = PackageNameMap.inDomain n idxm;

fun getIndex idx n =
    case peekIndex idx n of
      SOME thy => thy
    | NONE =>
      raise Error ("unknown theory block called " ^ PackageName.toString n);

fun mainIndex idx = getIndex idx mainName;

local
  fun add (thy,idxm) =
      let
        val n = name thy

        val () =
            if not (PackageNameMap.inDomain n idxm) then ()
            else raise Error ("multiple theory blocks called " ^
                              PackageName.toString n)
      in
        PackageNameMap.insert idxm (n,thy)
      end;

  fun checkImps idx thy =
      case List.find (fn n => not (memberIndex n idx)) (imports thy) of
        NONE => ()
      | SOME t =>
        let
          val n = name thy
        in
          raise Error ("theory block " ^ PackageName.toString n ^ " " ^
                       "imports unknown " ^ PackageName.toString t)
        end;
in
  fun fromListIndex thyl =
      let
        val idxm = List.foldl add (PackageNameMap.new ()) thyl

        val idx = Index (thyl,idxm)

        val () = List.app (checkImps idx) thyl
      in
        idx
      end;
end;

fun sortIndex {parents} idx =
    let
      fun sort (dealt,dealtset) (stack,stackset) work =
          case work of
            [] =>
            (case stack of
               [] => List.rev dealt
             | (thy,work,stackset) :: stack =>
               let
                 val dealt = thy :: dealt

                 val dealtset = PackageNameSet.add dealtset (name thy)
               in
                 sort (dealt,dealtset) (stack,stackset) work
               end)
          | thy :: work =>
            if PackageNameSet.member (name thy) dealtset then
              sort (dealt,dealtset) (stack,stackset) work
            else if PackageNameSet.member (name thy) stackset then
              let
                fun notT (thy',_,_) =
                    not (PackageName.equal (name thy') (name thy))

                val l = takeWhile notT stack

                val l = thy :: List.foldl (fn ((t,_,_),ts) => t :: ts) [thy] l

                val err = join " -> " (List.map (PackageName.toString o name) l)
              in
                raise Error ("circular dependency:\n" ^ err)
              end
            else
              let
                val thys = List.map (getIndex idx) (parents thy)

                val stack = (thy,work,stackset) :: stack

                val stackset = PackageNameSet.add stackset (name thy)

                val work = thys
              in
                sort (dealt,dealtset) (stack,stackset) work
              end

        val dealt = []
        and dealtset = PackageNameSet.empty
        and stack = []
        and stackset = PackageNameSet.empty
        and work = toListIndex idx
    in
      sort (dealt,dealtset) (stack,stackset) work
    end;

fun sortImports thys =
    sortIndex {parents = imports} (fromListIndex thys)
(*OpenTheoryDebug
    handle Error err => raise Error ("PackageTheory.sortImports: " ^ err);
*)

fun sortUnion thys =
    sortIndex {parents = importsUnion} (fromListIndex thys)
(*OpenTheoryDebug
    handle Error err => raise Error ("PackageTheory.sortUnion: " ^ err);
*)

local
  fun sorted {parents} =
      let
        fun check seen thys =
            case thys of
              [] => true
            | thy :: thys =>
              List.all (C PackageNameSet.member seen) (parents thy) andalso
              check (PackageNameSet.add seen (name thy)) thys
      in
        check PackageNameSet.empty
      end;
in
  val sortedImports = sorted {parents = imports}
  and sortedUnion = sorted {parents = importsUnion};
end;

(* ------------------------------------------------------------------------- *)
(* Theory constraints.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype constraint =
    ArticleConstraint of {filename : string}
  | ChecksumConstraint of Checksum.checksum
  | ImportConstraint of name
  | InterpretConstraint of Interpretation.rewrite
  | PackageConstraint of PackageNameVersion.nameVersion;

fun destArticleConstraint c =
    case c of
      ArticleConstraint f => SOME f
    | _ => NONE;

fun destChecksumConstraint c =
    case c of
      ChecksumConstraint x => SOME x
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

fun destChecksumConstraints cs = List.mapPartial destChecksumConstraint cs;

fun destImportConstraints cs = List.mapPartial destImportConstraint cs;

fun destInterpretConstraints cs = List.mapPartial destInterpretConstraint cs;

fun destPackageConstraints cs = List.mapPartial destPackageConstraint cs;

fun mkTheory (name,cs) =
    let
      val imports = destImportConstraints cs

      val rws = destInterpretConstraints cs

      val chks = destChecksumConstraints cs

      val node =
          case (destArticleConstraints cs, destPackageConstraints cs) of
            ([],[]) =>
            let
              val () =
                  if List.null rws then ()
                  else raise Error "interpret in union theory block"

              val () =
                  if List.null chks then ()
                  else raise Error "checksum in union theory block"
            in
              Union
            end
          | (_ :: _, _ :: _) =>
            raise Error "conflicting article and package in theory block"
          | (_ :: _ :: _, []) =>
            raise Error "multiple articles in theory block"
          | ([], _ :: _ :: _) =>
            raise Error "multiple packages in theory block"
          | ([{filename}],[]) =>
            let
              val () =
                  if List.null chks then ()
                  else raise Error "checksum in article theory block"

              val int = Interpretation.fromRewriteList rws
            in
              Article
                {interpretation = int,
                 filename = filename}
            end
          | ([],[p]) =>
            let
              val c =
                  case chks of
                    [] => NONE
                  | [c] => SOME c
                  | _ :: _ :: _ =>
                    raise Error "multiple checksums in package theory block"

              val int = Interpretation.fromRewriteList rws
            in
              Include
                {interpretation = int,
                 package = p,
                 checksum = c}
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

      val ics = List.map ImportConstraint imports

      val ncs =
          case node of
            Article {interpretation = int, filename = f} =>
            let
              val rws = Interpretation.toRewriteList int
            in
              List.map InterpretConstraint rws @
              [ArticleConstraint {filename = f}]
            end
          | Include {interpretation = int, package = p, checksum = c} =>
            let
              val rws = Interpretation.toRewriteList int

              val cs =
                  case c of
                    SOME chk => [chk]
                  | NONE => []
            in
              List.map InterpretConstraint rws @
              [PackageConstraint p] @
              List.map ChecksumConstraint cs
            end
          | Union =>
            []
    in
      (name, ics @ ncs)
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val escapeCharsFilename = [quoteChar];

val ppArticleKeyword = Print.ppString articleKeywordString
and ppChecksumKeyword = Print.ppString checksumKeywordString
and ppCloseBlock = Print.ppChar closeBlockChar
and ppImportKeyword = Print.ppString importKeywordString
and ppInterpretKeyword = Print.ppString interpretKeywordString
and ppOpenBlock = Print.ppChar openBlockChar
and ppPackageKeyword = Print.ppString packageKeywordString
and ppQuote = Print.ppChar quoteChar
and ppSeparator = Print.ppChar separatorChar;

fun ppBlock ppX x =
    Print.consistentBlock 0
      [Print.consistentBlock 2
         [ppOpenBlock,
          Print.break,
          ppX x],
       Print.break,
       ppCloseBlock];

fun ppFilename {filename} =
    Print.program
      [ppQuote,
       Print.ppEscapeString {escape = escapeCharsFilename} filename,
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
        ppNameValue ppArticleKeyword (ppFilename f)
      | ChecksumConstraint x =>
        ppNameValue ppChecksumKeyword (Checksum.pp x)
      | ImportConstraint r =>
        ppNameValue ppImportKeyword (PackageName.pp r)
      | InterpretConstraint r =>
        ppNameValue ppInterpretKeyword (Interpretation.ppRewrite r)
      | PackageConstraint p =>
        ppNameValue ppPackageKeyword (PackageNameVersion.pp p);
end;

fun ppConstraintList cs =
    case cs of
      [] => Print.skip
    | c :: cs =>
      Print.consistentBlock 0
        (ppConstraint c ::
         List.map (Print.sequence Print.newline o ppConstraint) cs);

fun pp thy =
    let
      val (n,cs) = destTheory thy
    in
      Print.consistentBlock 0
        (PackageName.pp n ::
         Print.ppString " " ::
         (if List.null cs then
            [ppOpenBlock,
             Print.ppString " ",
             ppCloseBlock]
          else
            [Print.consistentBlock 2
               [ppOpenBlock,
                Print.newline,
                ppConstraintList cs],
             Print.newline,
             ppCloseBlock]))
    end;

fun ppList thys =
    case thys of
      [] => Print.skip
    | thy :: thys =>
      let
        fun ppThy t = Print.program [Print.newline, Print.newline, pp t]
      in
        Print.consistentBlock 0
          (pp thy :: List.map ppThy thys)
      end;

val toStringFilename = Print.toString ppFilename;

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
  and checksumKeywordParser = exactString checksumKeywordString
  and closeBlockParser = exactChar closeBlockChar
  and importKeywordParser = exactString importKeywordString
  and interpretKeywordParser = exactString interpretKeywordString
  and openBlockParser = exactChar openBlockChar
  and packageKeywordParser = exactString packageKeywordString
  and quoteParser = exactChar quoteChar
  and separatorParser = exactChar separatorChar;

  val filenameParser =
      let
        val fileParser = escapeString {escape = escapeCharsFilename}
      in
        (quoteParser ++ fileParser ++ quoteParser) >>
        (fn ((),(f,())) => {filename = f})
      end;

  val articleConstraintParser =
      (articleKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       filenameParser) >>
      (fn ((),((),((),((),f)))) => ArticleConstraint f);

  val checksumConstraintParser =
      (checksumKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       Checksum.parser) >>
      (fn ((),((),((),((),r)))) => ChecksumConstraint r);

  val importConstraintParser =
      (importKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       PackageName.parser) >>
      (fn ((),((),((),((),r)))) => ImportConstraint r);

  val interpretConstraintParser =
      (interpretKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       Interpretation.parserRewrite) >>
      (fn ((),((),((),((),r)))) => InterpretConstraint r);

  val packageConstraintParser =
      (packageKeywordParser ++ manySpace ++
       separatorParser ++ manySpace ++
       PackageNameVersion.parser) >>
      (fn ((),((),((),((),p)))) => PackageConstraint p);

  val constraintParser =
      articleConstraintParser ||
      checksumConstraintParser ||
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
      (PackageName.parser ++ manySpace ++
       blockParser) >>
      (fn (n,((),cs)) => mkTheory (n,cs));

  val theorySpaceParser = theoryParser ++ manySpace >> fst;
in
  val parserFilename = filenameParser;

  val parser = manySpace ++ theorySpaceParser >> snd;

  val parserList = manySpace ++ many theorySpaceParser >> snd;
end;

fun fromStringFilename s =
    Parse.fromString parserFilename s
    handle Parse.NoParse =>
      raise Error ("bad filename format: " ^ s);

end
