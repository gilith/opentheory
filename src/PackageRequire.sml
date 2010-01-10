(* ========================================================================= *)
(* REQUIRED THEORY PACKAGES                                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageRequire :> PackageRequire =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val closeBlockString = "}"
and importKeywordString = "import"
and interpretKeywordString = "interpret"
and openBlockString = "{"
and packageKeywordString = "package"
and requireKeywordString = "require"
and separatorString = ":";

(* ------------------------------------------------------------------------- *)
(* A type of required theory packages.                                       *)
(* ------------------------------------------------------------------------- *)

type name = string;

datatype require =
    Require of
      {name : name,
       imports : name list,
       interpretation : Interpretation.interpretation,
       package : PackageName.name};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun name (Require {name = x, ...}) = x;

fun imports (Require {imports = x, ...}) = x;

fun interpretation (Require {interpretation = x, ...}) = x;

fun package (Require {package = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Require block constraints.                                                *)
(* ------------------------------------------------------------------------- *)

datatype constraint =
    ImportConstraint of name
  | InterpretConstraint of Interpretation.rewrite
  | PackageConstraint of PackageName.name;

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

fun mkRequire (name,cs) =
    let
      val imports = List.mapPartial destImportConstraint cs

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
         imports = imports,
         interpretation = interpretation,
         package = package}
    end;

fun destRequire req =
    let
      val Require {name,imports,interpretation,package} = req

      val imps = map ImportConstraint imports

      val rws = Interpretation.toRewriteList interpretation

      val ints = map InterpretConstraint rws

      val cs = imps @ ints @ [PackageConstraint package]
    in
      (name,cs)
    end;

(* ------------------------------------------------------------------------- *)
(* Topological sort of requirements.                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun toMap reql =
      let
        fun ins (req,(m,l)) =
            let
              val Require {name = n, imports = rs, ...} = req

              val m = StringMap.insert m (n,(rs,req))

              val l = n :: l
            in
              (m,l)
            end

        val reqs_namel as (reqs,_) = List.foldl ins (StringMap.new (), []) reql

        fun check (n,(rs,_)) =
            case List.find (fn r => not (StringMap.inDomain r reqs)) rs of
              NONE => ()
            | SOME r =>
              let
                val err = "require block \"" ^ n ^ "\" " ^
                          "imports non-block \"" ^ r ^ "\""
              in
                raise Error ("PackageRequire.sort: unknown import:\n" ^ err)
              end

        val () = StringMap.app check reqs
      in
        reqs_namel
      end;

  fun sortMap requires (dealt,dealtset) (stack,stackset) work =
      case work of
        [] =>
        (case stack of
           [] => rev dealt
         | (r,(req,work,stackset)) :: stack =>
           let
             val dealt = req :: dealt
             val dealtset = StringSet.add dealtset r
           in
             sortMap requires (dealt,dealtset) (stack,stackset) work
           end)
      | r :: work =>
        if StringSet.member r dealtset then
          sortMap requires (dealt,dealtset) (stack,stackset) work
        else if StringSet.member r stackset then
          let
            val l = map fst (takeWhile (fn (r',_) => r' <> r) stack)
            val l = r :: rev (r :: l)
            val err = join " -> " l
          in
            raise Error ("PackageRequire.sort: circular dependency:\n" ^ err)
          end
        else
          let
            val (rs,req) =
                case StringMap.peek requires r of
                  SOME rs_req => rs_req
                | NONE => raise Bug "PackageRequire.sort"

            val stack = (r,(req,work,stackset)) :: stack

            val stackset = StringSet.add stackset r

            val work = rs
          in
            sortMap requires (dealt,dealtset) (stack,stackset) work
          end;
in
  fun sort reqs =
      let
        val (reqs,work) = toMap reqs

        val dealt = []
        val dealtset = StringSet.empty

        val stack = []
        val stackset = StringSet.empty
      in
        sortMap reqs (dealt,dealtset) (stack,stackset) work
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppCloseBlock = Print.addString closeBlockString
and ppImportKeyword = Print.addString importKeywordString
and ppInterpretKeyword = Print.addString interpretKeywordString
and ppOpenBlock = Print.addString openBlockString
and ppPackageKeyword = Print.addString packageKeywordString
and ppRequireKeyword = Print.addString requireKeywordString
and ppSeparator = Print.addString separatorString;

fun ppBlock ppX x =
    Print.blockProgram Print.Consistent 0
      [Print.blockProgram Print.Consistent 2
         [ppOpenBlock,
          Print.addBreak 1,
          ppX x],
       Print.addBreak 1,
       ppCloseBlock];

val ppName = Print.ppString;

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
        ImportConstraint r =>
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

fun pp req =
    let
      val (name,cs) = destRequire req
    in
      Print.blockProgram Print.Consistent 0
        [ppRequireKeyword,
         Print.addString " ",
         ppName name,
         Print.addString " ",
         ppBlock ppConstraintList cs]
    end;

fun ppList reqs =
    case reqs of
      [] => Print.skip
    | req :: reqs =>
      let
        fun ppReq r = Print.program [Print.addNewline, Print.addNewline, pp r]
      in
        Print.blockProgram Print.Consistent 0
          (pp req :: map ppReq reqs)
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
  and importKeywordParser = exactString importKeywordString
  and interpretKeywordParser = exactString interpretKeywordString
  and openBlockParser = exactString openBlockString
  and packageKeywordParser = exactString packageKeywordString
  and requireKeywordParser = exactString requireKeywordString
  and separatorParser = exactString separatorString;

  val nameParser =
      let
        fun isInitialChar c = Char.isLower c

        fun isSubsequentChar c = Char.isAlphaNum c orelse c = #"-"
      in
        (some isInitialChar ++ many (some isSubsequentChar)) >>
        (fn (c,cs) => implode (c :: cs))
      end;

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
      importConstraintParser ||
      interpretConstraintParser ||
      packageConstraintParser;

  val constraintSpaceParser = constraintParser ++ manySpace >> fst;

  val requireParser =
      (requireKeywordParser ++ atLeastOneSpace ++
       nameParser ++ manySpace ++
       openBlockParser ++ manySpace ++
       many constraintSpaceParser ++
       closeBlockParser) >>
      (fn ((),((),(n,((),((),((),(cs,()))))))) => mkRequire (n,cs));

  val requireSpaceParser = requireParser ++ manySpace >> fst;
in
  val parserName = nameParser;

  val parser = manySpace ++ requireSpaceParser >> snd;

  val parserList = manySpace ++ many requireSpaceParser >> snd;
end;

end
