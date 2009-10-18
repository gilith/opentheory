(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE IDS                                     *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageId :> PackageId =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val separatorString = "-";

(* ------------------------------------------------------------------------- *)
(* A type of theory package ids.                                             *)
(* ------------------------------------------------------------------------- *)

datatype id =
    Id of
      {base : string,
       version : PackageVersion.version};

fun base (Id {base = x, ...}) = x;

fun version (Id {version = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (i1,i2) =
    let
      val Id {base = b1, version = v1} = i1
      and Id {base = b2, version = v2} = i2
    in
      case String.compare (b1,b2) of
        LESS => LESS
      | EQUAL => PackageVersion.compare (v1,v2)
      | GREATER => GREATER
    end;

fun equal i1 i2 =
    let
      val Id {base = b1, version = v1} = i1
      and Id {base = b2, version = v2} = i2
    in
      b1 = b2 andalso PackageVersion.equal v1 v2
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSeparator = Print.addString separatorString;

fun pp (Id {base = b, version = v}) =
    Print.program
      [Print.ppString b,
       ppSeparator,
       PackageVersion.pp v];

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val separatorParser = exactString separatorString;

  val componentParser =
      let
        fun isInitialChar c = Char.isLower c

        fun isSubsequentChar c = Char.isLower c orelse Char.isDigit c
      in
        (some isInitialChar ++ many (some isSubsequentChar)) >>
        (fn (c,cs) => implode (c :: cs))
      end;

  val baseParser =
      componentParser ++
      many (separatorParser ++ componentParser >> snd) >>
in
  val parser =
      let
        fun isInitialChar c = Char.isLower c

        fun isSubsequentChar c = Char.isAlphaNum c orelse c = #"-"
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

end
