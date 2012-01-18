(* ========================================================================= *)
(* QUERYING THEORY PACKAGE DIRECTORIES                                       *)
(* Copyright (c) 2012 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryQuery :> DirectoryQuery =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val allKeywordString = "All"
and emptyKeywordString = "Empty"
and differenceSymbolString = "-"
and identityKeywordString = "Identity"
and includedByKeywordString = "IncludedBy"
and includesKeywordString = "Includes"
and intersectSymbolString = "&"
and latestKeywordString = "Latest"
and optionalSymbolString = "?"
and reflexiveTransitiveSymbolString = "*"
and requiredByKeywordString = "RequiredBy"
and requiresKeywordString = "Requires"
and subtheoriesKeywordString = "Subtheories"
and subtheoryOfKeywordString = "SubtheoryOf"
and transitiveSymbolString = "+"
and unionSymbolString = "|";

(* ------------------------------------------------------------------------- *)
(* A type of package query.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype set =
    Name of PackageName.name
  | NameVersion of PackageNameVersion.nameVersion
  | All
  | Empty;

datatype function =
    Identity
  | Constant of set
  | Requires
  | RequiredBy
  | Includes
  | IncludedBy
  | Subtheories
  | SubtheoryOf
  | Latest
  | Union of function * function
  | Intersect of function * function
  | Difference of function * function
  | ReflexiveTransitive of function
  | Transitive of function
  | Optional of function
  | Compose of function * function;

(* ------------------------------------------------------------------------- *)
(* Does the function ignore its input?                                       *)
(* ------------------------------------------------------------------------- *)

fun isConstant func =
    case func of
      Identity => false
    | Constant _ => true
    | Requires => false
    | RequiredBy => false
    | Includes => false
    | IncludedBy => false
    | Subtheories => false
    | SubtheoryOf => false
    | Latest => false
    | Union (func1,func2) => isConstant func1 andalso isConstant func2
    | Intersect (func1,func2) => isConstant func1 andalso isConstant func2
    | Difference (func1,func2) => isConstant func1 andalso isConstant func2
    | ReflexiveTransitive _ => false
    | Transitive func => isConstant func
    | Optional _ => false
    | Compose (func1,func2) => isConstant func1 orelse isConstant func2;

(* ------------------------------------------------------------------------- *)
(* Evaluating queries.                                                       *)
(* ------------------------------------------------------------------------- *)

fun evaluateSet dir set =
    case set of
      Name n => Directory.nameVersions dir n
    | NameVersion nv =>
      if Directory.member nv dir then PackageNameVersionSet.singleton nv
      else PackageNameVersionSet.empty
    | All => Directory.all dir
    | Empty => PackageNameVersionSet.empty;

local
  fun rtc f set =
      let
        val set' = PackageNameVersionSet.union (f set) set

        val converged =
            PackageNameVersionSet.size set' = PackageNameVersionSet.size set
      in
        if converged then set else rtc f set'
      end;
in
  fun evaluate dir func =
      case func of
        Identity => I
      | Constant set => K (evaluateSet dir set)
      | Requires => PackageNameVersionSet.lift (Directory.requires dir)
      | RequiredBy => PackageNameVersionSet.lift (Directory.requiredBy dir)
      | Includes => PackageNameVersionSet.lift (Directory.includes dir)
      | IncludedBy => PackageNameVersionSet.lift (Directory.includedBy dir)
      | Subtheories => PackageNameVersionSet.lift (Directory.subtheories dir)
      | SubtheoryOf => PackageNameVersionSet.lift (Directory.subtheoryOf dir)
      | Latest => PackageNameVersionSet.latestVersions
      | Union (func1,func2) =>
        let
          val f1 = evaluate dir func1
          and f2 = evaluate dir func2
        in
          fn s => PackageNameVersionSet.union (f1 s) (f2 s)
        end
      | Intersect (func1,func2) =>
        let
          val f1 = evaluate dir func1
          and f2 = evaluate dir func2
        in
          fn s => PackageNameVersionSet.intersect (f1 s) (f2 s)
        end
      | Difference (func1,func2) =>
        let
          val f1 = evaluate dir func1
          and f2 = evaluate dir func2
        in
          fn s => PackageNameVersionSet.difference (f1 s) (f2 s)
        end
      | ReflexiveTransitive func =>
        let
          val f = evaluate dir func
        in
          rtc f
        end
      | Transitive func =>
        let
          val f = evaluate dir func
        in
          rtc f o f
        end
      | Optional func =>
        let
          val f = evaluate dir func
        in
          fn s => PackageNameVersionSet.union s (f s)
        end
      | Compose (func1,func2) =>
        let
          val f1 = evaluate dir func1
          and f2 = evaluate dir func2
        in
          f1 o f2
        end;
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val infixes =
    Print.Infixes
      [{token = intersectSymbolString, precedence = 3, assoc = Print.LeftAssoc},
       {token = differenceSymbolString, precedence = 2, assoc = Print.LeftAssoc},
       {token = unionSymbolString, precedence = 2, assoc = Print.LeftAssoc}];

val ppAllKeyword = Print.ppString allKeywordString
and ppEmptyKeyword = Print.ppString emptyKeywordString
and ppIdentityKeyword = Print.ppString identityKeywordString
and ppIncludedByKeyword = Print.ppString includedByKeywordString
and ppIncludesKeyword = Print.ppString includesKeywordString
and ppLatestKeyword = Print.ppString latestKeywordString
and ppOptionalSymbol = Print.ppString optionalSymbolString
and ppReflexiveTransitiveSymbol = Print.ppString reflexiveTransitiveSymbolString
and ppRequiredByKeyword = Print.ppString requiredByKeywordString
and ppRequiresKeyword = Print.ppString requiresKeywordString
and ppSubtheoriesKeyword = Print.ppString subtheoriesKeywordString
and ppSubtheoryOfKeyword = Print.ppString subtheoryOfKeywordString
and ppTransitiveSymbol = Print.ppString transitiveSymbolString;

fun ppSet set =
    case set of
      Name name => PackageName.pp name
    | NameVersion namever => PackageNameVersion.pp namever
    | All => ppAllKeyword
    | Empty => ppEmptyKeyword;

local
  fun destUnary func =
      case func of
        ReflexiveTransitive f => SOME (f, ppReflexiveTransitiveSymbol)
      | Transitive f => SOME (f, ppTransitiveSymbol)
      | Optional f => SOME (f, ppOptionalSymbol)
      | _ => NONE;

  val stripCompose =
      let
        fun strip acc func =
            case func of
              Compose (f1,f2) => strip (f2 :: acc) f1
            | _ => (func,acc)
      in
        strip []
      end;

  fun destInfix func =
      case func of
        Union (f1,f2) => SOME (unionSymbolString, f1, f2)
      | Intersect (f1,f2) => SOME (intersectSymbolString, f1, f2)
      | Difference (f1,f2) => SOME (differenceSymbolString, f1, f2)
      | _ => NONE;

  fun isInfix func = Option.isSome (destInfix func);

  fun ppInfixToken (_,s) =
      Print.program [Print.space, Print.ppString s, Print.break];

  val ppInfix = Print.ppInfixes infixes destInfix ppInfixToken

  fun ppBasic func =
      case func of
        Identity => ppIdentityKeyword
      | Constant set => ppSet set
      | Requires => ppRequiresKeyword
      | RequiredBy => ppRequiredByKeyword
      | Includes => ppIncludesKeyword
      | IncludedBy => ppIncludedByKeyword
      | Subtheories => ppSubtheoriesKeyword
      | SubtheoryOf => ppSubtheoryOfKeyword
      | Latest => ppLatestKeyword
      | _ => ppBracket func

  and ppUnary func =
      case destUnary func of
        SOME (f,u) => Print.program [ppUnary f, u]
      | NONE => ppBasic func

  and ppComposeArgument func =
      Print.sequence
        (Print.ppBreak (Print.Break {size = 1, extraIndent = 2}))
        (ppUnary func)

  and ppCompose func =
      let
        val (f,fs) = stripCompose func
      in
        if List.null fs then ppUnary func
        else
          Print.inconsistentBlock 0
            (ppUnary f :: List.map ppComposeArgument fs)
      end

  and ppComposeHanging (func,_) = ppCompose func

  and ppHanging func_r = ppInfix ppComposeHanging func_r

  and ppNormal func = ppHanging (func,false)

  and ppBracket func = Print.ppBracket "(" ")" ppNormal func
in
  val pp = ppNormal;
end;

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

  val allKeywordParser = exactString allKeywordString
  and differenceSymbolParser = exactString differenceSymbolString
  and emptyKeywordParser = exactString emptyKeywordString
  and identityKeywordParser = exactString identityKeywordString
  and includedByKeywordParser = exactString includedByKeywordString
  and includesKeywordParser = exactString includesKeywordString
  and intersectSymbolParser = exactString intersectSymbolString
  and latestKeywordParser = exactString latestKeywordString
  and optionalSymbolParser = exactString optionalSymbolString
  and reflexiveTransitiveSymbolParser =
      exactString reflexiveTransitiveSymbolString
  and requiredByKeywordParser = exactString requiredByKeywordString
  and requiresKeywordParser = exactString requiresKeywordString
  and subtheoriesKeywordParser = exactString subtheoriesKeywordString
  and subtheoryOfKeywordParser = exactString subtheoryOfKeywordString
  and transitiveSymbolParser = exactString transitiveSymbolString
  and unionSymbolParser = exactString unionSymbolString;

  fun mkInfix (s,f1,f2) =
      if s = intersectSymbolString then Intersect (f1,f2)
      else if s = differenceSymbolString then Difference (f1,f2)
      else if s = unionSymbolString then Union (f1,f2)
      else raise Bug "DirectoryQuery.parser.mkInfix";

  val bracketSpaceParser =
      let
        val openSpace = exactChar #"(" ++ manySpace >> fst

        val closeSpace = exactChar #")" ++ manySpace >> fst
      in
        fn p => openSpace ++ p ++ closeSpace >> (fn ((),(v,())) => v)
      end;

  val setParser =
      PackageNameVersion.parser >> NameVersion ||
      PackageName.parser >> Name ||
      allKeywordParser >> K All ||
      emptyKeywordParser >> K Empty;

  val setSpaceParser = setParser ++ manySpace >> fst;

  val basicParser =
      identityKeywordParser >> K Identity ||
      requiresKeywordParser >> K Requires ||
      requiredByKeywordParser >> K RequiredBy ||
      includesKeywordParser >> K Includes ||
      includedByKeywordParser >> K IncludedBy ||
      subtheoriesKeywordParser >> K Subtheories ||
      subtheoryOfKeywordParser >> K SubtheoryOf ||
      latestKeywordParser >> K Latest;

  val basicSpaceParser =
      basicParser ++ manySpace >> fst ||
      setSpaceParser >> Constant;

  val unarySymbolParser =
      reflexiveTransitiveSymbolParser >> K ReflexiveTransitive ||
      transitiveSymbolParser >> K Transitive ||
      optionalSymbolParser >> K Optional;

  fun unarySymbolSpaceParser func =
      unarySymbolParser ++ manySpace >> (fn (f,()) => f func);

  val infixSymbolParser =
      differenceSymbolParser >> K differenceSymbolString ||
      intersectSymbolParser >> K intersectSymbolString ||
      unionSymbolParser >> K unionSymbolString;

  fun unarySpaceParser tokens =
      (basicSpaceParser >>++ mmany unarySymbolSpaceParser ||
       bracketSpaceParser functionSpaceParser) tokens

  and composeSpaceParser tokens =
      let
        fun composeWithUnary f1 =
            unarySpaceParser >> (fn f2 => Compose (f1,f2))
      in
        unarySpaceParser >>++ mmany composeWithUnary
      end tokens

  and functionSpaceParser tokens =
      parseInfixes infixes mkInfix infixSymbolParser composeSpaceParser tokens;
in
  val parserSet = manySpace ++ setSpaceParser >> snd;

  val parser = manySpace ++ functionSpaceParser >> snd;
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package query format: " ^ s);

end
