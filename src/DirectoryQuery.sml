(* ========================================================================= *)
(* QUERYING THEORY PACKAGE DIRECTORIES                                       *)
(* Copyright (c) 2012 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure DirectoryQuery :> DirectoryQuery =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val allKeywordString = "All"
and andSymbolString = "/\\"
and consistentWithRepoKeywordString = "ConsistentWithRepo"
and differenceSymbolString = "-"
and earlierThanRepoKeywordString = "EarlierThanRepo"
and emptyKeywordString = "Empty"
and identityKeywordString = "Identity"
and includedByKeywordString = "IncludedBy"
and includesKeywordString = "Includes"
and intersectSymbolString = "&"
and laterThanRepoKeywordString = "LaterThanRepo"
and latestKeywordString = "Latest"
and mineKeywordString = "Mine"
and noneKeywordString = "None"
and notSymbolString = "~"
and onRepoKeywordString = "OnRepo"
and optionalSymbolString = "?"
and orSymbolString = "\\/"
and reflexiveTransitiveSymbolString = "*"
and requiredByKeywordString = "RequiredBy"
and requiresKeywordString = "Requires"
and subtheoriesKeywordString = "Subtheories"
and subtheoryOfKeywordString = "SubtheoryOf"
and transitiveSymbolString = "+"
and upgradableKeywordString = "Upgradable"
and uploadableKeywordString = "Uploadable"
and unionSymbolString = "|";

(* ------------------------------------------------------------------------- *)
(* A type of package query.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype set =
    Name of PackageName.name
  | NameVersion of PackageNameVersion.nameVersion
  | All
  | None;

datatype predicate =
    Empty
  | Mine
  | OnRepo
  | ConsistentWithRepo
  | EarlierThanRepo
  | LaterThanRepo
  | Not of predicate
  | And of predicate * predicate
  | Or of predicate * predicate;

datatype function =
    Identity
  | Constant of set
  | Filter of predicate
  | Requires
  | RequiredBy
  | Includes
  | IncludedBy
  | Subtheories
  | SubtheoryOf
  | Latest
  | Upgradable  (* EarlierThanRepo *)
  | Uploadable  (* Mine & (~OnRepo /\ ~EarlierThanRepo /\ ConsistentWithRepo) *)
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

fun ignoresRepo pred =
    case pred of
      Empty => true
    | Mine => true
    | OnRepo => false
    | ConsistentWithRepo => false
    | EarlierThanRepo => false
    | LaterThanRepo => false
    | Not pred1 => ignoresRepo pred1
    | And (pred1,pred2) => ignoresRepo pred1 andalso ignoresRepo pred2
    | Or (pred1,pred2) => ignoresRepo pred1 andalso ignoresRepo pred2;

fun ignoresInput func =
    case func of
      Identity => false
    | Constant _ => true
    | Filter _ => false
    | Requires => false
    | RequiredBy => false
    | Includes => false
    | IncludedBy => false
    | Subtheories => false
    | SubtheoryOf => false
    | Latest => false
    | Upgradable => false
    | Uploadable => false
    | Union (func1,func2) => ignoresInput func1 andalso ignoresInput func2
    | Intersect (func1,func2) => ignoresInput func1 andalso ignoresInput func2
    | Difference (func1,func2) => ignoresInput func1 andalso ignoresInput func2
    | ReflexiveTransitive _ => false
    | Transitive func => ignoresInput func
    | Optional _ => false
    | Compose (func1,func2) => ignoresInput func1 orelse ignoresInput func2;

(* ------------------------------------------------------------------------- *)
(* Evaluating queries.                                                       *)
(* ------------------------------------------------------------------------- *)

val upgradableDef =
    Filter EarlierThanRepo;

val uploadableDef =
    Intersect
      (Filter Mine,
       Filter
         (And
            (Not OnRepo,
             And (Not EarlierThanRepo, ConsistentWithRepo))));

fun evaluateSet dir set =
    case set of
      Name n => Directory.nameVersions dir n
    | NameVersion nv =>
      if Directory.member nv dir then PackageNameVersionSet.singleton nv
      else PackageNameVersionSet.empty
    | All => Directory.all dir
    | None => PackageNameVersionSet.empty;

local
  fun evalPred dir pred namever repo =
      case pred of
        Empty => Directory.emptyTheory dir namever
      | Mine => Directory.selfAuthor dir namever
      | OnRepo => DirectoryRepo.member namever repo
      | ConsistentWithRepo => Directory.consistentWithRepo dir repo namever
      | EarlierThanRepo => Directory.earlierThanRepo dir repo namever
      | LaterThanRepo => Directory.laterThanRepo dir repo namever
      | Not pred1 => not (evalPred dir pred1 namever repo)
      | And (pred1,pred2) =>
        evalPred dir pred1 namever repo andalso
        evalPred dir pred2 namever repo
      | Or (pred1,pred2) =>
        evalPred dir pred1 namever repo orelse
        evalPred dir pred2 namever repo;
in
  fun evaluatePredicate dir repos pred namever =
      case repos of
        [] => false
      | repo :: _ =>
        if ignoresRepo pred then evalPred dir pred namever repo
        else List.exists (evalPred dir pred namever) repos
end;

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
  fun evaluate dir repos func =
      case func of
        Identity => I
      | Constant set => K (evaluateSet dir set)
      | Filter predicate =>
        let
          val pred = evaluatePredicate dir repos predicate
        in
          PackageNameVersionSet.filter pred
        end
      | Requires => PackageNameVersionSet.lift (Directory.requires dir)
      | RequiredBy => PackageNameVersionSet.lift (Directory.requiredBy dir)
      | Includes => PackageNameVersionSet.lift (Directory.includes dir)
      | IncludedBy => PackageNameVersionSet.lift (Directory.includedBy dir)
      | Subtheories => PackageNameVersionSet.lift (Directory.subtheories dir)
      | SubtheoryOf => PackageNameVersionSet.lift (Directory.subtheoryOf dir)
      | Latest => PackageNameVersionSet.latestVersions
      | Upgradable => evaluate dir repos upgradableDef
      | Uploadable => evaluate dir repos uploadableDef
      | Union (func1,func2) =>
        let
          val f1 = evaluate dir repos func1
          and f2 = evaluate dir repos func2
        in
          fn s => PackageNameVersionSet.union (f1 s) (f2 s)
        end
      | Intersect (func1,func2) =>
        let
          val f1 = evaluate dir repos func1
          and f2 = evaluate dir repos func2
        in
          fn s => PackageNameVersionSet.intersect (f1 s) (f2 s)
        end
      | Difference (func1,func2) =>
        let
          val f1 = evaluate dir repos func1
          and f2 = evaluate dir repos func2
        in
          fn s => PackageNameVersionSet.difference (f1 s) (f2 s)
        end
      | ReflexiveTransitive func =>
        let
          val f = evaluate dir repos func
        in
          rtc f
        end
      | Transitive func =>
        let
          val f = evaluate dir repos func
        in
          rtc f o f
        end
      | Optional func =>
        let
          val f = evaluate dir repos func
        in
          fn s => PackageNameVersionSet.union s (f s)
        end
      | Compose (func1,func2) =>
        let
          val f1 = evaluate dir repos func1
          and f2 = evaluate dir repos func2
        in
          f1 o f2
        end;
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val infixesPredicate =
    Print.Infixes
      [{token = andSymbolString, precedence = 3, assoc = Print.LeftAssoc},
       {token = orSymbolString, precedence = 2, assoc = Print.LeftAssoc}];

val infixes =
    Print.Infixes
      [{token = intersectSymbolString, precedence = 3, assoc = Print.LeftAssoc},
       {token = differenceSymbolString, precedence = 2, assoc = Print.LeftAssoc},
       {token = unionSymbolString, precedence = 2, assoc = Print.LeftAssoc}];

val ppAllKeyword = Print.ppString allKeywordString
and ppConsistentWithRepoKeyword = Print.ppString consistentWithRepoKeywordString
and ppEarlierThanRepoKeyword = Print.ppString earlierThanRepoKeywordString
and ppEmptyKeyword = Print.ppString emptyKeywordString
and ppIdentityKeyword = Print.ppString identityKeywordString
and ppIncludedByKeyword = Print.ppString includedByKeywordString
and ppIncludesKeyword = Print.ppString includesKeywordString
and ppLaterThanRepoKeyword = Print.ppString laterThanRepoKeywordString
and ppLatestKeyword = Print.ppString latestKeywordString
and ppMineKeyword = Print.ppString mineKeywordString
and ppNoneKeyword = Print.ppString noneKeywordString
and ppNotSymbol = Print.ppString notSymbolString
and ppOnRepoKeyword = Print.ppString onRepoKeywordString
and ppOptionalSymbol = Print.ppString optionalSymbolString
and ppReflexiveTransitiveSymbol = Print.ppString reflexiveTransitiveSymbolString
and ppRequiredByKeyword = Print.ppString requiredByKeywordString
and ppRequiresKeyword = Print.ppString requiresKeywordString
and ppSubtheoriesKeyword = Print.ppString subtheoriesKeywordString
and ppSubtheoryOfKeyword = Print.ppString subtheoryOfKeywordString
and ppTransitiveSymbol = Print.ppString transitiveSymbolString
and ppUpgradableKeyword = Print.ppString upgradableKeywordString
and ppUploadableKeyword = Print.ppString uploadableKeywordString;

fun ppSet set =
    case set of
      Name name => PackageName.pp name
    | NameVersion namever => PackageNameVersion.pp namever
    | All => ppAllKeyword
    | None => ppNoneKeyword;

local
  fun destUnary pred =
      case pred of
        Not p => SOME (ppNotSymbol, p)
      | _ => NONE;

  fun destInfix pred =
      case pred of
        And (p1,p2) => SOME (andSymbolString, p1, p2)
      | Or (p1,p2) => SOME (orSymbolString, p1, p2)
      | _ => NONE;

  fun isInfix pred = Option.isSome (destInfix pred);

  fun ppInfixToken (_,s) =
      Print.program [Print.space, Print.ppString s, Print.break];

  val ppInfix = Print.ppInfixes infixesPredicate destInfix ppInfixToken

  fun ppBasic pred =
      case pred of
        Empty => ppEmptyKeyword
      | Mine => ppMineKeyword
      | OnRepo => ppOnRepoKeyword
      | ConsistentWithRepo => ppConsistentWithRepoKeyword
      | EarlierThanRepo => ppEarlierThanRepoKeyword
      | LaterThanRepo => ppLaterThanRepoKeyword
      | _ => ppBracket pred

  and ppUnary pred =
      case destUnary pred of
        SOME (u,f) => Print.program [u, ppUnary f]
      | NONE => ppBasic pred

  and ppUnaryHanging (pred,_) = ppUnary pred

  and ppHanging pred_r = ppInfix ppUnaryHanging pred_r

  and ppNormal pred = ppHanging (pred,false)

  and ppBracket pred = Print.ppBracket "(" ")" ppNormal pred
in
  val ppBasicPredicate = ppBasic
  and ppPredicate = ppNormal;
end;

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
      | Filter pred => ppBasicPredicate pred
      | Requires => ppRequiresKeyword
      | RequiredBy => ppRequiredByKeyword
      | Includes => ppIncludesKeyword
      | IncludedBy => ppIncludedByKeyword
      | Subtheories => ppSubtheoriesKeyword
      | SubtheoryOf => ppSubtheoryOfKeyword
      | Latest => ppLatestKeyword
      | Upgradable => ppUpgradableKeyword
      | Uploadable => ppUploadableKeyword
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

  and ppFilter func =
      case func of
        Filter pred => ppPredicate pred
      | _ => ppCompose func

  and ppFilterHanging (func,_) = ppFilter func

  and ppHanging func_r = ppInfix ppFilterHanging func_r

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
  and noneKeywordParser = exactString noneKeywordString;

  val setParser =
      PackageNameVersion.parser >> NameVersion ||
      PackageName.parser >> Name ||
      allKeywordParser >> K All ||
      noneKeywordParser >> K None;

  val setSpaceParser = setParser ++ manySpace >> fst;
in
  val parserSet = manySpace ++ setSpaceParser >> snd;
end;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val andSymbolParser = exactString andSymbolString
  and consistentWithRepoKeywordParser =
      exactString consistentWithRepoKeywordString
  and earlierThanRepoKeywordParser = exactString earlierThanRepoKeywordString
  and emptyKeywordParser = exactString emptyKeywordString
  and laterThanRepoKeywordParser = exactString laterThanRepoKeywordString
  and mineKeywordParser = exactString mineKeywordString
  and notSymbolParser = exactString notSymbolString
  and onRepoKeywordParser = exactString onRepoKeywordString
  and orSymbolParser = exactString orSymbolString;

  val bracketSpaceParser =
      let
        val openSpace = exactChar #"(" ++ manySpace >> fst

        val closeSpace = exactChar #")" ++ manySpace >> fst
      in
        fn p => openSpace ++ p ++ closeSpace >> (fn ((),(v,())) => v)
      end;

  val basicKeywordParser =
      emptyKeywordParser >> K Empty ||
      mineKeywordParser >> K Mine ||
      consistentWithRepoKeywordParser >> K ConsistentWithRepo ||
      earlierThanRepoKeywordParser >> K EarlierThanRepo ||
      laterThanRepoKeywordParser >> K LaterThanRepo ||
      onRepoKeywordParser >> K OnRepo;

  val basicKeywordSpaceParser =
      basicKeywordParser ++ manySpace >> fst;

  val unarySymbolParser =
      notSymbolParser >> K Not;

  val unarySymbolSpaceParser =
      unarySymbolParser ++ manySpace >> fst;

  fun mkInfix (s,p1,p2) =
      if s = andSymbolString then And (p1,p2)
      else if s = orSymbolString then Or (p1,p2)
      else raise Bug "DirectoryQuery.parserPredicate.mkInfix";

  val infixSymbolParser =
      andSymbolParser >> K andSymbolString ||
      orSymbolParser >> K orSymbolString;

  val infixSymbolSpaceParser =
      infixSymbolParser ++ manySpace >> fst;

  fun basicSpaceParser tokens =
      (basicKeywordSpaceParser ||
       bracketSpaceParser predicateSpaceParser) tokens

  and unarySpaceParser tokens =
      (many unarySymbolSpaceParser ++ basicSpaceParser >>
       (fn (uns,pred) => List.foldr (fn (f,x) => f x) pred uns)) tokens

  and predicateSpaceParser tokens =
      parseInfixes infixesPredicate mkInfix infixSymbolSpaceParser
        unarySpaceParser tokens;
in
  val parserPredicate = manySpace ++ predicateSpaceParser >> snd;
end;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val differenceSymbolParser = exactString differenceSymbolString
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
  and unionSymbolParser = exactString unionSymbolString
  and upgradableKeywordParser = exactString upgradableKeywordString
  and uploadableKeywordParser = exactString uploadableKeywordString;

  val bracketSpaceParser =
      let
        val openSpace = exactChar #"(" ++ manySpace >> fst

        val closeSpace = exactChar #")" ++ manySpace >> fst
      in
        fn p => openSpace ++ p ++ closeSpace >> (fn ((),(v,())) => v)
      end;

  val basicKeywordParser =
      identityKeywordParser >> K Identity ||
      requiresKeywordParser >> K Requires ||
      requiredByKeywordParser >> K RequiredBy ||
      includesKeywordParser >> K Includes ||
      includedByKeywordParser >> K IncludedBy ||
      subtheoriesKeywordParser >> K Subtheories ||
      subtheoryOfKeywordParser >> K SubtheoryOf ||
      latestKeywordParser >> K Latest ||
      upgradableKeywordParser >> K Upgradable ||
      uploadableKeywordParser >> K Uploadable;

  val basicKeywordSpaceParser =
      basicKeywordParser ++ manySpace >> fst;

  val unarySymbolParser =
      reflexiveTransitiveSymbolParser >> K ReflexiveTransitive ||
      transitiveSymbolParser >> K Transitive ||
      optionalSymbolParser >> K Optional;

  fun unarySymbolSpaceParser func =
      unarySymbolParser ++ manySpace >> (fn (f,()) => f func);

  fun mkInfix (s,f1,f2) =
      if s = intersectSymbolString then Intersect (f1,f2)
      else if s = differenceSymbolString then Difference (f1,f2)
      else if s = unionSymbolString then Union (f1,f2)
      else raise Bug "DirectoryQuery.parser.mkInfix";

  val infixSymbolParser =
      differenceSymbolParser >> K differenceSymbolString ||
      intersectSymbolParser >> K intersectSymbolString ||
      unionSymbolParser >> K unionSymbolString;

  val infixSymbolSpaceParser =
      infixSymbolParser ++ manySpace >> fst;

  fun basicSpaceParser tokens =
      (basicKeywordSpaceParser ||
       parserSet >> Constant ||
       parserPredicate >> Filter ||
       bracketSpaceParser functionSpaceParser) tokens

  and unarySpaceParser tokens =
      (basicSpaceParser >>++
       mmany unarySymbolSpaceParser) tokens

  and composeSpaceParser tokens =
      let
        fun composeWithUnary f1 =
            unarySpaceParser >> (fn f2 => Compose (f1,f2))
      in
        unarySpaceParser >>++ mmany composeWithUnary
      end tokens

  and functionSpaceParser tokens =
      parseInfixes infixes mkInfix infixSymbolSpaceParser
        composeSpaceParser tokens;
in
  val parser = manySpace ++ functionSpaceParser >> snd;
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package query format: " ^ s);

end
