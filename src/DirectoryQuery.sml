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

val acyclicKeywordString = "Acyclic"
and allKeywordString = "All"
and andSymbolString = "/\\"
and closedKeywordString = "Closed"
and consistentWithRepoKeywordString = "ConsistentWithRepo"
and deprecatedKeywordString = "Deprecated"
and differenceSymbolString = "-"
and earlierThanRepoKeywordString = "EarlierThanRepo"
and emptyKeywordString = "Empty"
and identicalOnRepoKeywordString = "IdenticalOnRepo"
and identityKeywordString = "Identity"
and includedByKeywordString = "IncludedBy"
and includesKeywordString = "Includes"
and intersectSymbolString = "&"
and laterThanRepoKeywordString = "LaterThanRepo"
and latestKeywordString = "Latest"
and mineKeywordString = "Mine"
and noneKeywordString = "None"
and notSymbolString = "~"
and obsoleteKeywordString = "Obsolete"
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
and unionSymbolString = "|"
and wellFoundedKeywordString = "WellFounded";

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
  | Closed
  | Acyclic
  | WellFounded
  | OnRepo
  | IdenticalOnRepo
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
  | Deprecated  (* (Identity - Latest) (Requires|Includes)* *)
  | Obsolete  (* All - (Requires|Includes)* *)
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
    | Closed => true
    | Acyclic => true
    | WellFounded => true
    | OnRepo => false
    | IdenticalOnRepo => false
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
    | Deprecated => false
    | Obsolete => false
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

val deprecatedDef =
    Compose
      (Difference (Identity,Latest),
       ReflexiveTransitive (Union (Requires,Includes)));

val obsoleteDef =
    Difference
      (Constant All,
       ReflexiveTransitive (Union (Requires,Includes)));

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
  fun evalPred dir repo pred namever =
      case pred of
        Empty => Directory.emptyTheory dir namever
      | Mine => Directory.selfAuthor dir namever
      | OnRepo => DirectoryRepo.member namever repo
      | IdenticalOnRepo => Directory.identicalOnRepo dir repo namever
      | ConsistentWithRepo => Directory.consistentWithRepo dir repo namever
      | EarlierThanRepo => Directory.earlierThanRepo dir repo namever
      | LaterThanRepo => Directory.laterThanRepo dir repo namever
      | _ => raise Bug "DirectoryQuery.evalPred";

  fun evalPredSet dir repo pred namevers =
      if PackageNameVersionSet.null namevers then namevers
      else
        case pred of
          Closed => Directory.closedDependencies dir namevers
        | Acyclic => Directory.acyclicDependencies dir namevers
        | WellFounded => Directory.wellFoundedDependencies dir namevers
        | Not pred1 =>
          let
            val result1 = evalPredSet dir repo pred1 namevers
          in
            PackageNameVersionSet.difference namevers result1
          end
        | And (pred1,pred2) =>
          let
            val result1 = evalPredSet dir repo pred1 namevers

            val result2 = evalPredSet dir repo pred2 result1
          in
            result2
          end
        | Or (pred1,pred2) =>
          let
            val result1 = evalPredSet dir repo pred1 namevers

            val namevers = PackageNameVersionSet.difference namevers result1

            val result2 = evalPredSet dir repo pred2 namevers
          in
            PackageNameVersionSet.union result1 result2
          end
        | _ =>
          PackageNameVersionSet.filter (evalPred dir repo pred) namevers;

  fun evalPredSetRepo dir pred namevers (repo,result) =
      let
        val namevers' = PackageNameVersionSet.difference namevers result

        val result' = evalPredSet dir repo pred namevers'
      in
        PackageNameVersionSet.union result result'
      end;
in
  fun evaluatePredicateSet dir repos pred namevers =
      case repos of
        [] => PackageNameVersionSet.empty
      | repo :: repos =>
        let
          val result = evalPredSet dir repo pred namevers
        in
          if ignoresRepo pred then result
          else List.foldl (evalPredSetRepo dir pred namevers) result repos
        end;
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
      | Filter pred => evaluatePredicateSet dir repos pred
      | Requires => PackageNameVersionSet.lift (Directory.requires dir)
      | RequiredBy => PackageNameVersionSet.lift (Directory.requiredBy dir)
      | Includes => PackageNameVersionSet.lift (Directory.includes dir)
      | IncludedBy => PackageNameVersionSet.lift (Directory.includedBy dir)
      | Subtheories => PackageNameVersionSet.lift (Directory.subtheories dir)
      | SubtheoryOf => PackageNameVersionSet.lift (Directory.subtheoryOf dir)
      | Latest => PackageNameVersionSet.latestVersions
      | Deprecated => evaluate dir repos deprecatedDef
      | Obsolete => evaluate dir repos obsoleteDef
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

val ppAcyclicKeyword = Print.ppString acyclicKeywordString
and ppAllKeyword = Print.ppString allKeywordString
and ppClosedKeyword = Print.ppString closedKeywordString
and ppConsistentWithRepoKeyword = Print.ppString consistentWithRepoKeywordString
and ppDeprecatedKeyword = Print.ppString deprecatedKeywordString
and ppEarlierThanRepoKeyword = Print.ppString earlierThanRepoKeywordString
and ppEmptyKeyword = Print.ppString emptyKeywordString
and ppIdenticalOnRepoKeyword = Print.ppString identicalOnRepoKeywordString
and ppIdentityKeyword = Print.ppString identityKeywordString
and ppIncludedByKeyword = Print.ppString includedByKeywordString
and ppIncludesKeyword = Print.ppString includesKeywordString
and ppLaterThanRepoKeyword = Print.ppString laterThanRepoKeywordString
and ppLatestKeyword = Print.ppString latestKeywordString
and ppMineKeyword = Print.ppString mineKeywordString
and ppNoneKeyword = Print.ppString noneKeywordString
and ppNotSymbol = Print.ppString notSymbolString
and ppObsoleteKeyword = Print.ppString obsoleteKeywordString
and ppOnRepoKeyword = Print.ppString onRepoKeywordString
and ppOptionalSymbol = Print.ppString optionalSymbolString
and ppReflexiveTransitiveSymbol = Print.ppString reflexiveTransitiveSymbolString
and ppRequiredByKeyword = Print.ppString requiredByKeywordString
and ppRequiresKeyword = Print.ppString requiresKeywordString
and ppSubtheoriesKeyword = Print.ppString subtheoriesKeywordString
and ppSubtheoryOfKeyword = Print.ppString subtheoryOfKeywordString
and ppTransitiveSymbol = Print.ppString transitiveSymbolString
and ppUpgradableKeyword = Print.ppString upgradableKeywordString
and ppUploadableKeyword = Print.ppString uploadableKeywordString
and ppWellFoundedKeyword = Print.ppString wellFoundedKeywordString;

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
      | Closed => ppClosedKeyword
      | Acyclic => ppAcyclicKeyword
      | WellFounded => ppWellFoundedKeyword
      | OnRepo => ppOnRepoKeyword
      | IdenticalOnRepo => ppIdenticalOnRepoKeyword
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
      | Deprecated => ppDeprecatedKeyword
      | Obsolete => ppObsoleteKeyword
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

  val acyclicKeywordParser = exactString acyclicKeywordString
  and andSymbolParser = exactString andSymbolString
  and closedKeywordParser = exactString closedKeywordString
  and consistentWithRepoKeywordParser =
      exactString consistentWithRepoKeywordString
  and earlierThanRepoKeywordParser = exactString earlierThanRepoKeywordString
  and emptyKeywordParser = exactString emptyKeywordString
  and identicalOnRepoKeywordParser = exactString identicalOnRepoKeywordString
  and laterThanRepoKeywordParser = exactString laterThanRepoKeywordString
  and mineKeywordParser = exactString mineKeywordString
  and notSymbolParser = exactString notSymbolString
  and onRepoKeywordParser = exactString onRepoKeywordString
  and orSymbolParser = exactString orSymbolString
  and wellFoundedKeywordParser = exactString wellFoundedKeywordString;

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
      closedKeywordParser >> K Closed ||
      acyclicKeywordParser >> K Acyclic ||
      wellFoundedKeywordParser >> K WellFounded ||
      consistentWithRepoKeywordParser >> K ConsistentWithRepo ||
      earlierThanRepoKeywordParser >> K EarlierThanRepo ||
      identicalOnRepoKeywordParser >> K IdenticalOnRepo ||
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

  val deprecatedKeywordParser = exactString deprecatedKeywordString
  and differenceSymbolParser = exactString differenceSymbolString
  and identityKeywordParser = exactString identityKeywordString
  and includedByKeywordParser = exactString includedByKeywordString
  and includesKeywordParser = exactString includesKeywordString
  and intersectSymbolParser = exactString intersectSymbolString
  and latestKeywordParser = exactString latestKeywordString
  and obsoleteKeywordParser = exactString obsoleteKeywordString
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
      deprecatedKeywordParser >> K Deprecated ||
      obsoleteKeywordParser >> K Obsolete ||
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
