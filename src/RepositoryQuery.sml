(* ========================================================================= *)
(* QUERYING PACKAGE REPOSITORIES                                             *)
(* Copyright (c) 2012 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure RepositoryQuery :> RepositoryQuery =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val acyclicKeywordString = "Acyclic"
and allKeywordString = "All"
and andSymbolString = "/\\"
and closedKeywordString = "Closed"
and consistentWithRemoteKeywordString = "ConsistentWithRemote"
and deprecatedKeywordString = "Deprecated"
and differenceSymbolString = "-"
and earlierThanRemoteKeywordString = "EarlierThanRemote"
and emptyKeywordString = "Empty"
and identicalOnRemoteKeywordString = "IdenticalOnRemote"
and identityKeywordString = "Identity"
and includedByKeywordString = "IncludedBy"
and includesKeywordString = "Includes"
and intersectSymbolString = "&"
and laterThanRemoteKeywordString = "LaterThanRemote"
and latestKeywordString = "Latest"
and mineKeywordString = "Mine"
and noneKeywordString = "None"
and notSymbolString = "~"
and obsoleteKeywordString = "Obsolete"
and onRemoteKeywordString = "OnRemote"
and optionalSymbolString = "?"
and orSymbolString = "\\/"
and reflexiveTransitiveSymbolString = "*"
and requiredByKeywordString = "RequiredBy"
and requiresKeywordString = "Requires"
and subtheoriesKeywordString = "Subtheories"
and subtheoryOfKeywordString = "SubtheoryOf"
and transitiveSymbolString = "+"
and upToDateKeywordString = "UpToDate"
and upgradableKeywordString = "Upgradable"
and uploadableKeywordString = "Uploadable"
and unionSymbolString = "|"
and versionsKeywordString = "Versions";

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
  | UpToDate
  | OnRemote
  | IdenticalOnRemote
  | ConsistentWithRemote
  | EarlierThanRemote
  | LaterThanRemote
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
  | Versions
  | Latest
  | Deprecated
  | Obsolete
  | Upgradable
  | Uploadable
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

fun ignoresRemote pred =
    case pred of
      Empty => true
    | Mine => true
    | Closed => true
    | Acyclic => true
    | UpToDate => true
    | OnRemote => false
    | IdenticalOnRemote => false
    | ConsistentWithRemote => false
    | EarlierThanRemote => false
    | LaterThanRemote => false
    | Not pred1 => ignoresRemote pred1
    | And (pred1,pred2) => ignoresRemote pred1 andalso ignoresRemote pred2
    | Or (pred1,pred2) => ignoresRemote pred1 andalso ignoresRemote pred2;

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
    | Versions => false
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
    (* (Identity - Latest) (Requires|Includes)* *)
    Compose
      (Difference (Identity,Latest),
       ReflexiveTransitive (Union (Requires,Includes)));

val obsoleteDef =
    (* All - (Requires|Includes)* *)
    Difference
      (Constant All,
       ReflexiveTransitive (Union (Requires,Includes)));

val upgradableDef =
    (* EarlierThanRemote *)
    Filter EarlierThanRemote;

val uploadableDef =
    (* Mine & (~OnRemote /\ LaterThanRemote /\ ConsistentWithRemote) *)
    Intersect
      (Filter Mine,
       Filter (And (Not OnRemote, And (LaterThanRemote,ConsistentWithRemote))));

fun evaluateSet repo set =
    case set of
      Name n =>
      (case Repository.latestNameVersion repo n of
         NONE => PackageNameVersionSet.empty
       | SOME nv => PackageNameVersionSet.singleton nv)
    | NameVersion nv =>
      if Repository.member nv repo then PackageNameVersionSet.singleton nv
      else PackageNameVersionSet.empty
    | All => Repository.all repo
    | None => PackageNameVersionSet.empty;

local
  fun evalPred repo remote pred namever =
      case pred of
        Empty => Repository.emptyTheories repo namever
      | Mine => Repository.selfAuthor repo namever
      | OnRemote => RepositoryRemote.member namever remote
      | IdenticalOnRemote => Repository.identicalOnRemote repo remote namever
      | ConsistentWithRemote =>
        Repository.consistentWithRemote repo remote namever
      | EarlierThanRemote =>
        RepositoryRemote.earlierThanLatestNameVersion remote namever
      | LaterThanRemote =>
        RepositoryRemote.laterThanLatestNameVersion remote namever
      | _ => raise Bug "RepositoryQuery.evalPred";

  fun evalPredSet repo remote pred namevers =
      if PackageNameVersionSet.null namevers then namevers
      else
        case pred of
          Closed => Repository.closedDependencies repo namevers
        | Acyclic => Repository.acyclicDependencies repo namevers
        | UpToDate => Repository.upToDateDependencies repo namevers
        | Not pred1 =>
          let
            val result1 = evalPredSet repo remote pred1 namevers
          in
            PackageNameVersionSet.difference namevers result1
          end
        | And (pred1,pred2) =>
          let
            val result1 = evalPredSet repo remote pred1 namevers

            val result2 = evalPredSet repo remote pred2 result1
          in
            result2
          end
        | Or (pred1,pred2) =>
          let
            val result1 = evalPredSet repo remote pred1 namevers

            val namevers = PackageNameVersionSet.difference namevers result1

            val result2 = evalPredSet repo remote pred2 namevers
          in
            PackageNameVersionSet.union result1 result2
          end
        | _ =>
          PackageNameVersionSet.filter (evalPred repo remote pred) namevers;

  fun evalPredSetRemote repo pred namevers (remote,result) =
      let
        val namevers' = PackageNameVersionSet.difference namevers result

        val result' = evalPredSet repo remote pred namevers'
      in
        PackageNameVersionSet.union result result'
      end;
in
  fun evaluatePredicateSet repo remotes pred namevers =
      case remotes of
        [] => PackageNameVersionSet.empty
      | remote :: remotes =>
        let
          val result = evalPredSet repo remote pred namevers
        in
          if ignoresRemote pred then result
          else List.foldl (evalPredSetRemote repo pred namevers) result remotes
        end;
end;

local
  fun versions repo nv =
      Repository.nameVersions repo (PackageNameVersion.name nv);

  fun rtc f set =
      let
        val set' = PackageNameVersionSet.union (f set) set

        val converged =
            PackageNameVersionSet.size set' = PackageNameVersionSet.size set
      in
        if converged then set else rtc f set'
      end;
in
  fun evaluate repo remotes func =
      case func of
        Identity => I
      | Constant set => K (evaluateSet repo set)
      | Filter pred => evaluatePredicateSet repo remotes pred
      | Requires => PackageNameVersionSet.lift (Repository.requires repo)
      | RequiredBy => PackageNameVersionSet.lift (Repository.requiredBy repo)
      | Includes => PackageNameVersionSet.lift (Repository.includes repo)
      | IncludedBy => PackageNameVersionSet.lift (Repository.includedBy repo)
      | Subtheories => PackageNameVersionSet.lift (Repository.subtheories repo)
      | SubtheoryOf => PackageNameVersionSet.lift (Repository.subtheoryOf repo)
      | Versions => PackageNameVersionSet.lift (versions repo)
      | Latest => PackageNameVersionSet.latestVersions
      | Deprecated => evaluate repo remotes deprecatedDef
      | Obsolete => evaluate repo remotes obsoleteDef
      | Upgradable => evaluate repo remotes upgradableDef
      | Uploadable => evaluate repo remotes uploadableDef
      | Union (func1,func2) =>
        let
          val f1 = evaluate repo remotes func1
          and f2 = evaluate repo remotes func2
        in
          fn s => PackageNameVersionSet.union (f1 s) (f2 s)
        end
      | Intersect (func1,func2) =>
        let
          val f1 = evaluate repo remotes func1
          and f2 = evaluate repo remotes func2
        in
          fn s => PackageNameVersionSet.intersect (f1 s) (f2 s)
        end
      | Difference (func1,func2) =>
        let
          val f1 = evaluate repo remotes func1
          and f2 = evaluate repo remotes func2
        in
          fn s => PackageNameVersionSet.difference (f1 s) (f2 s)
        end
      | ReflexiveTransitive func =>
        let
          val f = evaluate repo remotes func
        in
          rtc f
        end
      | Transitive func =>
        let
          val f = evaluate repo remotes func
        in
          rtc f o f
        end
      | Optional func =>
        let
          val f = evaluate repo remotes func
        in
          fn s => PackageNameVersionSet.union s (f s)
        end
      | Compose (func1,func2) =>
        let
          val f1 = evaluate repo remotes func1
          and f2 = evaluate repo remotes func2
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
and ppConsistentWithRemoteKeyword = Print.ppString consistentWithRemoteKeywordString
and ppDeprecatedKeyword = Print.ppString deprecatedKeywordString
and ppEarlierThanRemoteKeyword = Print.ppString earlierThanRemoteKeywordString
and ppEmptyKeyword = Print.ppString emptyKeywordString
and ppIdenticalOnRemoteKeyword = Print.ppString identicalOnRemoteKeywordString
and ppIdentityKeyword = Print.ppString identityKeywordString
and ppIncludedByKeyword = Print.ppString includedByKeywordString
and ppIncludesKeyword = Print.ppString includesKeywordString
and ppLaterThanRemoteKeyword = Print.ppString laterThanRemoteKeywordString
and ppLatestKeyword = Print.ppString latestKeywordString
and ppMineKeyword = Print.ppString mineKeywordString
and ppNoneKeyword = Print.ppString noneKeywordString
and ppNotSymbol = Print.ppString notSymbolString
and ppObsoleteKeyword = Print.ppString obsoleteKeywordString
and ppOnRemoteKeyword = Print.ppString onRemoteKeywordString
and ppOptionalSymbol = Print.ppString optionalSymbolString
and ppReflexiveTransitiveSymbol = Print.ppString reflexiveTransitiveSymbolString
and ppRequiredByKeyword = Print.ppString requiredByKeywordString
and ppRequiresKeyword = Print.ppString requiresKeywordString
and ppSubtheoriesKeyword = Print.ppString subtheoriesKeywordString
and ppSubtheoryOfKeyword = Print.ppString subtheoryOfKeywordString
and ppTransitiveSymbol = Print.ppString transitiveSymbolString
and ppUpToDateKeyword = Print.ppString upToDateKeywordString
and ppUpgradableKeyword = Print.ppString upgradableKeywordString
and ppUploadableKeyword = Print.ppString uploadableKeywordString
and ppVersionsKeyword = Print.ppString versionsKeywordString;

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
      | UpToDate => ppUpToDateKeyword
      | OnRemote => ppOnRemoteKeyword
      | IdenticalOnRemote => ppIdenticalOnRemoteKeyword
      | ConsistentWithRemote => ppConsistentWithRemoteKeyword
      | EarlierThanRemote => ppEarlierThanRemoteKeyword
      | LaterThanRemote => ppLaterThanRemoteKeyword
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
      | Versions => ppVersionsKeyword
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
  and consistentWithRemoteKeywordParser =
      exactString consistentWithRemoteKeywordString
  and earlierThanRemoteKeywordParser = exactString earlierThanRemoteKeywordString
  and emptyKeywordParser = exactString emptyKeywordString
  and identicalOnRemoteKeywordParser = exactString identicalOnRemoteKeywordString
  and laterThanRemoteKeywordParser = exactString laterThanRemoteKeywordString
  and mineKeywordParser = exactString mineKeywordString
  and notSymbolParser = exactString notSymbolString
  and onRemoteKeywordParser = exactString onRemoteKeywordString
  and orSymbolParser = exactString orSymbolString
  and upToDateKeywordParser = exactString upToDateKeywordString;

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
      upToDateKeywordParser >> K UpToDate ||
      consistentWithRemoteKeywordParser >> K ConsistentWithRemote ||
      earlierThanRemoteKeywordParser >> K EarlierThanRemote ||
      identicalOnRemoteKeywordParser >> K IdenticalOnRemote ||
      laterThanRemoteKeywordParser >> K LaterThanRemote ||
      onRemoteKeywordParser >> K OnRemote;

  val basicKeywordSpaceParser =
      basicKeywordParser ++ manySpace >> fst;

  val unarySymbolParser =
      notSymbolParser >> K Not;

  val unarySymbolSpaceParser =
      unarySymbolParser ++ manySpace >> fst;

  fun mkInfix (s,p1,p2) =
      if s = andSymbolString then And (p1,p2)
      else if s = orSymbolString then Or (p1,p2)
      else raise Bug "RepositoryQuery.parserPredicate.mkInfix";

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
  and uploadableKeywordParser = exactString uploadableKeywordString
  and versionsKeywordParser = exactString versionsKeywordString;

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
      versionsKeywordParser >> K Versions ||
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
      else raise Bug "RepositoryQuery.parser.mkInfix";

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
