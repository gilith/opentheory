(* ========================================================================= *)
(* EXPORTING THEORY PACKAGES AS HASKELL PACKAGES                             *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Haskell :> Haskell =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val arbitraryTypeTag = "arbitrary-type"
and authorTag = "author"
and buildTypeTag = "build-type"
and cabalVersionTag = "cabal-version"
and categoryTag = "category"
and descriptionTag = "description"
and equalityTypeTag = "equality-type"
and ghcOptionsTag = "ghc-options"
and licenseTag = "license"
and licenseFileTag = "license-file"
and maintainerTag = "maintainer"
and moduleTag = "module"
and nameTag = "name"
and portabilityTag = "portability"
and stabilityTag = "stability"
and synopsisTag = "synopsis"
and versionTag = "version";

(* ------------------------------------------------------------------------- *)
(* File system helper functions.                                             *)
(* ------------------------------------------------------------------------- *)

fun existsDirectory {directory = dir} =
    OS.FileSys.isDir dir
    handle OS.SysErr _ => false;

fun createDirectory {directory = dir} = OS.FileSys.mkDir dir;

fun removeDirectory {directory = dir} = OS.FileSys.rmDir dir;

fun subDirectory {directory = dir} sub =
    let
      val dir = OS.Path.concat (dir,sub)
    in
      {directory = dir}
    end;

fun mkSubDirectory dir sub =
    let
      val dir = subDirectory dir sub

      val () = createDirectory dir
    in
      dir
    end;

local
  fun nukeFile {filename = file} =
      if OS.FileSys.isDir file then nukeDir {directory = file}
      else OS.FileSys.remove file

  and nukeDirFiles dir =
      let
        val filenames = readDirectory dir

        val () = app nukeFile filenames
      in
        ()
      end

  and nukeDir dir =
      let
        val () = nukeDirFiles dir

        val () = removeDirectory dir
      in
        ()
      end;
in
  val nukeDirectoryFiles = nukeDirFiles;
end;

(* ------------------------------------------------------------------------- *)
(* Escaping strings.                                                         *)
(* ------------------------------------------------------------------------- *)

val escapeString =
    let
      fun escape c =
          case c of
            #"\\" => "\\\\"
          | #"\"" => "\\\""
          | #"\n" => "\\n"
          | c => str c
    in
      String.translate escape
    end;

(* ------------------------------------------------------------------------- *)
(* Haskell has a notion of "symbol names".                                   *)
(* See: http://www.haskell.org/onlinereport/lexemes.html                     *)
(* ------------------------------------------------------------------------- *)

val isSymbolChar = Char.contains "!#$%&*+./<=>?@\\^|-~:";

val isSymbolString = CharVector.all isSymbolChar;

fun isSymbolName n = isSymbolString (Name.component n);

(* ------------------------------------------------------------------------- *)
(* Haskell infix tokens.                                                     *)
(* See: http://zvon.org/other/haskell/Outputprelude/index.html               *)
(* ------------------------------------------------------------------------- *)

val infixes =
    Print.Infixes
      [{token = ".", precedence = 9, assoc = Print.RightAssoc},
       (*{token = "!!", precedence = 9, assoc = Print.LeftAssoc},*)
       {token = "^", precedence = 8, assoc = Print.RightAssoc},
       {token = "^^", precedence = 8, assoc = Print.RightAssoc},
       {token = "**", precedence = 8, assoc = Print.RightAssoc},
       {token = "/", precedence = 7, assoc = Print.LeftAssoc},
       {token = "*", precedence = 7, assoc = Print.LeftAssoc},
       {token = "quot", precedence = 7, assoc = Print.LeftAssoc},
       {token = "rem", precedence = 7, assoc = Print.LeftAssoc},
       {token = "div", precedence = 7, assoc = Print.LeftAssoc},
       {token = "mod", precedence = 7, assoc = Print.LeftAssoc},
       {token = "+", precedence = 6, assoc = Print.LeftAssoc},
       {token = "-", precedence = 6, assoc = Print.LeftAssoc},
       {token = ":", precedence = 5, assoc = Print.RightAssoc},
       {token = "++", precedence = 5, assoc = Print.RightAssoc},
       {token = "==", precedence = 4, assoc = Print.NonAssoc},
       {token = "/=", precedence = 4, assoc = Print.NonAssoc},
       {token = "<", precedence = 4, assoc = Print.NonAssoc},
       {token = "<=", precedence = 4, assoc = Print.NonAssoc},
       {token = ">=", precedence = 4, assoc = Print.NonAssoc},
       {token = ">", precedence = 4, assoc = Print.NonAssoc},
       {token = "elem", precedence = 4, assoc = Print.NonAssoc},
       {token = "notElem", precedence = 4, assoc = Print.NonAssoc},
       {token = "&&", precedence = 3, assoc = Print.RightAssoc},
       {token = "||", precedence = 2, assoc = Print.RightAssoc},
       {token = ">>", precedence = 1, assoc = Print.LeftAssoc},
       {token = ">>=", precedence = 1, assoc = Print.LeftAssoc},
       (*{token = "=<<", precedence = 1, assoc = Print.RightAssoc},*)
       {token = "$", precedence = 0, assoc = Print.RightAssoc},
       {token = "$!", precedence = 0, assoc = Print.RightAssoc},
       {token = "seq", precedence = 0, assoc = Print.RightAssoc}];

val infixTokens = Print.tokensInfixes infixes;

(* ------------------------------------------------------------------------- *)
(* Haskell names.                                                            *)
(* ------------------------------------------------------------------------- *)

(* Namespaces *)

val mainNamespace = Namespace.fromList ["Main"]
and testNamespace = Namespace.fromList ["Test","QuickCheck"];

(* Types *)

val boolTypeName = Name.mkGlobal "Bool"
and funTypeName = Name.mkGlobal "->"
and listTypeName = Name.mkGlobal "[]"
and pairTypeName = Name.mkGlobal ",";

(* Constants *)

val eqName = Name.mkGlobal "=="
and leName = Name.mkGlobal "<="
and ltName = Name.mkGlobal "<"
and pairName = Name.mkGlobal ",";

(* Variables *)

val anonymousName = Name.mkGlobal "_";

(* Tests *)

val arbitraryConstName = Name.mk (testNamespace,"arbitrary")
and arbitraryClassName = Name.mk (testNamespace,"Arbitrary");

(* ------------------------------------------------------------------------- *)
(* Haskell syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Interpreting primitive symbols *)

local
  val typeOpRewrites =
      List.map Interpretation.TypeOpRewrite
        [(Name.boolTypeOp,boolTypeName),
         (Name.funTypeOp,funTypeName)];

  val constRewrites =
      List.map Interpretation.ConstRewrite
        [(Name.eqConst,eqName)];
in
  val primitiveRewrites = typeOpRewrites @ constRewrites;

  val primitiveInterpretation =
      Interpretation.fromRewriteList primitiveRewrites;
end;

(* Generalized abstractions *)

val destGenAbs = total Term.destGenAbs;

fun isGenAbs tm = Option.isSome (destGenAbs tm);

local
  fun strip acc tm =
      case destGenAbs tm of
        NONE => (List.rev acc, tm)
      | SOME (v,tm) => strip (v :: acc) tm;
in
  val stripGenAbs = strip [];
end;

(* Lets *)

val destLet = total Term.destLet;

fun isLet tm = Option.isSome (destLet tm);

(* Conditionals *)

val destCond = total Term.destCond;

fun isCond tm = Option.isSome (destCond tm);

(* Numerals *)

val destNumeral = total Term.destNumeral;

fun isNumeral tm = Option.isSome (destNumeral tm);

(* Case expressions *)

val destCase = total Term.destCase;

fun isCase tm = Option.isSome (destCase tm);

fun casePatterns (a,bs) =
    let
      val ty = Term.typeOf a

      fun mkBranch (n,xs,t) =
          let
            val c = Const.mkUndef n

            val ty = Type.listMkFun (List.map Term.typeOf xs, ty)

            val pat = Term.listMkApp (Term.mkConst (c,ty), xs)
          in
            (pat,t)
          end
    in
      (a, List.map mkBranch bs)
    end;

(* ------------------------------------------------------------------------- *)
(* Inferring the set of type operators that must support equality/order.     *)
(* ------------------------------------------------------------------------- *)

local
  val comparisons = NameSet.fromList [eqName,leName,ltName];

  fun destComparison n ty =
      if not (NameSet.member n comparisons) then NONE
      else
        let
          val (tys,_) = Type.stripFun ty
        in
          case tys of
            [_,_] => SOME tys
          | _ => NONE
        end;
in
  fun inferEqualityTypes int c ty =
      let
        val n = Interpretation.interpretConst int (Const.name c)
      in
        case destComparison n ty of
          SOME l => l
        | NONE => []
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Anonymizing unused variables.                                             *)
(* ------------------------------------------------------------------------- *)

fun anonymize tms =
    let
      val fvs = Term.freeVarsList tms

      fun anonVar v =
          if VarSet.member v fvs then NONE
          else SOME (v, Term.mkVar (Var.mk (anonymousName, Var.typeOf v)))
    in
      fn pat =>
         let
           val vl = VarSet.toList (Term.freeVars pat)

           val tmMap = TermSubst.fromListMap (List.mapPartial anonVar vl)

           val sub = TermSubst.mkMono tmMap
         in
           Option.getOpt (TermSubst.subst sub pat, pat)
         end
    end;

(* ------------------------------------------------------------------------- *)
(* Haskell package information.                                              *)
(* ------------------------------------------------------------------------- *)

datatype information =
    Information of
      {name : PackageName.name,
       version : PackageVersion.version,
       description : string,
       author : PackageAuthor.author,
       license : string,
       licenseUrl : string,
       provenance : PackageNameVersion.nameVersion,
       equalityTypes : NameSet.set,
       arbitraryTypes : NameSet.set,
       tags : PackageTag.tag list};

local
  fun destTag tag =
      let
        val PackageTag.Tag' {name = n, value = v} = PackageTag.dest tag
      in
        case PackageName.destHaskellTag n of
          NONE => NONE
        | SOME n =>
          SOME (PackageTag.mk (PackageTag.Tag' {name = n, value = v}))
      end;

  val partitionHaskellTags =
      let
        fun add (tag,(htags,tags)) =
            case destTag tag of
              NONE => (htags, tag :: tags)
            | SOME htag => (htag :: htags, tags)
      in
        fn tags => List.foldl add ([],[]) (List.rev tags)
      end

  fun checkNoTag name htags =
      case PackageTag.filterName name htags of
        [] => ()
      | _ :: _ =>
        let
          val err = "bad haskell-" ^ PackageName.toString name ^ " tag"
        in
          raise Error err
        end;

  fun peekTag name htags =
      case PackageTag.partitionName name htags of
        ([],_) => NONE
      | ([v],htags) => SOME (v,htags)
      | (_ :: _ :: _, _) =>
        let
          val err = "multiple haskell-" ^ PackageName.toString name ^ " tags"
        in
          raise Error err
        end;

  val partitionTags = PackageTag.partitionName;

  fun mkInfo repo pkg tags srcFile htags =
      let
        val (name,htags) =
            let
              val tag = PackageName.fromString nameTag
            in
              case peekTag tag htags of
                SOME (v,htags) => (PackageName.fromString v, htags)
              | NONE =>
                let
                  val n = PackageTag.findName tags
                in
                  (PackageName.mkHaskellName n, htags)
                end
            end

        val version =
            let
              val tag = PackageName.fromString versionTag

              val () = checkNoTag tag htags
            in
              PackageTag.findVersion tags
            end

        val (description,htags) =
            let
              val tag = PackageName.fromString descriptionTag
            in
              case peekTag tag htags of
                SOME (v,htags) => (v,htags)
              | NONE =>
                let
                  val {description} = PackageTag.findDescription tags
                in
                  (description,htags)
                end
            end

        val author =
            let
              val tag = PackageName.fromString authorTag

              val () = checkNoTag tag htags
            in
              PackageTag.findAuthor tags
            end

        val license =
            let
              val tag = PackageName.fromString licenseTag

              val () = checkNoTag tag htags

              val {license} = PackageTag.findLicense tags
            in
              license
            end

        val licenseUrl =
            let
              val license = Repository.getLicense repo {name = license}

              val {url} = RepositoryConfig.urlLicense license
            in
              url
            end

        val provenance =
            let
              val name = PackageTag.findName tags
              and version = PackageTag.findVersion tags
            in
              PackageNameVersion.mk
                (PackageNameVersion.NameVersion'
                   {name = name,
                    version = version})
            end

        val (equalityTypes,htags) =
            let
              val tag = PackageName.fromString equalityTypeTag

              val (tyl,htags) = partitionTags tag htags

              val tyl = List.map Name.quotedFromString tyl

              val tys = NameSet.fromList tyl

              val () =
                  if NameSet.size tys = length tyl then ()
                  else
                    let
                      val err =
                          "duplicate haskell-" ^ equalityTypeTag ^
                          " information"
                    in
                      raise Error err
                    end
            in
              (tys,htags)
            end

        val (arbitraryTypes,htags) =
            let
              val tag = PackageName.fromString arbitraryTypeTag

              val (tyl,htags) = partitionTags tag htags

              val tyl = List.map Name.quotedFromString tyl

              val tys = NameSet.fromList tyl

              val () =
                  if NameSet.size tys = length tyl then ()
                  else
                    let
                      val err =
                          "duplicate haskell-" ^ arbitraryTypeTag ^
                          " information"
                    in
                      raise Error err
                    end
            in
              (tys,htags)
            end

        val (interpretation,htags) =
            case peekTag PackageName.intExtraTag htags of
              NONE => (Interpretation.natural,htags)
            | SOME (filename,htags) =>
              let
                val file = Package.joinDirectory pkg {filename = filename}
              in
                (Interpretation.fromTextFile file, htags)
              end

        val (testFile,htags) =
            case peekTag PackageName.testExtraTag htags of
              NONE => (NONE,htags)
            | SOME (file,htags) => (SOME file, htags)

        val info =
            Information
              {name = name,
               version = version,
               description = description,
               author = author,
               license = license,
               licenseUrl = licenseUrl,
               provenance = provenance,
               equalityTypes = equalityTypes,
               arbitraryTypes = arbitraryTypes,
               tags = htags}
      in
        {information = info,
         srcFilename = srcFile,
         interpretation = interpretation,
         testFilename = testFile}
      end;
in
  fun mkInformation repo pkg =
      let
        val info = Package.information pkg

        val tags = PackageInformation.tags info

        val (htags,tags) = partitionHaskellTags tags
      in
        case peekTag PackageName.srcExtraTag htags of
          NONE => NONE
        | SOME (srcFile,htags) => SOME (mkInfo repo pkg tags srcFile htags)
      end;
end;

fun nameInformation (Information {name = x, ...}) = x;

fun versionInformation (Information {version = x, ...}) = x;

fun licenseUrlInformation (Information {licenseUrl = x, ...}) = {url = x};

fun equalityTypesInformation (Information {equalityTypes = x, ...}) = x;

fun arbitraryTypesInformation (Information {arbitraryTypes = x, ...}) = x;

fun exportable repo nv =
    let
      val pkg =
          case Repository.peek repo nv of
            SOME p => p
          | NONE => raise Bug "Haskell.exportable"
    in
      Option.isSome (mkInformation repo pkg)
    end;

(* ------------------------------------------------------------------------- *)
(* Haskell package dependencies.                                             *)
(* ------------------------------------------------------------------------- *)

datatype depend =
    Depend of
      {name : PackageName.name,
       oldest : PackageVersion.version,
       newest : PackageVersion.version};

datatype packageDependencyRequirements =
    PackageDependencyRequirements of
      {haskellName : PackageName.name,
       definedSymbolNames : Name.name SymbolMap.map,
       equalityTypes : NameSet.set,
       arbitraryTypes : NameSet.set};

local
  fun definesSymbol s th =
      let
        val tab = PackageTheorems.symbol th
      in
        case s of
          Symbol.TypeOp t =>
          let
            val n = TypeOp.name t
          in
            case SymbolTable.peekTypeOp tab n of
              NONE => false
            | SOME t => not (TypeOp.isUndef t)
          end
        | Symbol.Const c =>
          let
            val n = Const.name c
          in
            case SymbolTable.peekConst tab n of
              NONE => false
            | SOME c => not (Const.isUndef c)
          end
      end;

  fun addSymbol ths (s,pm) =
      let
        val th =
            case List.filter (definesSymbol s) ths of
              [] =>
              let
                val (k,n) =
                    case s of
                      Symbol.TypeOp t => ("type operator", TypeOp.name t)
                    | Symbol.Const c => ("constant", Const.name c)

                val err =
                    "no required theory defines " ^ k ^ " with name " ^
                    Name.toString n
              in
                raise Error err
              end
            | [th] => th
            | _ :: _ :: _ =>
              raise Bug "Haskell.mkDepends.addSymbol: multiple definitions";

        val n = PackageNameVersion.name (PackageTheorems.nameVersion th)

        val ss = Option.getOpt (PackageNameMap.peek pm n, SymbolSet.empty)

        val ss = SymbolSet.add ss s
      in
        PackageNameMap.insert pm (n,ss)
      end;

  fun interpretSymbol repo pm (th,(nvs,sym)) =
      let
        val nv = PackageTheorems.nameVersion th

        val n = PackageNameVersion.name nv
      in
        case PackageNameMap.peek pm n of
          NONE => (nvs,sym)
        | SOME ss =>
          let
            val pkg =
                case Repository.peek repo nv of
                  SOME p => p
                | NONE => raise Bug "Haskell.mkDepends.interpretSymbol.pkg"

            val {information = info,
                 srcFilename = _,
                 interpretation = int,
                 testFilename = _} =
                case mkInformation repo pkg of
                  SOME x => x
                | NONE =>
                  let
                    val err =
                        "no haskell-src-file information in required theory " ^
                        PackageNameVersion.toString nv
                  in
                    raise Error err
                  end

            fun interpret s =
                case s of
                  Symbol.TypeOp t =>
                  let
                    val n = TypeOp.name t
                  in
                    Interpretation.interpretTypeOp int n
                  end
                | Symbol.Const c =>
                  let
                    val n = Const.name c
                  in
                    Interpretation.interpretConst int n
                  end

            val hn = nameInformation info

            val sm = SymbolSet.map interpret ss

            val eqs = equalityTypesInformation info
            and arbs = arbitraryTypesInformation info
          in
            (nv :: nvs, PackageNameMap.insert sym (n,(hn,sm,(nv,eqs,arbs))))
          end
      end;

  fun interpretationSymbol thyInt sym =
      let
        fun addSym (s,n,rws) =
            let
              val rw =
                  case s of
                    Symbol.TypeOp t =>
                    Interpretation.TypeOpRewrite (TypeOp.name t, n)
                  | Symbol.Const c =>
                    Interpretation.ConstRewrite (Const.name c, n)
            in
              rw :: rws
            end

        fun addName (_,(_,sm,_),rws) = SymbolMap.foldr addSym rws sm

        val rewrs =
            primitiveRewrites @
            PackageNameMap.foldr addName [] sym @
            Interpretation.toRewriteList thyInt
      in
        Interpretation.fromRewriteList rewrs
      end;

  val classSymbol =
      let
        fun addClass req (s,_,ns) =
            case s of
              Symbol.Const c => ns
            | Symbol.TypeOp t =>
              let
                val n = TypeOp.name t
              in
                if NameSet.member n req then NameSet.add ns n else ns
              end

        fun mkClass tag req sm nv decl =
            let
              val nvreq = SymbolMap.foldl (addClass req) NameSet.empty sm

              val () =
                  case NameSet.toList (NameSet.difference nvreq decl) of
                    [] => ()
                  | nl =>
                    let
                      fun n2s n = "\n  " ^ Name.quotedToString n

                      val err =
                          "in required theory " ^
                          PackageNameVersion.toString nv ^
                          " the following types need to be declared as " ^
                          "haskell-" ^ tag ^ String.concat (List.map n2s nl)
                    in
                      raise Error err
                    end
            in
              nvreq
            end

        fun mkClasses reqEqs reqArbs (n,sm,(nv,nveqs,nvarbs)) =
            let
              val eqs = mkClass equalityTypeTag reqEqs sm nv nveqs
              and arbs = mkClass arbitraryTypeTag reqArbs sm nv nvarbs
            in
              PackageDependencyRequirements
                {haskellName = n,
                 definedSymbolNames = sm,
                 equalityTypes = eqs,
                 arbitraryTypes = arbs}
            end
      in
        fn reqEqs => fn reqArbs =>
        PackageNameMap.transform (mkClasses reqEqs reqArbs)
      end;

  fun mkSymbol repo thyInt ths sym reqEqs reqArbs =
      let
        val sym =
            SymbolSet.difference
              (SymbolTable.symbols (SymbolTable.undefined sym))
              SymbolSet.primitives

        val pm = PackageNameMap.new ()

        val pm = SymbolSet.foldl (addSymbol ths) pm sym

        val sym = PackageNameMap.new ()

        val (nvs,sym) =
            List.foldl (interpretSymbol repo pm) ([],sym) (List.rev ths)

        val int = interpretationSymbol thyInt sym

        val sym = classSymbol (reqEqs int) reqArbs sym
      in
        (nvs,sym,int)
      end;

  fun checkSymbol sym th info int =
      let
        val tab = PackageTheorems.symbol th

        fun defTypeOp n =
            case SymbolTable.peekTypeOp tab n of
              NONE => NONE
            | SOME t =>
              if TypeOp.isUndef t then NONE
              else SOME (Interpretation.interpretTypeOp int (TypeOp.name t))

        fun defConst n =
            case SymbolTable.peekConst tab n of
              NONE => NONE
            | SOME c =>
              if Const.isUndef c then NONE
              else SOME (Interpretation.interpretConst int (Const.name c))

        fun check (s,n) =
            let
              val no =
                  case s of
                    Symbol.TypeOp t => defTypeOp (TypeOp.name t)
                  | Symbol.Const c => defConst (Const.name c)
            in
              case no of
                NONE => false
              | SOME n' => Name.equal n' n
            end

        val nv = PackageTheorems.nameVersion th

        val n = PackageNameVersion.name nv

        val PackageDependencyRequirements
              {haskellName,
               definedSymbolNames,
               equalityTypes,
               arbitraryTypes} =
            case PackageNameMap.peek sym n of
              NONE => raise Bug "Haskell.mkDepends.checkSymbol"
            | SOME x => x
      in
        PackageName.equal (nameInformation info) haskellName andalso
        SymbolMap.all check definedSymbolNames andalso
        NameSet.subset equalityTypes (equalityTypesInformation info) andalso
        NameSet.subset arbitraryTypes (arbitraryTypesInformation info)
      end;

  fun recordOldest oldest nv =
      let
        val n = PackageNameVersion.name nv
        and v = PackageNameVersion.version nv

(*OpenTheoryDebug
        val () =
            if not (PackageNameMap.inDomain n oldest) then ()
            else raise Bug "Haskell.mkDepends.recordOldest"
*)
      in
        PackageNameMap.insert oldest (n,v)
      end;

  fun mkOldest repo {previousVersion} sym =
      let
        fun check nv vs =
            case previousVersion nv of
              NONE => NONE
            | SOME nv =>
              let
                val pkg = Repository.get repo nv

                val th = Package.theorems pkg
              in
                (* If Haskell export was not set up for this version *)
                (* then we stop checking this package *)
                case mkInformation repo pkg of
                  NONE => NONE
                | SOME info_int =>
                  let
                    val {information = info,
                         srcFilename = _,
                         interpretation = int,
                         testFilename = _} = info_int
                  in
                    case total (PackageTheorems.addVersion vs) th of
                      NONE => NONE
                    | SOME vs =>
                      if not (checkSymbol sym th info int) then NONE
                      else SOME (nv,vs)
                  end
              end

        fun push oldest nvs vs =
            if Queue.null nvs then oldest
            else
              let
                val (nv,nvs) = Queue.hdTl nvs
              in
                case check nv vs of
                  NONE =>
                  let
                    val oldest = recordOldest oldest nv
                  in
                    push oldest nvs vs
                  end
                | SOME (nv,vs) =>
                  let
                    val nvs = Queue.add nv nvs
                  in
                    push oldest nvs vs
                  end
              end
      in
        push (PackageNameMap.new ())
      end;

  fun destOldest sym oldest =
      let
        fun mk nv =
            let
              val n = PackageNameVersion.name nv
              and new = PackageNameVersion.version nv

              val PackageDependencyRequirements {haskellName,...} =
                  case PackageNameMap.peek sym n of
                    SOME x => x
                  | NONE => raise Bug "Haskell.mkDepends.destOldest.mk: sym"

              val old =
                  case PackageNameMap.peek oldest n of
                    SOME x => x
                  | NONE => raise Bug "Haskell.mkDepends.destOldest.mk: old"
            in
              Depend
                {name = haskellName,
                 oldest = old,
                 newest = new}
            end
      in
        List.map mk
      end;
in
  fun mkDepends repo prev reqs thyInt thy sym reqEqs reqArbs =
      let
        val ths =
            case Repository.requiresTheorems repo reqs of
              SOME ths => ths
            | NONE => raise Error "required theories not installed"

        val vs = PackageTheorems.mkVersions (Theory.summary thy) ths

        val (nvs,sym,int) = mkSymbol repo thyInt ths sym reqEqs reqArbs

        val oldest = mkOldest repo prev sym (Queue.fromList nvs) vs

        val deps = destOldest sym oldest nvs
      in
        (deps,int)
      end;
end;

(* ------------------------------------------------------------------------- *)
(* A type of source declarations.                                            *)
(* ------------------------------------------------------------------------- *)

datatype data =
    Data of
      {name : TypeOp.typeOp,
       parameters : Name.name list,
       constructors : (Const.const * Type.ty list) list,
       caseConst : Const.const,
       equalityType : bool,
       arbitraryType : bool};

datatype newtype =
    Newtype of
      {name : TypeOp.typeOp,
       parameters : Name.name list,
       repType : Type.ty,
       abs : Const.const,
       rep : Const.const,
       equalityType : bool,
       arbitraryType : bool};

datatype whereValue =
    WhereValue of
      {name : Var.var,
       equations : equation list}

and equation =
    Equation of
      {arguments : Term.term list,
       body : Term.term,
       whereValues : whereValue list}

datatype value =
    Value of
      {name : Const.const,
       ty : Type.ty,
       equations : equation list};

datatype arbitrary =
    Arbitrary of
      {name : TypeOp.typeOp,
       parameters : Name.name list,
       lift : Term.term};

datatype source =
    DataSource of data
  | NewtypeSource of newtype
  | ValueSource of value
  | ArbitrarySource of arbitrary;

(* ------------------------------------------------------------------------- *)
(* Symbols in source declarations.                                           *)
(* ------------------------------------------------------------------------- *)

fun destArbitrarySource src =
    case src of
      ArbitrarySource x => SOME x
    | _ => NONE;

fun isArbitrarySource src = Option.isSome (destArbitrarySource src);

fun typeOpSource s =
    case s of
      DataSource (Data {name,...}) => SOME name
    | NewtypeSource (Newtype {name,...}) => SOME name
    | ValueSource _ => NONE
    | ArbitrarySource _ => NONE;

fun findTypeOpSourceList n =
    let
      fun find s =
          case typeOpSource s of
            NONE => NONE
          | SOME t => if Name.equal (TypeOp.name t) n then SOME s else NONE
    in
      first find
    end;

(* All defined symbols in source declarations *)

local
  fun addConstructor ((c,tys),sym) =
      let
        val sym = SymbolTable.addConst sym c
      in
        sym
      end;
in
  fun definedSymbolTableData data =
      let
        val Data
              {name,
               parameters = _,
               constructors = cons,
               caseConst,
               equalityType = _,
               arbitraryType = _} = data

        val sym = SymbolTable.empty

        val sym = SymbolTable.addTypeOp sym name

        val sym = List.foldl addConstructor sym cons

        val sym = SymbolTable.addConst sym caseConst
      in
        sym
      end;
end;

fun definedSymbolTableNewtype newtype =
    let
      val Newtype
            {name,
             parameters = _,
             repType = _,
             abs,
             rep,
             equalityType = _,
             arbitraryType = _} = newtype

      val sym = SymbolTable.empty

      val sym = SymbolTable.addTypeOp sym name

      val sym = SymbolTable.addConst sym abs

      val sym = SymbolTable.addConst sym rep
    in
      sym
    end;

fun definedSymbolTableValue value =
    let
      val Value {name,...} = value

      val sym = SymbolTable.empty

      val sym = SymbolTable.addConst sym name
    in
      sym
    end;

fun definedSymbolTableArbitrary (_ : arbitrary) =
    let
      val sym = SymbolTable.empty
    in
      sym
    end;

fun definedSymbolTableSource s =
    case s of
      DataSource x => definedSymbolTableData x
    | NewtypeSource x => definedSymbolTableNewtype x
    | ValueSource x => definedSymbolTableValue x
    | ArbitrarySource x => definedSymbolTableArbitrary x;

fun definedSymbolTableSourceList sl =
    SymbolTable.unionList (List.map definedSymbolTableSource sl);

(* All symbols in source declarations *)

local
  fun addConstructor ((c,tys),sym) =
      let
        val sym = SymbolTable.addConst sym c

        val sym = SymbolTable.addTypeList sym tys;
      in
        sym
      end;
in
  fun symbolTableData data =
      let
        val Data
              {name,
               parameters = _,
               constructors = cons,
               caseConst,
               equalityType = _,
               arbitraryType = _} = data

        val sym = SymbolTable.empty

        val sym = SymbolTable.addTypeOp sym name

        val sym = List.foldl addConstructor sym cons

        val sym = SymbolTable.addConst sym caseConst
      in
        sym
      end;
end;

fun symbolTableNewtype newtype =
    let
      val Newtype
            {name,
             parameters = _,
             repType,
             abs,
             rep,
             equalityType = _,
             arbitraryType = _} = newtype

      val sym = SymbolTable.empty

      val sym = SymbolTable.addTypeOp sym name

      val sym = SymbolTable.addType sym repType

      val sym = SymbolTable.addConst sym abs

      val sym = SymbolTable.addConst sym rep
    in
      sym
    end;

local
  fun addValue (value,sym) =
      let
        val WhereValue {name, equations = eqns} = value

        val sym = SymbolTable.addVar sym name

        val sym = List.foldl addEquation sym eqns
      in
        sym
      end

  and addEquation (eqn,sym) =
      let
        val Equation {arguments = args, body = tm, whereValues = values} = eqn

        val sym = SymbolTable.addTermList sym args

        val sym = SymbolTable.addTerm sym tm

        val sym = List.foldl addValue sym values
      in
        sym
      end;
in
  fun addSymbolTableWhereValue sym value = addValue (value,sym);

  fun addSymbolTableWhereValues sym values = List.foldl addValue sym values;

  fun addSymbolTableEquation sym eqn = addEquation (eqn,sym);

  fun addSymbolTableValue sym value =
      let
        val Value {name, ty, equations = eqns} = value

        val sym = SymbolTable.addConst sym name

        val sym = SymbolTable.addType sym ty

        val sym = List.foldl addEquation sym eqns
      in
        sym
      end;
end;

val symbolTableEquation = addSymbolTableEquation SymbolTable.empty;

val symbolTableValue = addSymbolTableValue SymbolTable.empty;

fun symbolTableArbitrary arbitrary =
    let
      val Arbitrary {name = _, parameters = _, lift} = arbitrary

      val sym = SymbolTable.empty

      val sym = SymbolTable.addTerm sym lift
    in
      sym
    end;

fun symbolTableSource s =
    case s of
      DataSource x => symbolTableData x
    | NewtypeSource x => symbolTableNewtype x
    | ValueSource x => symbolTableValue x
    | ArbitrarySource x => symbolTableArbitrary x;

fun symbolTableSourceList sl =
    SymbolTable.unionList (List.map symbolTableSource sl);

(* All symbols that explicitly appear in source declarations. This excludes *)
(* type signatures of "where" values (these are commented out in the *)
(* generated Haskell code), but includes case constants (these are *)
(* transformed into constructors later). *)

val explicitSymbolTableData = symbolTableData;

val explicitSymbolTableNewtype = symbolTableNewtype;

local
  fun destSpecialTerm tm =
      if isNumeral tm then SOME []
      else
        case destCond tm of
          SOME (c,a,b) => SOME [c,a,b]
        | NONE =>
          case destLet tm of
            SOME (v,x,y) => SOME [v,x,y]
          | NONE =>
            case destGenAbs tm of
              SOME (v,x) => SOME [v,x]
            | NONE => NONE;

  fun addTerm (tm,sym) =
      case destSpecialTerm tm of
        SOME tms => List.foldl addTerm sym tms
      | NONE =>
        case Term.dest tm of
          TypeTerm.Const' (c,_) => SymbolTable.addConst sym c
        | TypeTerm.Var' _ => sym
        | TypeTerm.App' (f,x) => addTerm (f, addTerm (x,sym))
        | TypeTerm.Abs' (_,b) => addTerm (b,sym);

  fun addValue (value,sym) =
      let
        val WhereValue {name = _, equations = eqns} = value

        val sym = List.foldl addEquation sym eqns
      in
        sym
      end

  and addEquation (eqn,sym) =
      let
        val Equation {arguments = args, body, whereValues = values} = eqn

        val sym = List.foldl addTerm sym args

        val sym = addTerm (body,sym)

        val sym = List.foldl addValue sym values
      in
        sym
      end;
in
  fun explicitSymbolTableValue value =
      let
        val Value {name, ty, equations = eqns} = value

        val sym = SymbolTable.empty

        val sym = SymbolTable.addConst sym name

        val sym = SymbolTable.addType sym ty

        val sym = List.foldl addEquation sym eqns
      in
        sym
      end;

  fun explicitSymbolTableArbitrary arbitrary =
      let
        val Arbitrary {name, parameters = _, lift} = arbitrary

        val sym = SymbolTable.empty

        val sym = addTerm (lift,sym)
      in
        sym
      end;
end;

fun explicitSymbolTableSource s =
    case s of
      DataSource x => explicitSymbolTableData x
    | NewtypeSource x => explicitSymbolTableNewtype x
    | ValueSource x => explicitSymbolTableValue x
    | ArbitrarySource x => explicitSymbolTableArbitrary x;

fun explicitSymbolTableSourceList sl =
    SymbolTable.unionList (List.map explicitSymbolTableSource sl);

(* ------------------------------------------------------------------------- *)
(* Rewriting Haskell declarations.                                           *)
(* ------------------------------------------------------------------------- *)

fun sharingRewriteEquation eqn rewr =
    let
      val Equation {arguments = args, body, whereValues = values} = eqn

      val (args',rewr) = TermRewrite.sharingRewriteTermList args rewr

      val (body',rewr) = TermRewrite.sharingRewriteTerm body rewr

      val (values',rewr) = sharingRewriteWhereValueList values rewr

      val eqn' =
          case (args',body',values') of
            (NONE,NONE,NONE) => NONE
          | _ =>
            let
              val args = Option.getOpt (args',args)
              and body = Option.getOpt (body',body)
              and values = Option.getOpt (values',values)

              val eqn =
                  Equation
                    {arguments = args,
                     body = body,
                     whereValues = values}
            in
              SOME eqn
            end
    in
      (eqn',rewr)
    end

and sharingRewriteEquationList eqns rewr =
    TermRewrite.sharingRewriteList sharingRewriteEquation eqns rewr

and sharingRewriteWhereValue value rewr =
    let
      val WhereValue {name, equations = eqns} = value

      val (name',rewr) = TermRewrite.sharingRewriteVar name rewr

      val (eqns',rewr) = sharingRewriteEquationList eqns rewr

      val value' =
          case (name',eqns') of
            (NONE,NONE) => NONE
          | _ =>
            let
              val name = Option.getOpt (name',name)
              and eqns = Option.getOpt (eqns',eqns)

              val value =
                  WhereValue
                    {name = name,
                     equations = eqns}
            in
              SOME value
            end
    in
      (value',rewr)
    end

and sharingRewriteWhereValueList values rewr =
    TermRewrite.sharingRewriteList sharingRewriteWhereValue values rewr;

(* ------------------------------------------------------------------------- *)
(* Converting theorems into source declarations.                             *)
(* ------------------------------------------------------------------------- *)

fun mkData eqs arbs th =
    let
      val Sequent.Sequent {hyp,concl} = Thm.sequent th

      val () =
          if TermAlphaSet.null hyp then ()
          else raise Error "hypotheses"

      val {dataType,constructors,caseConst} = Term.destCaseDef concl

      val (name,parms) = Type.destOp dataType

      val parms = List.map Type.destVar parms

      val equalityType = NameSet.member (TypeOp.name name) eqs
      and arbitraryType = NameSet.member (TypeOp.name name) arbs
    in
      Data
        {name = name,
         parameters = parms,
         constructors = constructors,
         caseConst = caseConst,
         equalityType = equalityType,
         arbitraryType = arbitraryType}
    end
    handle Error err =>
      raise Error ("bad data theorem: " ^ err);

fun mkNewtype eqs arbs int th =
    let
      val Sequent.Sequent {hyp,concl} = Thm.sequent th

      val () =
          if TermAlphaSet.null hyp then ()
          else raise Error "hypotheses"

      val (abs,rep) =
          let
            val (a,t0) = Term.destForall concl

            val (t1,t2) = Term.destEq t0

            val () =
                if Term.equalVar a t2 then ()
                else raise Error "bad rhs of absRep"

            val (abs,t3) = Term.destApp t1

            val (rep,t4) = Term.destApp t3

            val () =
                if Term.equalVar a t4 then ()
                else raise Error "bad var inside absRep"
          in
            (abs,rep)
          end

      val (abs,repAbsTy) = Term.destConst abs
      and (rep,_) = Term.destConst rep

      val (repTy,absTy) = Type.destFun repAbsTy

      val (name,parms) = Type.destOp absTy

      val parms = List.map Type.destVar parms

      val () =
          let
            val tn = Interpretation.interpretTypeOp int (TypeOp.name name)
            and cn = Interpretation.interpretConst int (Const.name abs)
          in
            if Name.equal cn tn then ()
            else raise Error "constructor name does not match type"
          end

      val equalityType = NameSet.member (TypeOp.name name) eqs
      and arbitraryType = NameSet.member (TypeOp.name name) arbs
    in
      Newtype
        {name = name,
         parameters = parms,
         repType = repTy,
         abs = abs,
         rep = rep,
         equalityType = equalityType,
         arbitraryType = arbitraryType}
    end
    handle Error err =>
      raise Error ("bad newtype theorem: " ^ err);

local
  fun mkEqn tm =
      let
        val (_,tm) = Term.stripForall tm

        val (l,r) = Term.destEq tm

        val (f,a) = Term.stripApp l

        val arity = length a

        val eqn =
            Equation
              {arguments = a,
               body = r,
               whereValues = []}
      in
        ((f,arity),eqn)
      end;
in
  fun mkValue th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val (fns,eqns) = unzip (List.map mkEqn (Term.stripConj concl))

        val (name,ty) =
            case fns of
              [] => raise Error "no equations"
            | (f,n) :: fns =>
              let
                fun eq (f',n') = Term.equal f f' andalso n = n'

                val () =
                    if List.all eq fns then ()
                    else raise Error "different const/arity"
              in
                Term.destConst f
              end
      in
        Value
          {name = name,
           ty = ty,
           equations = eqns}
      end
      handle Error err =>
        raise Error ("bad value theorem: " ^ err);
end;

local
  fun mkEqn tm =
      let
        val (_,tm) = Term.stripForall tm

        val (l,r) = Term.destEq tm

        val (f,a) = Term.stripApp l

        val arity = length a

        val eqn =
            Equation
              {arguments = a,
               body = r,
               whereValues = []}
      in
        ((f,arity),eqn)
      end;
in
  fun mkArbitrary th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val lift = Term.destSurjective concl

        val ty = Type.rangeFun (Term.typeOf lift)

        val (name,tys) = Type.destOp ty

        val parameters = List.map Type.destVar tys
      in
        Arbitrary
          {name = name,
           parameters = parameters,
           lift = lift}
      end
      handle Error err =>
        raise Error ("bad arbitrary theorem: " ^ err);
end;

fun mkSource eqs arbs int th =
    let
      val dataResult =
          Left (mkData eqs arbs th)
          handle Error err => Right err

      val newtypeResult =
          Left (mkNewtype eqs arbs int th)
          handle Error err => Right err

      val valueResult =
          Left (mkValue th)
          handle Error err => Right err

      val arbitraryResult =
          Left (mkArbitrary th)
          handle Error err => Right err
    in
      case (dataResult,newtypeResult,valueResult,arbitraryResult) of
        (Left x, _, _, _) => DataSource x
      | (Right _, Left x, _, _) => NewtypeSource x
      | (Right _, Right _, Left x, _) => ValueSource x
      | (Right _, Right _, Right _, Left x) => ArbitrarySource x
      | (Right e1, Right e2, Right e3, Right e4) =>
        let
          val err =
              "bad source theorem:\n  " ^
              e1 ^ "\n  " ^ e2 ^ "\n  " ^ e3 ^ "\n  " ^ e4 ^ "\n" ^
              Print.toString Thm.pp th
        in
          raise Error err
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Sorting source declarations into a module hierarchy.                      *)
(* ------------------------------------------------------------------------- *)

datatype module =
    Module of
      {namespace : Namespace.namespace,
       source : source list,
       submodules : module list};

fun foldModule f =
    let
      fun g ns (s,acc) = f (ns,s,acc)

      fun foldM (module,acc) =
          let
            val Module {namespace,source,submodules} = module

            val acc = List.foldl foldM acc submodules

            val acc = List.foldl (g namespace) acc source
          in
            acc
          end
    in
      fn acc => fn module => foldM (module,acc)
    end;

fun symbolTableModule module =
    let
      val Module {namespace = _, source, submodules} = module

      val sym = symbolTableSourceList source

      val syml = List.map symbolTableModule submodules
    in
      SymbolTable.unionList (sym :: syml)
    end;

fun findTypeDeclarationModule module n =
    let
      fun find m =
          let
            val Module {namespace = _, source = sl, submodules = ml} = m
          in
            case findTypeOpSourceList n sl of
              NONE => first find ml
            | s => s
          end
    in
      find module
    end;

local
  val rightToLeftVars =
      let
        fun extract acc tms =
            case tms of
              [] => acc
            | tm :: tms =>
              case Term.dest tm of
                TypeTerm.Const' _ => extract acc tms
              | TypeTerm.Var' v => extract (v :: acc) tms
              | TypeTerm.App' (f,x) => extract acc (f :: x :: tms)
              | TypeTerm.Abs' _ =>
                raise Error "lambda abstraction in equation pattern"
      in
        extract []
      end;

  fun mkEqn eqn =
      let
        val sym = symbolTableEquation eqn
      in
        (sym,eqn,[])
      end;

  fun destEqn (_,eqn,subs) =
      let
        val Equation {arguments = args, body, whereValues = values} = eqn

        val revArgVars = rightToLeftVars args

        val argVars = List.rev revArgVars

        fun addRewrite ((wherename,value),(values,rewr)) =
            let
              val Value {name, ty, equations = eqns} = value

              fun isValue vs tm' =
                  case vs of
                    [] =>
                    (case tm' of
                       TypeTerm.Const' (c,_) => Const.equal c name
                     | _ => false)
                  | v :: vs =>
                    (case tm' of
                       TypeTerm.App' (f,x) =>
                       Term.equalVar v x andalso isValue vs (Term.dest f)
                     | _ => false)

              fun mkVar t = Var.mk (wherename,t)

              val rewr =
                  fn tm =>
                     if not (isValue revArgVars tm) then rewr tm
                     else SOME (Term.mkVar (mkVar (TypeTerm.typeOf' tm)))

              val name =
                  let
                    fun stripType vs t =
                        case vs of
                          [] => t
                        | v :: vs =>
                          let
                            val (d,t) = Type.destFun t

                            val () =
                                if Type.equal d (Var.typeOf v) then ()
                                else raise Error "bad type in \"where\" value"
                          in
                            stripType vs t
                          end
                  in
                    mkVar (stripType argVars ty)
                  end

              val eqns =
                  let
                    fun stripArgs vs tms =
                        case vs of
                          [] => tms
                        | v :: vs =>
                          case tms of
                            [] =>
                            let
                              val err = "missing arguments in \"where\" value"
                            in
                              raise Error err
                            end
                          | tm :: tms =>
                            if Term.equalVar v tm then stripArgs vs tms
                            else
                              let
                                val err = "bad argument in \"where\" value"
                              in
                                raise Error err
                              end

                    fun stripEqn eqn =
                        let
                          val Equation
                                {arguments = args,
                                 body,
                                 whereValues = values} = eqn

                          val args = stripArgs argVars args
                        in
                          Equation
                            {arguments = args,
                             body = body,
                             whereValues = values}
                        end
                  in
                    List.map stripEqn eqns
                  end

              val values = WhereValue {name = name, equations = eqns} :: values
            in
              (values,rewr)
            end

        val rewr = K NONE

        val (values,rewr) = List.foldl addRewrite (values,rewr) (List.rev subs)

        val rewr = TermRewrite.new TypeRewrite.id rewr

        val eqn =
            Equation
              {arguments = args,
               body = body,
               whereValues = values}

        val (eqn',_) = sharingRewriteEquation eqn rewr
      in
        Option.getOpt (eqn',eqn)
      end;

  fun inEqn name (sym,_,_) = SymbolTable.knownConst sym name;

  fun notinEqn name eqn = not (inEqn name eqn);

  fun addEqn value (sym,eqn,subs) =
      let
        val sym = addSymbolTableValue sym (snd value)
        and subs = subs @ [value]
      in
        (sym,eqn,subs)
      end;

  fun pullValues int (src,(values,others)) =
      case src of
        ValueSource value =>
        let
          val Value {name,...} = value

          val name = Interpretation.interpretConst int (Const.name name)

          val values = NameMap.insert values (name,(value,[]))
        in
          (values,others)
        end
      | _ =>
        let
          val others = src :: others
        in
          (values,others)
        end;

  fun checkValue eqns (_,value) =
      let
        val Value {name, ty = _, equations = _} = value

        val name = Const.name name
      in
        case List.filter (inEqn name) eqns of
          [] => raise Bug "Haskell.groupSource.checkValue"
        | [_] => ()
        | _ :: _ :: _ =>
          let
            val err = "ambiguous nested value " ^ Name.toString name
          in
            raise Error err
          end
      end;

  fun addValue value =
      let
        val (_, Value {name, ty = _, equations = _}) = value

        val name = Const.name name

        fun findEqn revEqns eqns =
            case eqns of
              [] => NONE
            | eqn :: eqns =>
              if notinEqn name eqn then findEqn (eqn :: revEqns) eqns
              else
                let
                  val eqn = addEqn value eqn
                in
                  SOME (List.revAppend (revEqns, eqn :: eqns))
                end
      in
        findEqn []
      end;

  val addValues =
      let
        fun add revValues values eqns =
            case values of
              [] => (List.rev revValues, eqns)
            | value :: values =>
              case addValue value eqns of
                NONE => add (value :: revValues) values eqns
              | SOME eqns => add revValues values eqns

        fun repeat values eqns =
            let
              val (values',eqns) = add [] values eqns
            in
              case values' of
                [] => eqns
              | (_,value) :: _ =>
                if List.length values' < List.length values then
                  repeat values' eqns
                else
                  let
                    val Value {name, ty = _, equations = _} = value

                    val name = Const.name name

                    val err = "unused nested value " ^ Name.toString name
                  in
                    raise Error err
                  end
            end

        fun repeatCheck values eqns =
            let
              val eqns = List.map mkEqn eqns

              val eqns = repeat values eqns

              val () = List.app (checkValue eqns) values
            in
              List.map destEqn eqns
            end
      in
        repeatCheck
      end;

  fun mergeSubvalues (value,subvalues) =
      let
        val Value {name, ty, equations = eqns} = value

        val eqns = addValues subvalues eqns

        val value = Value {name = name, ty = ty, equations = eqns}
      in
        value
      end;

  fun groupValues (name,_,(values,acc)) =
      let
        val value = mergeSubvalues (NameMap.get values name)

        val name' = Name.fromNamespace (Name.namespace name)
      in
        case NameMap.peek values name' of
          NONE =>
          let
            val acc = value :: acc
          in
            (values,acc)
          end
        | SOME (value',subvalues') =>
          let
            val wherename = Name.mkGlobal (Name.component name)

            val subvalues' = (wherename,value) :: subvalues'

            val values = NameMap.insert values (name',(value',subvalues'))
          in
            (values,acc)
          end
      end;
in
  fun groupSource int src =
      let
        val values = NameMap.new ()

        val (values,src) =
            List.foldl (pullValues int) (values,[]) (List.rev src)

        val (_,values) = NameMap.foldr groupValues (values,[]) values
      in
        src @ List.map ValueSource values
      end;
end;

local
  val mkSyms =
      let
        fun addT t src symsrc = SymbolMap.insert symsrc (Symbol.TypeOp t, src);

        fun addC c src symsrc = SymbolMap.insert symsrc (Symbol.Const c, src);

        fun inc (src,(defs,arbs)) =
            case src of
              DataSource (Data {name,...}) => (addT name src defs, arbs)
            | NewtypeSource (Newtype {name,...}) => (addT name src defs, arbs)
            | ValueSource (Value {name,...}) => (addC name src defs, arbs)
            | ArbitrarySource (Arbitrary {name,...}) =>
              (defs, addT name src arbs);
      in
        List.foldl inc (SymbolMap.new (), SymbolMap.new ())
      end;

  fun mkGraph syms =
      let
        fun add (sym,src,graph) =
            let
              val ss = SymbolTable.symbols (symbolTableSource src)

              val ss = SymbolSet.intersect syms ss
            in
              SymbolGraph.addChildren graph (sym,ss)
            end
      in
        SymbolMap.foldl add SymbolGraph.empty
      end;

  fun linearize symSrc =
      let
        fun addSym (sym,acc) =
            case SymbolMap.peek symSrc sym of
              SOME src => src :: acc
            | NONE => raise Bug "Haskell.sortSource.linearize.addSym"

        fun addScc (scc,acc) = SymbolSet.foldr addSym acc scc

        val syms = SymbolSet.domain symSrc

        val graph = mkGraph syms symSrc

        val sccl = SymbolGraph.preOrderSCC graph (SymbolGraph.vertices graph)
      in
        List.foldl addScc [] sccl
      end;
in
  fun sortSource srcl =
      let
        val (defs,arbs) = mkSyms srcl
      in
        linearize defs @ linearize arbs
      end;
end;

local
  fun init int src =
      let
        fun intT t = Interpretation.interpretTypeOp int (TypeOp.name t)

        fun intC c = Interpretation.interpretConst int (Const.name c)

        val n =
            case src of
              DataSource (Data {name,...}) => intT name
            | NewtypeSource (Newtype {name,...}) => intT name
            | ValueSource (Value {name,...}) => intC name
            | ArbitrarySource (Arbitrary {name,...}) => intT name

        val ns = Namespace.toList (Name.namespace n)
      in
        (ns,src)
      end;

  fun split ((ns,src),(acc,nsubs)) =
      case ns of
        [] => (src :: acc, nsubs)
      | n :: ns => (acc, (n,(ns,src)) :: nsubs);

  fun group nsubs =
      case nsubs of
        [] => []
      | (n,sub) :: nsubs =>
        let
          val (ns,nsubs) = List.partition (equal n o fst) nsubs
        in
          (n, sub :: List.map snd ns) :: group nsubs
        end;

  fun mkTree namespace nsource =
      let
        val (source,nsubs) = List.foldl split ([],[]) (List.rev nsource)

        val nsubs = group nsubs
      in
        Module
          {namespace = namespace,
           source = source,
           submodules = List.map (addTree namespace) nsubs}
      end

  and addTree parent (n,nsource) =
      let
        val namespace = Namespace.mkNested (parent,n)
      in
        mkTree namespace nsource
      end;
in
  fun mkModule int source =
      let
        val namespace = Namespace.global
        and source = List.map (init int) source
      in
        mkTree namespace source
      end;
end;

fun mkSourceModule eqs arbs int src =
    let
      val ths = ThmSet.toList (Thms.thms src)

      val src = List.map (mkSource eqs arbs int) ths

      val src = groupSource int src

      val src = sortSource src
    in
      mkModule int src
    end;

local
  fun exposed (module,acc) =
      let
        val Module {namespace,source,submodules} = module

        val acc =
            if List.null source then acc
            else NamespaceSet.add acc namespace
      in
        List.foldl exposed acc submodules
      end;
in
  fun exposedModule source = exposed (source,NamespaceSet.empty);
end;

(* ------------------------------------------------------------------------- *)
(* Converting theorems into test declarations.                               *)
(* ------------------------------------------------------------------------- *)

datatype test =
    Test of
      {name : string,
       description : string,
       value : value,
       invocation : string};

val sourceTests =
    let
      fun mk (Test {value,...}) = ValueSource value
    in
      List.map mk
    end;

fun symbolTableTests tests = symbolTableSourceList (sourceTests tests);

fun mkTests show =
    let
      fun substVar v sub =
          let
            val (v',sub) = TermSubst.sharingSubstVar v sub
          in
            (Option.getOpt (v',v), sub)
          end;

      fun substTerm tm sub =
          let
            val (tm',sub) = TermSubst.sharingSubst tm sub
          in
            (Option.getOpt (tm',tm), sub)
          end;

      fun ppTest (kind,n,test) =
          Print.inconsistentBlock 2
            [Print.ppString (capitalize kind),
             Print.ppString " ",
             Print.ppInt n,
             Print.ppString ":",
             Print.newline,
             Term.ppWithShow show test]

      fun mkTest al pl th =
          let
            val Sequent.Sequent {hyp,concl} = Thm.sequent th

            val () =
                if TermAlphaSet.null hyp then ()
                else raise Error "hypotheses"

            val () =
                if VarSet.null (Term.freeVars concl) then ()
                else raise Error "free variables"

            val (vs,body) = Term.stripForall concl

            val sub =
                let
                  fun addTy (n,s) = TypeSubst.insertMap s (n,Type.bool)

                  val tys = Var.typeVarsList vs

                  val tyMap = NameSet.foldl addTy TypeSubst.emptyMap tys

                  val tySub = TypeSubst.mk tyMap
                in
                  TermSubst.mk tySub TermSubst.emptyMap
                end

            val (vs,sub) = maps substVar vs sub

            val (body,sub) = substTerm body sub

            val args = List.map Term.mkVar vs

            val isAssert = List.null args

            val eqn =
                Equation
                  {arguments = args,
                   body = body,
                   whereValues = []}

            val n = length (if isAssert then al else pl)

            val kind = if isAssert then "assertion" else "proposition"

            val name = kind ^ Int.toString n

            val const =
                let
                  val n = Name.mk (mainNamespace,name)

                  val tm = Term.listMkAbs (vs,body)

                  val (c,_) = Thm.defineConst n tm
                in
                  c
                end

            val ty =
                let
                  fun addArg (arg,acc) = Type.mkFun (Term.typeOf arg, acc)
                in
                  List.foldr addArg (Term.typeOf body) args
                end

            val value =
                Value
                  {name = const,
                   ty = ty,
                   equations = [eqn]}

            val desc = Print.toString ppTest (kind,n,concl)

            val invoke = if isAssert then "assert" else "check"

            val test =
                Test
                  {name = name,
                   description = desc,
                   value = value,
                   invocation = invoke}
          in
            if isAssert then (test :: al, pl) else (al, test :: pl)
          end
          handle Error err =>
            raise Error ("bad test theorem: " ^ err)

      fun mk al pl ths =
          case ths of
            [] => List.revAppend (al, List.rev pl)
          | th :: ths =>
            let
              val (al,pl) = mkTest al pl th
            in
              mk al pl ths
            end
    in
      mk [] []
    end;

(* ------------------------------------------------------------------------- *)
(* Inferring the set of type operators that must support equality/order.     *)
(* ------------------------------------------------------------------------- *)

datatype inferredEqualityTypes =
    InferredEqualityTypes of
      {interpretation : Interpretation.interpretation,
       equalityTypes : Type.sharingTypeOps,
       seenTerms : IntSet.set};

fun newInferredEqualityTypes int =
    let
      val equalityTypes = Type.emptySharingTypeOps
      and seenTerms = IntSet.empty

(*OpenTheoryTrace3
      val () =
          Print.trace Interpretation.pp
            "Haskell.newInferredEqualityTypes.int" int
*)
    in
      InferredEqualityTypes
        {interpretation = int,
         equalityTypes = equalityTypes,
         seenTerms = seenTerms}
    end;

fun interpretationInferredEqualityTypes ret =
    let
      val InferredEqualityTypes {interpretation,...} = ret
    in
      interpretation
    end;

fun inferredEqualityTypes (InferredEqualityTypes {equalityTypes,...}) =
    Type.toSetSharingTypeOps equalityTypes;

local
  fun addType (ty,ret) =
      let
        val InferredEqualityTypes
              {interpretation,
               equalityTypes,
               seenTerms} = ret

        val equalityTypes = Type.addSharingTypeOps ty equalityTypes
      in
        InferredEqualityTypes
          {interpretation = interpretation,
           equalityTypes = equalityTypes,
           seenTerms = seenTerms}
      end;
in
  fun addTypeInferredEqualityTypes ret ty = addType (ty,ret);

  val addTypesInferredEqualityTypes = List.foldl addType;
end;

fun addConstInferredEqualityTypes ret c ty =
    let
      val int = interpretationInferredEqualityTypes ret

      val tys = inferEqualityTypes int c ty

      val ret = addTypesInferredEqualityTypes ret tys
    in
      ret
    end;

local
  fun registerTerm ret tm =
      let
        val InferredEqualityTypes
              {interpretation,
               equalityTypes,
               seenTerms} = ret

        val i = Term.id tm
      in
        if IntSet.member i seenTerms then NONE
        else
          let
            val seenTerms = IntSet.add seenTerms i

(*OpenTheoryTrace5
            val () =
                Print.trace Term.pp
                  "Haskell.addTermInferredEqualityTypes.registerTerm.tm" tm
*)
            val ret =
                InferredEqualityTypes
                  {interpretation = interpretation,
                   equalityTypes = equalityTypes,
                   seenTerms = seenTerms}
          in
            SOME ret
          end
      end;

  fun addTerms ret tms =
      case tms of
        [] => ret
      | tm :: tms => addTerm ret tm tms

  and addTerm ret tm tms =
      case registerTerm ret tm of
        NONE => addTerms ret tms
      | SOME ret =>
        case destGenAbs tm of
          SOME (_,b) => addTerm ret b tms
        | NONE =>
          case Term.dest tm of
            TypeTerm.Const' (c,ty) =>
            let
              val ret = addConstInferredEqualityTypes ret c ty
            in
              addTerms ret tms
            end
          | TypeTerm.Var' _ => addTerms ret tms
          | TypeTerm.App' (f,x) => addTerm ret f (x :: tms)
          | TypeTerm.Abs' _ => raise Bug "Haskell.addTermInferredEqualityTypes.addTerm.Abs";
in
  fun addTermInferredEqualityTypes ret tm = addTerm ret tm [];
end;

local
  fun addConstructor ((_,tys),ret) =
      addTypesInferredEqualityTypes ret tys;
in
  fun addDataInferredEqualityTypes ret data =
      let
        val Data
              {name = _,
               parameters = _,
               constructors,
               caseConst = _,
               equalityType,
               arbitraryType = _} = data
      in
        if not equalityType then ret
        else List.foldl addConstructor ret constructors
      end;
end;

fun addNewtypeInferredEqualityTypes ret newtype =
    let
      val Newtype
            {name = _,
             parameters = _,
             repType,
             abs = _,
             rep = _,
             equalityType,
             arbitraryType = _} = newtype
    in
      if not equalityType then ret
      else addTypeInferredEqualityTypes ret repType
    end;

local
  fun addWhere (whereValue,ret) =
      let
        val WhereValue
              {name = _,
               equations} = whereValue

        val ret = List.foldl addEquation ret equations
      in
        ret
      end

  and addEquation (equation,ret) =
      let
        val Equation
              {arguments = _,
               body,
               whereValues} = equation

        val ret = addTermInferredEqualityTypes ret body

        val ret = List.foldl addWhere ret whereValues
      in
        ret
      end;
in
  fun addValueInferredEqualityTypes ret value =
      let
        val Value
              {name = _,
               ty = _,
               equations} = value

        val ret = List.foldl addEquation ret equations
      in
        ret
      end;
end;

fun addArbitraryInferredEqualityTypes ret arbitrary =
    let
      val Arbitrary
            {name = _,
             parameters = _,
             lift} = arbitrary

      val ret = addTermInferredEqualityTypes ret lift
    in
      ret
    end;

local
  fun addSource (src,ret) =
      case src of
        DataSource x => addDataInferredEqualityTypes ret x
      | NewtypeSource x => addNewtypeInferredEqualityTypes ret x
      | ValueSource x => addValueInferredEqualityTypes ret x
      | ArbitrarySource x => addArbitraryInferredEqualityTypes ret x;
in
  fun addSourceInferredEqualityTypes ret src = addSource (src,ret);

  val addSourcesInferredEqualityTypes = List.foldl addSource;
end;

local
  fun addModule (module,ret) =
      let
        val Module
              {namespace = _,
               source,
               submodules} = module

        val ret = addSourcesInferredEqualityTypes ret source

        val ret = List.foldl addModule ret submodules
      in
        ret
      end;
in
  fun addModuleInferredEqualityTypes ret module = addModule (module,ret);
end;

local
  fun addTest (test,ret) =
      let
        val Test
              {name = _,
               description = _,
               value,
               invocation = _} = test

        val ret = addValueInferredEqualityTypes ret value
      in
        ret
      end;
in
  fun addTestInferredEqualityTypes ret test = addTest (test,ret);

  val addTestsInferredEqualityTypes = List.foldl addTest;
end;

(* ------------------------------------------------------------------------- *)
(* Inferring the set of type operators that must support arbitrary/show.     *)
(* ------------------------------------------------------------------------- *)

datatype inferredArbitraryTypes =
    InferredArbitraryTypes of Term.sharingTypeOps;

val emptyInferredArbitraryTypes =
    InferredArbitraryTypes Term.emptySharingTypeOps;

fun inferredArbitraryTypes (InferredArbitraryTypes ts) =
    Term.toSetSharingTypeOps ts;

local
  fun addType (ty,ret) =
      let
        val InferredArbitraryTypes ts = ret

        val ts = Term.addTypeSharingTypeOps ty ts
      in
        InferredArbitraryTypes ts
      end;
in
  fun addTypeInferredArbitraryTypes ret ty = addType (ty,ret);

  val addTypesInferredArbitraryTypes = List.foldl addType;
end;

local
  fun addTerm (tm,ret) =
      let
        val InferredArbitraryTypes ts = ret

        val ts = Term.addSharingTypeOps tm ts
      in
        InferredArbitraryTypes ts
      end;
in
  fun addTermInferredArbitraryTypes ret tm = addTerm (tm,ret);

  val addTermsInferredArbitraryTypes = List.foldl addTerm;
end;

local
  fun addConstructor ((_,tys),ret) =
      addTypesInferredArbitraryTypes ret tys;
in
  fun addDataInferredArbitraryTypes ret data =
      let
        val Data
              {name = _,
               parameters = _,
               constructors,
               caseConst = _,
               equalityType = _,
               arbitraryType} = data
      in
        if not arbitraryType then ret
        else List.foldl addConstructor ret constructors
      end;
end;

fun addNewtypeInferredArbitraryTypes ret newtype =
    let
      val Newtype
            {name = _,
             parameters = _,
             repType,
             abs = _,
             rep = _,
             equalityType = _,
             arbitraryType} = newtype
    in
      if not arbitraryType then ret
      else addTypeInferredArbitraryTypes ret repType
    end;

fun addValueInferredArbitraryTypes ret (_ : value) = ret;

fun addArbitraryInferredArbitraryTypes ret arbitrary =
    let
      val Arbitrary
            {name = _,
             parameters = _,
             lift} = arbitrary

      val ty = Type.domainFun (Term.typeOf lift)

      val ret = addTypeInferredArbitraryTypes ret ty
    in
      ret
    end;

local
  fun addSource (src,ret) =
      case src of
        DataSource x => addDataInferredArbitraryTypes ret x
      | NewtypeSource x => addNewtypeInferredArbitraryTypes ret x
      | ValueSource x => addValueInferredArbitraryTypes ret x
      | ArbitrarySource x => addArbitraryInferredArbitraryTypes ret x;
in
  fun addSourceInferredArbitraryTypes ret src = addSource (src,ret);

  val addSourcesInferredArbitraryTypes = List.foldl addSource;
end;

local
  fun addModule (module,ret) =
      let
        val Module
              {namespace = _,
               source,
               submodules} = module

        val ret = addSourcesInferredArbitraryTypes ret source

        val ret = List.foldl addModule ret submodules
      in
        ret
      end;
in
  fun addModuleInferredArbitraryTypes ret module = addModule (module,ret);
end;

local
  fun addTest (test,ret) =
      let
        val Test
              {name = _,
               description = _,
               value,
               invocation = _} = test

        val eqn =
            case value of
              Value
                {name = _,
                 ty = _,
                 equations = [eqn]} => eqn
            | _ => raise Bug "Haskell.addTestInferredArbitraryTypes.eqn"

        val args =
            case eqn of
              Equation
                {arguments,
                 body = _,
                 whereValues = _} => arguments

        val ret = addTermsInferredArbitraryTypes ret args
      in
        ret
      end;
in
  fun addTestInferredArbitraryTypes ret test = addTest (test,ret);

  val addTestsInferredArbitraryTypes = List.foldl addTest;
end;

(* ------------------------------------------------------------------------- *)
(* Converting a theory package to a Haskell package.                         *)
(* ------------------------------------------------------------------------- *)

datatype haskell =
     Haskell of
       {system : RepositorySystem.system,
        information : information,
        depends : depend list,
        interpretation : Interpretation.interpretation,
        source : module,
        tests : test list};

fun information (Haskell {information = x, ...}) = x;

fun name haskell = nameInformation (information haskell);

local
  fun packageTheory repo pkg =
      let
        val sav = false

        val fndr = Repository.finder repo

        val graph = TheoryGraph.empty {savable = sav}

        val imps = TheorySet.empty

        val int = Interpretation.natural

        val (_,thy) =
            TheoryGraph.importPackage fndr graph
              {imports = imps,
               interpretation = int,
               package = pkg}
      in
        thy
      end;

  fun articleAssumptions art =
      let
        val sum = Article.summary art

        val req = Summary.requires sum
      in
        Sequents.sequents req
      end;

  fun derivedTheorems thy {filename} =
      let
        val sav = false

        val imp = Theory.article thy

        val int = Interpretation.natural

        val art =
            Article.fromTextFile
              {savable = sav,
               import = imp,
               interpretation = int,
               filename = filename}

        (* Ensure the article did not make any extra assumptions *)
        val () =
            let
              val thyAsms = articleAssumptions imp
              and artAsms = articleAssumptions art
            in
              if SequentSet.subset artAsms thyAsms then ()
              else raise Error ("extra assumption made in " ^ filename)
            end
      in
        Article.thms art
      end;

  fun checkAttributeTypes tag sym =
      let
        fun check n =
            let
              val kind =
                  case SymbolTable.peekTypeOp sym n of
                    NONE => SOME "unknown"
                  | SOME t =>
                    if not (TypeOp.isUndef t) then
                      NONE
                    else if TypeOpSet.member t TypeOpSet.primitives then
                      SOME "primitive"
                    else
                      SOME "external"
            in
              case kind of
                NONE => ()
              | SOME k =>
                let
                  val err =
                      k ^ " type operator " ^ Name.toString n ^
                      " cannot be declared as a haskell-" ^ tag
                in
                  raise Error err
                end
            end
      in
        NameSet.app check
      end;

  val checkEqualityTypes = checkAttributeTypes equalityTypeTag;

  fun requiredEqualityTypes eqs src tests int =
      let
        fun diff (t,ns) =
            let
              val n = TypeOp.name t
            in
              if TypeOp.isUndef t then
                if not (TypeOpSet.member t TypeOpSet.primitives) then
                  NameSet.add ns n
                else if TypeOp.isBool t then ns
                else
                  let
                    val err =
                        "primitive type operator " ^ Name.toString n ^
                        " is not haskell-" ^ equalityTypeTag
                  in
                    raise Error err
                  end
              else if NameSet.member n eqs then ns
              else
                let
                  val err =
                      "defined type operator " ^ Name.toString n ^
                      " must be declared as haskell-" ^ equalityTypeTag
                in
                  raise Error err
                end
            end

        val ret = newInferredEqualityTypes int

        val ret = addModuleInferredEqualityTypes ret src

        val ret = addTestsInferredEqualityTypes ret tests

        val ts = inferredEqualityTypes ret

(*OpenTheoryTrace3
        val () =
            Print.trace (Print.ppList TypeOp.pp)
              "Haskell.fromPackage.requiredEqualityTypes.ts"
              (TypeOpSet.toList ts)
*)
      in
        TypeOpSet.foldl diff NameSet.empty ts
      end;

  val checkArbitraryTypes = checkAttributeTypes arbitraryTypeTag;

  fun checkArbitraryInstances arbs src =
      let
        fun add m t p =
            let
              val n = TypeOp.name t

              val () =
                  if not (NameMap.inDomain n m) then ()
                  else
                    let
                      val err =
                          "duplicate declaration for type " ^ Name.toString n
                    in
                      raise Error err
                    end
            in
              NameMap.insert m (n,p)
            end

        fun inc (_,s,(dm,im)) =
            case s of
              DataSource (Data {name,parameters,...}) =>
              (add dm name parameters, im)
            | NewtypeSource (Newtype {name,parameters,...}) =>
              (add dm name parameters, im)
            | ValueSource _ => (dm,im)
            | ArbitrarySource (Arbitrary {name,parameters,...}) =>
              (dm, add im name parameters)

        val (dm,im) = foldModule inc (NameMap.new (), NameMap.new ()) src

        fun checkInstance (n,p) =
            case NameMap.peek dm n of
              NONE =>
              let
                val err =
                    "arbitrary instance for undefined type " ^
                    Name.toString n
              in
                raise Error err
              end
            | SOME p' =>
              if not (listEqual Name.equal p' p) then
                let
                  val err =
                      "arbitrary instance has different parameters " ^
                      "for type " ^ Name.toString n
                in
                  raise Error err
                end
              else if not (NameSet.member n arbs) then
                let
                  val err =
                      "arbitrary instance undeclared in theory file " ^
                      "for type " ^ Name.toString n
                in
                  raise Error err
                end
              else ()

        fun checkArbitrary n =
            if not (NameMap.inDomain n dm) then ()
            else if NameMap.inDomain n im then ()
            else
              let
                val err =
                    "no arbitrary instance declared for " ^
                    "haskell-" ^ arbitraryTypeTag ^ ": " ^
                    Name.toString n
              in
                raise Error err
              end

        val () = NameMap.app checkInstance im

        val () = NameSet.app checkArbitrary arbs
      in
        ()
      end;

  fun requiredArbitraryTypes arbs src tests =
      let
        fun diff (t,ns) =
            let
              val n = TypeOp.name t
            in
              if TypeOp.isUndef t then
                if not (TypeOpSet.member t TypeOpSet.primitives) then
                  NameSet.add ns n
                else if TypeOp.isBool t then ns
                else
                  let
                    val err =
                        "primitive type operator " ^ Name.toString n ^
                        " is not haskell-" ^ arbitraryTypeTag
                  in
                    raise Error err
                  end
              else if NameSet.member n arbs then ns
              else
                let
                  val err =
                      "defined type operator " ^ Name.toString n ^
                      " must be declared as haskell-" ^ arbitraryTypeTag
                in
                  raise Error err
                end
            end

        val ret = emptyInferredArbitraryTypes

        val ret = addModuleInferredArbitraryTypes ret src

        val ret = addTestsInferredArbitraryTypes ret tests

        val ts = inferredArbitraryTypes ret

(*OpenTheoryTrace3
        val () =
            Print.trace (Print.ppList TypeOp.pp)
              "Haskell.fromPackage.requiredArbitraryTypes.ts"
              (TypeOpSet.toList ts)
*)
      in
        TypeOpSet.foldl diff NameSet.empty ts
      end;
in
  fun fromPackage repo prev namever =
      let
        val pkg =
            case Repository.peek repo namever of
              SOME p => p
            | NONE =>
              let
                val err =
                    "package " ^ PackageNameVersion.toString namever ^
                    " is not installed"
              in
                raise Error err
              end

        val sys = Repository.system repo

        val {information = info,
             srcFilename = srcFile,
             interpretation = thyInt,
             testFilename = testFile} =
            case mkInformation repo pkg of
              SOME x => x
            | NONE =>
              let
                val err = "no haskell-src-file information"
              in
                raise Error err
              end

        val thy = packageTheory repo pkg

        val eqs = equalityTypesInformation info
        and arbs = arbitraryTypesInformation info

        val src =
            let
              val file = Package.joinDirectory pkg {filename = srcFile}

              val ths = derivedTheorems thy file
            in
              mkSourceModule eqs arbs thyInt ths
            end

        val () = checkArbitraryInstances arbs src

        val tests =
            case testFile of
              NONE => []
            | SOME filename =>
              let
                val file = Package.joinDirectory pkg {filename = filename}

                val ths = Thms.thms (derivedTheorems thy file)

                val tests = mkTests (Package.show pkg) (ThmSet.toList ths)

                val () =
                    if not (List.null tests) then ()
                    else raise Error ("no tests defined in " ^ filename)
              in
                tests
              end

        val (deps,int) =
            let
              val reqs = Package.requires pkg

              val sym =
                  SymbolTable.union
                    (symbolTableModule src)
                    (symbolTableTests tests)

              val () = checkEqualityTypes sym eqs
              and () = checkArbitraryTypes sym arbs

              val reqEqs = requiredEqualityTypes eqs src tests

              val reqArbs = requiredArbitraryTypes arbs src tests
            in
              mkDepends repo prev reqs thyInt thy sym reqEqs reqArbs
            end
      in
        Haskell
          {system = sys,
           information = info,
           depends = deps,
           interpretation = int,
           source = src,
           tests = tests}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Creating the symbol name mapping in preparation for printing.             *)
(* ------------------------------------------------------------------------- *)

datatype symbolExport =
    SymbolExport of
      {interpretation : Interpretation.interpretation,
       namespace : Namespace.namespace,
       importNamespaces : Namespace.namespace option NamespaceMap.map};

fun namespaceSymbolExport (SymbolExport {namespace = ns, ...}) = ns;

fun importNamespacesSymbolExport exp =
    let
      val SymbolExport {importNamespaces,...} = exp
    in
      NamespaceMap.toList importNamespaces
    end;

local
  fun abbreviateName namespace imp n =
      let
        val (ns,c) = Name.dest n
      in
        if Namespace.equal ns namespace then Name.mkGlobal c
        else
          case NamespaceMap.peek imp ns of
            NONE => n
          | SOME NONE => n
          | SOME (SOME ns) => Name.mk (ns,c)
      end;
in
  fun typeOpNameSymbolExport exp n =
      let
        val SymbolExport
              {interpretation = int,
               namespace = ns,
               importNamespaces = imp} = exp

        val n = Interpretation.interpretTypeOp int n
      in
        abbreviateName ns imp n
      end;

  fun constNameSymbolExport exp n =
      let
        val SymbolExport
              {interpretation = int,
               namespace = ns,
               importNamespaces = imp} = exp

        val n = Interpretation.interpretConst int n
      in
        abbreviateName ns imp n
      end;
end;

local
  fun symbolTableNamespaces int =
      let
        fun typeOpNamespace t =
            let
              val n = TypeOp.name t

              val n = Interpretation.interpretTypeOp int n
            in
              Name.namespace n
            end

        fun constNamespace c =
            let
              val n = Const.name c

              val n =
                  case total Name.destCase n of
                    NONE => n
                  | SOME (_,l) => hd l

              val n = Interpretation.interpretConst int n
            in
              Name.namespace n
            end

        fun symbolNamespace s =
            case s of
              Symbol.TypeOp t => typeOpNamespace t
            | Symbol.Const c => constNamespace c

        fun addSymbolNamespace (s,ns) =
            NamespaceSet.add ns (symbolNamespace s)

        fun tableNamespaces tab =
            let
              val ss = SymbolTable.symbols tab
            in
              SymbolSet.foldl addSymbolNamespace NamespaceSet.empty ss
            end
      in
        tableNamespaces
      end;

  val abbreviateNamespaces =
      let
        fun lengthN ns = List.length (Namespace.toList ns)

        fun abbrevN ns =
            let
              fun abbrevList h t =
                  case abbrevTail t of
                    SOME n => SOME n
                  | NONE =>
                    let
                      val n = Namespace.fromList (h :: t)
                    in
                      if NamespaceSet.member n ns then NONE else SOME n
                    end

              and abbrevTail l =
                  case l of
                    [] => NONE
                  | h :: t => abbrevList h t

              fun abbrev n =
                  case Namespace.toList n of
                    [] => NONE
                  | _ :: l => abbrevTail l
            in
              abbrev
            end

        fun addAbbrevN (n,(ns,nm)) =
            let
              val n' = abbrevN ns n

              val ns = NamespaceSet.add ns (Option.getOpt (n',n))
              and nm = NamespaceMap.insert nm (n,n')
            in
              (ns,nm)
            end

        fun abbrevNS ns =
            let
              val ns = sortMap lengthN Int.compare (NamespaceSet.toList ns)

              val (_,nm) =
                  List.foldl addAbbrevN
                    (NamespaceSet.empty, NamespaceMap.new ()) ns
            in
              nm
            end
      in
        abbrevNS
      end;
in
  fun mkSymbolExport int ns src =
      let
        val sym = explicitSymbolTableSourceList src

        val imp =
            let
              val white = symbolTableNamespaces int sym
              and black = NamespaceSet.fromList [ns,Namespace.global]

              val ns = NamespaceSet.difference white black

              val ns =
                  if not (List.exists isArbitrarySource src) then ns
                  else NamespaceSet.add ns testNamespace
            in
              abbreviateNamespaces ns
            end
      in
        SymbolExport
          {interpretation = int,
           namespace = ns,
           importNamespaces = imp}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Haskell syntax using a symbol mapping.                                    *)
(* ------------------------------------------------------------------------- *)

(* List types *)

fun destListType exp =
    let
      fun pred t =
          let
            val n = typeOpNameSymbolExport exp (TypeOp.name t)
          in
            Name.equal n listTypeName
          end

      val dest = Type.destUnaryOp pred
    in
      total dest
    end;

fun isListType exp ty = Option.isSome (destListType exp ty);

(* Pair types *)

fun destPairType exp =
    let
      fun pred t =
          let
            val n = typeOpNameSymbolExport exp (TypeOp.name t)
          in
            Name.equal n pairTypeName
          end

      val dest = Type.destBinaryOp pred
    in
      total dest
    end;

fun isPairType exp ty = Option.isSome (destPairType exp ty);

(* Pair terms *)

fun destPair exp =
    let
      fun pred c =
          let
            val n = constNameSymbolExport exp (Const.name c)
          in
            Name.equal n pairName
          end

      val dest = Term.destBinaryOp pred
    in
      total dest
    end;

fun isPair exp tm = Option.isSome (destPair exp tm);

(* Infix terms *)

fun destInfix exp =
    let
      fun dest tm =
          let
            val (t,b) = Term.destApp tm

            val (t,a) = Term.destApp t

            val (c,_) = Term.destConst t

            val n = constNameSymbolExport exp (Const.name c)

            val (ns,s) = Name.dest n

            val () =
                if Namespace.isGlobal ns then ()
                else raise Error "Haskell.destInfix: not global"

            val () =
                if StringSet.member s infixTokens then ()
                else raise Error "Haskell.destInfix: not infix"
          in
            (s,a,b)
          end
    in
      total dest
    end;

fun isInfix exp tm = Option.isSome (destInfix exp tm);

(* Generalized applications *)

fun destGenApp exp =
    let
      fun dest tm =
          if isNumeral tm then
            raise Error "Haskell.destGenApp: numeral"
          else if isCond tm then
            raise Error "Haskell.destGenApp: cond"
          else if isLet tm then
            raise Error "Haskell.destGenApp: let"
          else if isGenAbs tm then
            raise Error "Haskell.destGenApp: abstraction"
          else if isCase tm then
            raise Error "Haskell.destGenApp: case"
          else if isPair exp tm then
            raise Error "Haskell.destGenApp: pair"
          else if isInfix exp tm then
            raise Error "Haskell.destGenApp: infix"
          else
            Term.destApp tm
    in
      total dest
    end;

fun stripGenApp exp =
    let
      fun strip acc tm =
          case destGenApp exp tm of
            NONE => (tm,acc)
          | SOME (f,x) => strip (x :: acc) f
    in
      strip []
    end;

(* ------------------------------------------------------------------------- *)
(* Haskell tags.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype tags = Tags of string StringMap.map;

fun allTags (Tags m) = StringSet.domain m;

fun getTag (Tags m) n =
    case StringMap.peek m n of
      SOME v => v
    | NONE => raise Bug "Haskell.getTag: not found";

fun nameTags tags = getTag tags nameTag;

fun versionTags tags = getTag tags versionTag;

fun nameVersionTags tags = nameTags tags ^ "-" ^ versionTags tags;

fun descriptionTags tags = getTag tags descriptionTag;

fun licenseTags tags = getTag tags licenseTag;

fun licenseFileTags tags = getTag tags licenseFileTag;

fun updateVersionTag (Tags m) v =
    Tags (StringMap.insert m (versionTag, PackageVersion.toString v));

local
  fun overrideTag (tag,tags) =
      let
        val PackageTag.Tag' {name,value} = PackageTag.dest tag

        val name = PackageName.toString name
      in
        StringMap.insert tags (name,value)
      end;
in
  fun mkTags info =
      let
        val Information
              {name,
               version,
               description = synopsis,
               author,
               license,
               licenseUrl = _,
               provenance,
               equalityTypes = _,
               arbitraryTypes = _,
               tags = otags} = info

        val name = PackageName.toString name
        and version = PackageVersion.toString version
        and author = PackageAuthor.toString author
        and description =
            synopsis ^ " - this package was automatically generated " ^
            "from the OpenTheory package " ^
            PackageNameVersion.toString provenance

        val tags =
            StringMap.fromList
              [(authorTag,author),
               (buildTypeTag,"Simple"),
               (cabalVersionTag,">= 1.8.0.2"),
               (categoryTag,"Formal Methods"),
               (descriptionTag,description),
               (ghcOptionsTag,"-Wall"),
               (licenseTag,license),
               (licenseFileTag,"LICENSE"),
               (maintainerTag,author),
               (nameTag,name),
               (portabilityTag,"portable"),
               (stabilityTag,"provisional"),
               (synopsisTag,synopsis),
               (versionTag,version)]

        val tags = List.foldl overrideTag tags otags
      in
        Tags tags
      end;
end;

fun ppTag (n,v) =
    Print.inconsistentBlock 2
      [Print.ppString n,
       Print.ppString ": ",
       Print.ppString v];

local
  fun ppTag1 tags n =
      let
        val v = getTag tags n
      in
        ppTag (n,v)
      end;
in
  fun ppTags tags ns =
      case ns of
        [] => raise Bug "Haskell.ppTags: not found"
      | n :: ns =>
        Print.inconsistentBlock 0
          (ppTag1 tags n ::
           List.map (Print.sequence Print.newline o ppTag1 tags) ns);
end;

(* ------------------------------------------------------------------------- *)
(* Printing Haskell source code.                                             *)
(* ------------------------------------------------------------------------- *)

val ppSyntax = Print.ppString;

fun ppInfixToken s =
    if isSymbolString s then ppSyntax s
    else Print.ppBracket "`" "`" ppSyntax s;

(* Names *)

val ppPackageName = PackageName.pp;

fun ppNamespace ns = Namespace.pp ns;

fun ppName n =
    if not (isSymbolName n) then Name.pp n
    else Print.ppBracket "(" ")" Name.pp n;

fun ppTypeOpName exp n =
    ppName (typeOpNameSymbolExport exp n);

fun ppConstName exp n =
    ppName (constNameSymbolExport exp n);

fun ppTypeVarName n =
    if not (Name.isGlobal n) then
      let
        val err =
            "type variable name " ^ Name.toString n ^
            " is not global"
      in
        raise Error err
      end
    else
      case String.explode (Name.destGlobal n) of
        [] => raise Error "type variable name is empty string"
      | c :: cs =>
        let
          val () =
              if Char.isUpper c then ()
              else
                let
                  val err =
                      "type variable name " ^ Name.toString n ^
                      " does not begin with upper case"
                in
                  raise Error err
                end
        in
          Print.program (List.map Print.ppChar (Char.toLower c :: cs))
        end;

fun ppVarName n =
    if Name.isGlobal n then Name.pp n
    else
      let
        val err =
            "variable name " ^ Name.toString n ^
            " is not global"
      in
        raise Error err
      end;

(* Types *)

fun ppTypeOp exp ot = ppTypeOpName exp (TypeOp.name ot);

val ppTypeVar = ppTypeVarName;

val ppTypeVarList =
    let
      fun ppSpaceTypeVar v = Print.sequence Print.space (ppTypeVar v)
    in
      fn vl => Print.program (List.map ppSpaceTypeVar vl)
    end;

local
  fun destApp exp ty =
      if Type.isFun ty then NONE
      else if isListType exp ty then NONE
      else if isPairType exp ty then NONE
      else
        case Type.dest ty of
          TypeTerm.VarTy' _ => NONE
        | TypeTerm.OpTy' (ot,tys) =>
          if List.null tys then NONE else SOME (ot,tys);

  fun ppBasic exp ty =
      case Type.dest ty of
        TypeTerm.VarTy' n => ppTypeVar n
      | TypeTerm.OpTy' (ot,tys) =>
        if List.null tys then ppTypeOp exp ot
        else Print.ppBracket "(" ")" (ppGen exp) ty

  and ppList exp ty =
      case destListType exp ty of
        NONE => ppBasic exp ty
      | SOME a =>
        Print.inconsistentBlock 1
          [ppSyntax "[",
           ppGen exp a,
           ppSyntax "]"]

  and ppPair exp ty =
      case destPairType exp ty of
        NONE => ppList exp ty
      | SOME (a,b) =>
        Print.inconsistentBlock 1
          [ppSyntax "(",
           ppApp exp a,
           ppSyntax ",",
           Print.break,
           ppFun exp b,
           ppSyntax ")"]

  and ppArguments exp tys =
      let
        val brk = Print.ppBreak (Print.Break {size = 1, extraIndent = 2})

        fun ppArg ty = Print.sequence brk (ppPair exp ty)
      in
        Print.program (List.map ppArg tys)
      end

  and ppApp exp ty =
      case destApp exp ty of
        NONE => ppPair exp ty
      | SOME (ot,tys) =>
        Print.inconsistentBlock 0
          [ppTypeOp exp ot,
           ppArguments exp tys]

  and ppFun exp ty =
      if not (Type.isFun ty) then ppApp exp ty
      else
        let
          fun ppDom d =
              Print.program
                [ppApp exp d,
                 Print.space,
                 ppSyntax "->",
                 Print.ppBreak (Print.Break {size = 1, extraIndent = 2})]

          val (ds,r) = Type.stripFun ty
        in
          Print.inconsistentBlock 0
            [Print.program (List.map ppDom ds),
             ppApp exp r]
        end

  and ppGen exp ty = ppFun exp ty;
in
  val ppType = ppGen;

  val ppTypeList = ppArguments;
end;

(* Terms *)

fun ppConst exp c =
    let
      val n = Const.name c
    in
      ppConstName exp n
    end;

local
  val unused = #"_";
in
  fun ppVar v =
      let
        val n = Var.name v

        val u =
            case Name.firstChar n of
              NONE => raise Error "variable name cannot be empty string"
            | SOME c => c = unused
      in
        if u then Print.ppChar unused else ppVarName n
      end;
end;

local
  fun isLetCondCase tm = isLet tm orelse isCond tm orelse isCase tm;

  fun ppInfixTokenSpace (_,s) =
      Print.program [Print.space, ppInfixToken s, Print.break];
in
  fun ppTerm exp =
      let
        val ppInfix = Print.ppInfixes infixes (destInfix exp) ppInfixTokenSpace

        fun ppBasicTerm tm =
            case destNumeral tm of
              SOME i => Print.ppInt i
            | NONE =>
              case destPair exp tm of
                SOME x_y => ppPair x_y
              | NONE =>
                case Term.dest tm of
                  TypeTerm.Var' v => ppVar v
                | TypeTerm.Const' (c,_) => ppConst exp c
                | TypeTerm.App' _ => ppBracketTerm tm
                | TypeTerm.Abs' _ => ppBracketTerm tm

        and ppApplicationTerm tm =
            let
              val b = Print.Break {size = 1, extraIndent = 2}

              fun ppArg x = Print.sequence (Print.ppBreak b) (ppBasicTerm x)

              val (tm,xs) = stripGenApp exp tm
            in
              if List.null xs then ppBasicTerm tm
              else
                Print.inconsistentBlock 0
                  (ppBasicTerm tm :: List.map ppArg xs)
            end

        and ppPair (x,y) =
            let
              fun ppComponent tm = ppInfixTerm (tm,true)
            in
              Print.inconsistentBlock 1
                [ppSyntax "(",
                 ppComponent x,
                 ppSyntax ",",
                 Print.break,
                 ppComponent y,
                 ppSyntax ")"]
            end

        and ppBoundVars (v,vs) =
            Print.program
              (ppBasicTerm v ::
               List.map (Print.sequence Print.break o ppBasicTerm) vs @
               [Print.space,
                ppSyntax "->"])

        and ppBindTerm (v,vs,body) =
            let
              val anon = anonymize [body]

              val v = anon v
              and vs = List.map anon vs
            in
              Print.inconsistentBlock 2
                [ppSyntax "\\",
                 ppBoundVars (v,vs),
                 Print.break,
                 ppNormalTerm body]
            end

        and ppBinderTerm (tm,r) =
            let
              val (vs,body) = stripGenAbs tm
            in
              case vs of
                [] => ppApplicationTerm tm
              | v :: vs =>
                if r then Print.ppBracket "(" ")" ppBindTerm (v,vs,body)
                else ppBindTerm (v,vs,body)
            end

        and ppLetTerm (v,t,b,r) =
            let
              val v = anonymize [b] v

              val ppLetBind =
                  Print.inconsistentBlock 4
                    [ppSyntax "let",
                     Print.space,
                     ppApplicationTerm v,
                     Print.space,
                     ppSyntax "=",
                     Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
                     ppLetCondCaseNestedTerm (t,true),
                     Print.space,
                     ppSyntax "in"]

              val ppLetBody =
                  case destLet b of
                    NONE => [ppLetCondCaseNestedTerm (b,r)]
                  | SOME (v,t,b) => ppLetTerm (v,t,b,r)
            in
              ppLetBind ::
              Print.break ::
              ppLetBody
            end

        and ppCondTerm (f,c,a,b,r) =
            let
              val ppCond = ppInfixTerm (c,true)

              val ppIfCond =
                  if f then
                    Print.inconsistentBlock 3
                      [ppSyntax "if",
                       Print.space,
                       ppCond]
                  else
                    Print.inconsistentBlock 8
                      [ppSyntax "else",
                       Print.space,
                       ppSyntax "if",
                       Print.space,
                       ppCond]

              val ppIfCondThen =
                  Print.consistentBlock 0
                    [ppIfCond,
                     Print.break,
                     ppSyntax "then"]

              val ppIfCondThenBranch =
                  Print.inconsistentBlock 0
                    [ppIfCondThen,
                     Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
                     ppLetCondCaseNestedTerm (a,true)]

              val ppElseBranch =
                  case destCond b of
                    SOME (c,a,b) => ppCondTerm (false,c,a,b,r)
                  | NONE =>
                    [Print.inconsistentBlock 2
                       [ppSyntax "else",
                        Print.break,
                        ppLetCondCaseNestedTerm (b,r)]]
            in
              ppIfCondThenBranch ::
              Print.break ::
              ppElseBranch
            end

        and ppCaseTerm ((a,bs),r) =
            let
              fun ppBranch (pat,(t,r)) =
                  let
                    val pat = anonymize [t] pat
                  in
                    Print.consistentBlock 2
                      [ppApplicationTerm pat,
                       Print.space,
                       ppSyntax "->",
                       Print.break,
                       ppLetCondCaseNestedTerm (t,r)]
                  end

              val ppDecl =
                  Print.consistentBlock 5
                    [ppSyntax "case",
                     Print.space,
                     ppInfixTerm (a,true),
                     Print.space,
                     ppSyntax "of"]

              fun ppAlternative br =
                  Print.program
                    [Print.newline,
                     ppBranch br]

              val brs =
                  let
                    fun add ((pat,t),bs) = (pat,(t,true)) :: bs
                  in
                    case List.rev bs of
                      [] => raise Bug "Haskell.ppTerm.ppCaseTerm: no branches"
                    | (pat,t) :: bs => List.foldl add [(pat,(t,r))] bs
                  end
            in
              Print.consistentBlock 2
                (ppDecl :: List.map ppAlternative brs)
            end

        and ppLetCondCaseNestedTerm (tm,r) =
            case destLet tm of
              SOME (v,t,b) =>
                Print.consistentBlock 0 (ppLetTerm (v,t,b,r))
            | NONE =>
              case destCond tm of
                SOME (c,a,b) =>
                Print.consistentBlock 0 (ppCondTerm (true,c,a,b,r))
              | NONE =>
                case destCase tm of
                  SOME a_bs => ppCaseTerm (casePatterns a_bs, r)
                | NONE => ppInfixTerm (tm,r)

        and ppLetCondCaseTerm (tm,r) =
            if not (isLetCondCase tm) then ppBinderTerm (tm,r)
            else if r then ppBracketTerm tm
            else ppLetCondCaseNestedTerm (tm,false)

        and ppInfixTerm tm_r = ppInfix ppLetCondCaseTerm tm_r

        and ppNormalTerm tm =
            let
(*OpenTheoryTrace5
              val () = Print.trace Term.pp
                         "Haskell.ppTerm.ppNormalTerm: tm" tm
*)
            in
              ppInfixTerm (tm,false)
            end

        and ppBracketTerm tm = Print.ppBracket "(" ")" ppNormalTerm tm
      in
        ppNormalTerm
      end;
end;

(*OpenTheoryDebug
val ppTerm = fn exp => fn tm =>
    let
      val result = ppTerm exp tm
    in
      result
    end
    handle Bug bug =>
      let
        val bug =
            "failed to print the following term in Haskell syntax:\n" ^
            Print.toString Term.pp tm ^ "\n" ^ bug
      in
        raise Bug bug
      end;
*)

(* Haskell *)

fun ppDepend dep =
    let
      val Depend {name,oldest,newest} = dep

      val next = PackageVersion.increment newest
    in
      Print.program
        [ppPackageName name,
         ppSyntax " >= ",
         PackageVersion.pp oldest,
         ppSyntax " && < ",
         PackageVersion.pp next]
    end;

fun ppDeriving {equalityType,arbitraryType} =
    let
      val classes =
          (if equalityType then ["Eq","Ord"] else []) @
          (if arbitraryType then ["Show"] else [])
    in
      if List.null classes then Print.skip
      else
        Print.sequence
          Print.newline
          (Print.inconsistentBlock 2
             [ppSyntax "deriving (",
              Print.ppOpList "," ppSyntax classes,
              ppSyntax ")"])
    end;

local
  fun ppDecl exp (name,parms) =
      Print.inconsistentBlock 2
        [ppSyntax "data ",
         ppTypeOp exp name,
         ppTypeVarList parms,
         ppSyntax " ="];

  fun ppCon exp prefix (c,tys) =
      Print.program
        [Print.newline,
         ppSyntax prefix,
         Print.inconsistentBlock 4
           [ppConst exp c,
            ppTypeList exp tys]];

  fun ppCons exp cs =
      case cs of
        [] => raise Error "datatype has no constructors"
      | c :: cs =>
        Print.program (ppCon exp "  " c :: List.map (ppCon exp "| ") cs);
in
  fun ppData exp data =
      let
        val Data
              {name,
               parameters = parms,
               constructors = cons,
               caseConst = _,
               equalityType = eq,
               arbitraryType = arb} = data
      in
        Print.inconsistentBlock 2
          [ppDecl exp (name,parms),
           ppCons exp cons,
           ppDeriving {equalityType = eq, arbitraryType = arb}]
    end;
end;

local
  fun ppDecl exp (name,parms) =
      Print.inconsistentBlock 2
        [ppSyntax "newtype ",
         ppTypeOp exp name,
         ppTypeVarList parms,
         ppSyntax " ="];

  fun ppRep exp (rep,repType) =
      Print.inconsistentBlock 2
        [ppConst exp rep,
         Print.space,
         ppSyntax "::",
         Print.break,
         ppType exp repType];

  fun ppIso exp (abs,rep) =
      Print.consistentBlock 0
        [ppConst exp abs,
         Print.space,
         ppSyntax "{",
         Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
         ppRep exp rep,
         Print.break,
         ppSyntax "}"];
in
  fun ppNewtype exp newtype =
      let
        val Newtype
              {name,
               parameters = parms,
               repType,
               abs,
               rep,
               equalityType = eq,
               arbitraryType = arb} = newtype
      in
        Print.inconsistentBlock 2
          [ppDecl exp (name,parms),
           Print.break,
           ppIso exp (abs,(rep,repType)),
           ppDeriving {equalityType = eq, arbitraryType = arb}]
      end;
end;

local
  fun bodiesWhereValue value =
      let
        val WhereValue {name = _, equations = eqns} = value
      in
        bodiesEquations eqns
      end

  and bodiesWhereValues values =
      case values of
        [] => []
      | value :: values => bodiesWhereValue value @ bodiesWhereValues values

  and bodiesEquation eqn =
      let
        val Equation {arguments = _, body, whereValues = values} = eqn
      in
        body :: bodiesWhereValues values
      end

  and bodiesEquations eqns =
      case eqns of
        [] => []
      | eqn :: eqns => bodiesEquation eqn @ bodiesEquations eqns;

  fun ppDecl exp (tm,ty) =
      Print.inconsistentBlock 2
        [ppTerm exp tm,
         ppSyntax " ::",
         Print.break,
         ppType exp ty];

  fun ppEquation exp tm eqn =
      let
        val Equation {arguments = args, body = rtm, whereValues = values} = eqn

        val args = List.map (anonymize (bodiesEquation eqn)) args

        val ltm = Term.listMkApp (tm,args)
      in
        Print.consistentBlock 2
          (ppTerm exp ltm ::
           ppSyntax " =" ::
           Print.break ::
           ppTerm exp rtm ::
           ppWherevalues exp values)
      end

  and ppWherevalues exp values =
      let
        fun ppSpaceVal value =
            Print.sequence (Print.newlines 2) (ppWhereValue exp value)
      in
        case values of
          [] => []
        | value :: values =>
          [Print.newline,
           Print.consistentBlock 0
             (ppSyntax "where" ::
              Print.newline ::
              ppWhereValue exp value ::
              List.map ppSpaceVal values)]
      end

  and ppWhereValue exp value =
      let
        val WhereValue {name, equations = eqns} = value

        val tm = Term.mkVar name
        and ty = Var.typeOf name
      in
        Print.inconsistentBlock 2
          (Print.ppBracket "{-" "-}" (ppDecl exp) (tm,ty) ::
           List.map (Print.sequence Print.newline o ppEquation exp tm) eqns)
      end;
in
  fun ppValue exp value =
      let
        val Value {name, ty, equations = eqns} = value

        val tm = Term.mkConst (name,ty)
      in
        Print.inconsistentBlock 0
          (ppDecl exp (tm,ty) ::
           List.map (Print.sequence Print.newline o ppEquation exp tm) eqns)
    end;
end;

local
  fun isBasicType ty =
      case Type.dest ty of
        TypeTerm.VarTy' _ => true
      | TypeTerm.OpTy' (_,tys) => List.null tys;

  fun isBasicTerm tm =
      Term.isVar tm orelse
      Term.isConst tm;

  fun ppBasicType exp ty =
      if isBasicType ty then ppType exp ty
      else Print.ppBracket "(" ")" (ppType exp) ty;

  fun ppBasicTerm exp tm =
      if isBasicTerm tm then ppTerm exp tm
      else Print.ppBracket "(" ")" (ppTerm exp) tm;

  fun ppArb exp ty =
      Print.inconsistentBlock 2
        [ppTypeOpName exp arbitraryClassName,
         Print.break,
         ppBasicType exp ty];

  fun ppCommaPrem exp v =
      Print.program
        [ppSyntax ",",
         Print.break,
         ppArb exp v];

  fun ppPrem1 exp v vs =
      if List.null vs then ppArb exp v
      else
        Print.consistentBlock 1
          ([ppSyntax "(",
            ppArb exp v] @
           List.map (ppCommaPrem exp) vs @
           [ppSyntax ")"]);

  fun ppPrems exp vs =
      case List.map Type.mkVar vs of
        [] => Print.skip
      | v :: vs => Print.sequence (ppPrem1 exp v vs) (ppSyntax " =>");

  fun ppDecl exp (t,vs) =
      let
        val ty = Type.mkOp (t, List.map Type.mkVar vs)
      in
        Print.consistentBlock 7
          [ppSyntax "instance ",
           ppPrems exp vs,
           Print.break,
           ppArb exp ty,
           ppSyntax " where"]
      end;

  fun ppLift exp lift =
      Print.inconsistentBlock 2
        [ppSyntax (Name.component arbitraryConstName),
         ppSyntax " =",
         Print.break,
         Print.inconsistentBlock 2
           [ppSyntax "fmap",
            Print.break,
            ppBasicTerm exp lift,
            Print.break,
            ppConstName exp arbitraryConstName]];
in
  fun ppArbitrary exp arbitrary =
      let
        val Arbitrary {name,parameters,lift} = arbitrary
      in
        Print.inconsistentBlock 2
          [ppDecl exp (name,parameters),
           Print.newline,
           ppLift exp lift]
      end;
end;

fun ppSource exp s =
    case s of
      DataSource x => ppData exp x
    | NewtypeSource x => ppNewtype exp x
    | ValueSource x => ppValue exp x
    | ArbitrarySource x => ppArbitrary exp x;

local
  fun ppSpaceSource exp s =
      Print.sequence
        (Print.sequence Print.newline Print.newline)
        (ppSource exp s);
in
  fun ppSourceList exp sl =
      case sl of
        [] => Print.skip
      | s :: sl =>
        Print.program (ppSource exp s :: List.map (ppSpaceSource exp) sl);
end;

fun ppModuleDeclaration exp =
    let
      val ns = namespaceSymbolExport exp
    in
      Print.inconsistentBlock 0
        [ppSyntax "module ",
         ppNamespace ns,
         Print.newline,
         ppSyntax "where"]
    end;

local
  fun ppImportNamespace (ns,ns') =
      let
        val ppImportAs =
            Print.inconsistentBlock 2
              ([ppSyntax "import",
                Print.space,
                ppSyntax "qualified",
                Print.space,
                ppNamespace ns] @
               (case ns' of
                  NONE => []
                | SOME ns =>
                  [Print.break,
                   ppSyntax "as",
                   Print.space,
                   ppNamespace ns]))
      in
        Print.sequence ppImportAs Print.newline
      end;
in
  fun ppModuleImport exp =
      let
        val import = importNamespacesSymbolExport exp
      in
        Print.inconsistentBlock 0
          (List.map ppImportNamespace import)
      end;
end;

fun ppModule int (tags,namespace,source) =
    let
      val desc = getTag tags synopsisTag
      and exp = mkSymbolExport int namespace source
    in
      Print.inconsistentBlock 0
        [ppSyntax "{- |",
         Print.newline,
         ppTag (moduleTag,"$Header$"),
         Print.newline,
         ppTag (descriptionTag,desc),
         Print.newline,
         ppTags tags [licenseTag],
         Print.newlines 2,
         ppTags tags
           [maintainerTag,
            stabilityTag,
            portabilityTag],
         Print.newline,
         ppSyntax "-}",
         Print.newlines 2,
         ppModuleDeclaration exp,
         Print.newlines 2,
         ppModuleImport exp,
         Print.newline,
         ppSourceList exp source]
    end;

local
  fun mkTestSymbolExport int tests =
      let
        val ns = mainNamespace
        and src = sourceTests tests
      in
        mkSymbolExport int ns src
      end;

  fun ppTestSpace exp test =
      let
        val Test {value,...} = test
      in
        Print.sequence (ppValue exp value) (Print.newlines 2)
      end;

  fun ppInvokeTest test =
      let
        val Test {name, description = desc, invocation = invoke, ...} = test
      in
        Print.program
          [ppSyntax invoke,
           ppSyntax " \"",
           ppSyntax (escapeString desc),
           ppSyntax "\\n  \" ",
           ppSyntax name,
           Print.newline]
      end;

  fun ppMain tests =
      Print.inconsistentBlock 0
        [ppSyntax "main :: IO ()",
         Print.newline,
         Print.inconsistentBlock 4
           [ppSyntax "main =",
            Print.newline,
            Print.consistentBlock 3
              [ppSyntax "do ",
               Print.program (List.map ppInvokeTest tests),
               ppSyntax "return ()"]]];
in
  fun ppTests int (tags,tests) =
      let
        val desc = getTag tags synopsisTag
        and exp = mkTestSymbolExport int tests
      in
        Print.inconsistentBlock 0
          [ppSyntax "{- |",
           Print.newline,
           ppTag (moduleTag,"Main"),
           Print.newline,
           ppTag (descriptionTag, desc ^ " - testing"),
           Print.newline,
           ppTags tags [licenseTag],
           Print.newlines 2,
           ppTags tags
             [maintainerTag,
              stabilityTag,
              portabilityTag],
           Print.newline,
           ppSyntax "-}",
           Print.newline,
           ppSyntax "module Main",
           Print.newline,
           ppSyntax "  ( main )",
           Print.newline,
           ppSyntax "where",
           Print.newlines 2,
           ppModuleImport exp,
           ppSyntax "import OpenTheory.Primitive.Test",
           Print.newline,
           Print.newline,
           Print.program (List.map (ppTestSpace exp) tests),
           ppMain tests]
      end;
end;

(* Cabal *)

local
  val initialTags =
      [nameTag,
       versionTag,
       categoryTag,
       synopsisTag,
       licenseTag,
       licenseFileTag,
       cabalVersionTag,
       buildTypeTag,
       authorTag,
       maintainerTag];

  val nonExtraTags =
      StringSet.fromList
        (descriptionTag ::
         ghcOptionsTag ::
         portabilityTag ::
         stabilityTag ::
         initialTags)

  fun ppSection s pps =
      Print.inconsistentBlock 2
        [ppSyntax s,
         Print.newline,
         Print.program pps];

  local
    fun ppExtraDepend dep =
        Print.program
          [ppSyntax ",",
           Print.newline,
           ppDepend dep];
  in
    fun ppBuildDepends deps =
        ppSyntax "base >= 4.0 && < 5.0," ::
        Print.newline ::
        ppSyntax "QuickCheck >= 2.4.0.1 && < 3.0," ::
        Print.newline ::
        ppSyntax "opentheory-primitive >= 1.4 && < 2.0" ::
        List.map ppExtraDepend deps;
  end;

  local
    val ppMod = ppNamespace;

    fun ppCommaMod ns =
        Print.program
          [ppSyntax ",",
           Print.newline,
           ppMod ns];
  in
    fun ppExposedModules mods =
        case NamespaceSet.toList mods of
          [] => []
        | ns :: nss => ppMod ns :: List.map ppCommaMod nss;
  end;

  fun ppText s =
      case String.tokens Char.isSpace s of
        [] => Print.skip
      | x :: xs =>
        Print.program
          (ppSyntax x ::
           List.map (Print.sequence Print.break o ppSyntax) xs);
in
  fun ppCabal (tags,deps,src,tests) =
      let
        val name = nameTags tags
        and nameVersion = nameVersionTags tags
        and desc = descriptionTags tags
        and mods = exposedModule src

        val extraTags = StringSet.difference (allTags tags) nonExtraTags
      in
        Print.inconsistentBlock 0
          ([ppTags tags (initialTags @ StringSet.toList extraTags),
            Print.newline,
            Print.inconsistentBlock 2
              [ppSyntax descriptionTag,
               ppSyntax ":",
               Print.newline,
               ppText desc],
            Print.newline,
            Print.newline,
            ppSection "library"
              [ppSection "build-depends:" (ppBuildDepends deps),
               Print.newline,
               ppTag ("hs-source-dirs","src"),
               Print.newline,
               ppTags tags [ghcOptionsTag],
               Print.newline,
               ppSection "exposed-modules:" (ppExposedModules mods)]] @
           (if List.null tests then []
            else
              [Print.newline,
               Print.newline,
               ppSection ("test-suite " ^ name ^ "-test")
                 [ppTag ("type","exitcode-stdio-1.0"),
                  Print.newline,
                  ppSection "build-depends:" (ppBuildDepends deps),
                  Print.newline,
                  ppTag ("hs-source-dirs","src"),
                  Print.newline,
                  ppTags tags [ghcOptionsTag],
                  Print.newline,
                  ppTag ("main-is","Test.hs")]]))
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Writing a Haskell package to disk.                                        *)
(* ------------------------------------------------------------------------- *)

local
  fun cabalFilename {directory = dir} name =
      let
        val base = Print.toLine ppPackageName name

        val file = OS.Path.joinBaseExt {base = base, ext = SOME "cabal"}

        val filename = OS.Path.joinDirFile {dir = dir, file = file}
      in
        {filename = filename}
      end;

  fun output dir name cabal =
      let
        val file = cabalFilename dir name

        val () = Stream.toTextFile file cabal
      in
        ()
      end;

  fun mkCabal (p,v,s) =
      let
        val l = "version: " ^ PackageVersion.toString v ^ "\n"
      in
        Stream.fromList (p @ l :: s)
      end;

  val destCabal =
      let
        fun findVersion p l =
            case l of
              [] => raise Error "no version found"
            | h :: t =>
              let
                val vo =
                    case total (destPrefix "version: ") h of
                      NONE => NONE
                    | SOME s =>
                      case total (destSuffix "\n") s of
                        NONE => NONE
                      | SOME v => total PackageVersion.fromString v
              in
                case vo of
                  NONE => findVersion (h :: p) t
                | SOME v => (p,v,t)
              end
      in
        fn cabal => findVersion [] (Stream.toList cabal)
      end;
in
  fun outputCabal {reexport} dir info deps src tests =
      let
        val name = nameInformation info
        and tags = mkTags info

        val cabal = Print.toStream ppCabal (tags,deps,src,tests)

        val rex = existsDirectory dir
      in
        if rex then
          let
            val cabal' = Stream.fromTextFile (cabalFilename dir name)

            val (p,v,s) = destCabal cabal
            and (p',v',s') = destCabal cabal'

(*OpenTheoryTrace
            val () = Print.trace PackageVersion.pp "Haskell.outputCabal.v" v
            and () = Print.trace PackageVersion.pp "Haskell.outputCabal.v'" v'

            val () = Print.trace (Print.ppList Print.ppString)
                       "Haskell.outputCabal.p" p
            and () = Print.trace (Print.ppList Print.ppString)
                       "Haskell.outputCabal.p'" p'

            val () = Print.trace (Print.ppList Print.ppString)
                       "Haskell.outputCabal.s" s
            and () = Print.trace (Print.ppList Print.ppString)
                       "Haskell.outputCabal.s'" s'
*)

(*OpenTheoryDebug
            val () =
                if PackageVersion.equal v (versionInformation info) then ()
                else raise Bug "Haskell.outputCabal: bad version"
*)
            val vo =
                case PackageVersion.compare (v',v) of
                  LESS => SOME v
                | EQUAL =>
                  if not reexport andalso p = p' andalso s = s' then NONE
                  else
                    let
                      val l = PackageVersion.toList v @ [1]
                    in
                      SOME (PackageVersion.fromList l)
                    end
                | GREATER =>
                  let
                    val l = PackageVersion.toList v
                    and l' = PackageVersion.toList v'

                    val n = List.length l

                    val ok = n < List.length l' andalso List.take (l',n) = l
                  in
                    if not ok then
                      let
                        val msg =
                            "existing Haskell package with version " ^
                            PackageVersion.toString v'

                        val () = if reexport then die msg else warn msg
                      in
                        NONE
                      end
                    else if not reexport andalso p = p' andalso s = s' then
                      NONE
                    else
                      let
                        val k = List.nth (l',n) + 1
                      in
                        SOME (PackageVersion.fromList (l @ [k]))
                      end
                  end
          in
            case vo of
              NONE => NONE
            | SOME version =>
              let
                val cabal = mkCabal (p,version,s)

                val () = nukeDirectoryFiles dir

                val () = output dir name cabal

                val tags = updateVersionTag tags version
              in
                SOME (rex,version,tags)
              end
          end
        else
          let
            val () = createDirectory dir

            val () = output dir name cabal

            val version = versionInformation info
          in
            SOME (rex,version,tags)
          end
      end;
end;

fun outputLicense sys {url} {directory} tags =
    let
      val licenseFile = licenseFileTags tags

      val file = OS.Path.joinDirFile {dir = directory, file = licenseFile}

      val {curl = cmd} = RepositorySystem.curl sys

      val cmd = cmd ^ " " ^ url ^ " --output " ^ file

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)
    in
      if OS.Process.isSuccess (OS.Process.system cmd) then ()
      else raise Error "downloading the license file failed"
    end;

local
  val setupContents =
      Stream.fromList
        ["module Main(main)\n",
         "\n",
         "import Distribution.Simple\n",
         "\n",
         "main :: IO ()\n",
         "main = defaultMain\n"];
in
  fun outputSetup {directory = dir} =
      let
        val file =
            let
              val f = OS.Path.joinBaseExt {base = "Setup", ext = SOME "hs"}
            in
              OS.Path.joinDirFile {dir = dir, file = f}
            end

        val () = Stream.toTextFile {filename = file} setupContents
      in
        ()
      end;
end;

local
  fun subNamespace parent ns =
      let
        val (nested,sub) = Namespace.destNested ns

        val () =
            if Namespace.equal parent nested then ()
            else raise Bug "Haskell.subNamespace"
      in
        sub
      end;

  fun outputSrc int tags {directory = dir} sub namespace src =
      let
        val ss = Print.toStream (ppModule int) (tags,namespace,src)

        val file =
            let
              val f = OS.Path.joinBaseExt {base = sub, ext = SOME "hs"}
            in
              OS.Path.joinDirFile {dir = dir, file = f}
            end

        val () = Stream.toTextFile {filename = file} ss
      in
        ()
      end;

  fun outputMod int tags dir parent module =
      let
        val Module {namespace,source,submodules} = module

        val sub = subNamespace parent namespace

        val () =
            if List.null source then ()
            else outputSrc int tags dir sub namespace source

        val () =
            if List.null submodules then ()
            else
              let
                val dir = mkSubDirectory dir sub
              in
                List.app (outputMod int tags dir namespace) submodules
              end
      in
        ()
      end;
in
  fun outputSource int dir tags module =
      let
        val Module {namespace,source,submodules} = module

        val () =
            if Namespace.isGlobal namespace then ()
            else raise Bug "Haskell.outputSource: not global namespace"

        val () =
            if List.null source then ()
            else raise Error "cannot export global definitions"

        val dir = mkSubDirectory dir "src"

        val () = List.app (outputMod int tags dir namespace) submodules
      in
        dir
      end;
end;

local
  fun outputMain int tags {directory = dir} tests =
      let
        val ss = Print.toStream (ppTests int) (tags,tests)

        val file =
            let
              val f = OS.Path.joinBaseExt {base = "Test", ext = SOME "hs"}
            in
              OS.Path.joinDirFile {dir = dir, file = f}
            end

        val () = Stream.toTextFile {filename = file} ss
      in
        ()
      end;
in
  fun outputTests int dir tags tests =
      if List.null tests then ()
      else outputMain int tags dir tests;
end;

fun writePackage rex haskell =
    let
      val Haskell
            {system = sys,
             information = info,
             depends = deps,
             interpretation = int,
             source = src,
             tests} = haskell

      val name = nameInformation info

      val dir =
          let
            val dir = {directory = OS.FileSys.getDir ()}

            val n = Print.toLine ppPackageName name
          in
            subDirectory dir n
          end
    in
      case outputCabal rex dir info deps src tests of
        NONE => (name,NONE)
      | SOME (reexport,version,tags) =>
        let
          val () =
              let
                val url = licenseUrlInformation info
              in
                outputLicense sys url dir tags
              end

          val () = outputSetup dir

          val srcdir = outputSource int dir tags src

          val () = outputTests int srcdir tags tests
        in
          (name, SOME ({reexport = reexport}, version))
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Exporting a theory package as a Haskell package.                          *)
(* ------------------------------------------------------------------------- *)

fun exportPackage rex repo prev namever =
    let
      val haskell = fromPackage repo prev namever
    in
      writePackage rex haskell
    end;

end
