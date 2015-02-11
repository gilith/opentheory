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

val authorTag = "author"
and buildTypeTag = "build-type"
and cabalVersionTag = "cabal-version"
and categoryTag = "category"
and descriptionTag = "description"
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

val mainNamespace = Namespace.fromList ["Main"];

(* Types *)

val boolTypeName = Name.mkGlobal "Bool"
and funTypeName = Name.mkGlobal "->"
and listTypeName = Name.mkGlobal "[]"
and pairTypeName = Name.mkGlobal ",";

(* Constants *)

val eqName = Name.mkGlobal "=="
and pairName = Name.mkGlobal ",";

(* Variables *)

val anonymousName = Name.mkGlobal "_";

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

  val partitionTags =
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

        val (htags,tags) = partitionTags tags
      in
        case peekTag PackageName.srcExtraTag htags of
          NONE => NONE
        | SOME (srcFile,htags) => SOME (mkInfo repo pkg tags srcFile htags)
      end;
end;

fun nameInformation (Information {name = x, ...}) = x;

fun licenseUrlInformation (Information {licenseUrl = x, ...}) = {url = x};

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

  fun addSymbol ths (s,sm) =
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
              raise Bug "Haskell.mkDepends.findSymbol: multiple definitions";

        val n = PackageNameVersion.name (PackageTheorems.nameVersion th)

        val ss = Option.getOpt (PackageNameMap.peek sm n, SymbolSet.empty)

        val ss = SymbolSet.add ss s
      in
        PackageNameMap.insert sm (n,ss)
      end;

  fun interpretSymbol repo sm (th,(nvs,syms)) =
      let
        val nv = PackageTheorems.nameVersion th

        val n = PackageNameVersion.name nv
      in
        case PackageNameMap.peek sm n of
          NONE => (nvs,syms)
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

            val hn = nameInformation info

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

            val sm = SymbolSet.map interpret ss
          in
            (nv :: nvs, PackageNameMap.insert syms (n,(hn,sm)))
          end
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

        val (hn,sm) =
            case PackageNameMap.peek sym n of
              NONE => raise Bug "Haskell.mkDepends.checkSymbol"
            | SOME x => x
      in
        PackageName.equal (nameInformation info) hn andalso
        SymbolMap.all check sm
      end;

  fun mkSymbol repo ths sym =
      let
        val sym =
            SymbolSet.difference
              (SymbolTable.symbols (SymbolTable.undefined sym))
              SymbolSet.primitives

        val sm = PackageNameMap.new ()

        val sm = SymbolSet.foldl (addSymbol ths) sm sym

        val syms = PackageNameMap.new ()
      in
        List.foldl (interpretSymbol repo sm) ([],syms) (List.rev ths)
      end;

  fun destSymbol sym =
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

        fun addName (_,(_,sm),rws) = SymbolMap.foldr addSym rws sm

        val rws = PackageNameMap.foldr addName [] sym
      in
        Interpretation.fromRewriteList rws
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

  fun mkOldest repo sym =
      let
        fun check nv vs =
            case Repository.previousNameVersion repo nv of
              NONE => NONE
            | SOME nv =>
              let
                val pkg = Repository.get repo nv

                val th = Package.theorems pkg
              in
                (* If Haskell export was not set up for this version *)
                (* then we don't need to check the dependencies *)
                case mkInformation repo pkg of
                  NONE => SOME (nv,vs)
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

              val hn =
                  case PackageNameMap.peek sym n of
                    SOME (x,_) => x
                  | NONE => raise Bug "Haskell.mkDepends.destOldest.mk: sym"

              val old =
                  case PackageNameMap.peek oldest n of
                    SOME x => x
                  | NONE => raise Bug "Haskell.mkDepends.destOldest.mk: old"
            in
              Depend
                {name = hn,
                 oldest = old,
                 newest = new}
            end
      in
        List.map mk
      end;
in
  fun mkDepends repo reqs thy sym =
      let
        val ths =
            case Repository.requiresTheorems repo reqs of
              SOME ths => ths
            | NONE => raise Error "required theories not installed"

        val vs = PackageTheorems.mkVersions (Theory.summary thy) ths

        val (nvs,sym) = mkSymbol repo ths sym

        val oldest = mkOldest repo sym (Queue.fromList nvs) vs

        val deps = destOldest sym oldest nvs
        and int = destSymbol sym
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
       caseConst : Const.const};

datatype newtype =
    Newtype of
      {name : TypeOp.typeOp,
       parameters : Name.name list,
       repType : Type.ty,
       abs : Const.const,
       rep : Const.const};

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

datatype source =
    DataSource of data
  | NewtypeSource of newtype
  | ValueSource of value;

(* ------------------------------------------------------------------------- *)
(* Symbols in source declarations.                                           *)
(* ------------------------------------------------------------------------- *)

(* The primary defined symbol in a source declaration *)

fun symbolData (Data {name,...}) = Symbol.TypeOp name;

fun symbolNewtype (Newtype {name,...}) = Symbol.TypeOp name;

fun symbolValue (Value {name,...}) = Symbol.Const name;

fun symbolSource s =
    case s of
      DataSource x => symbolData x
    | NewtypeSource x => symbolNewtype x
    | ValueSource x => symbolValue x;

fun nameSource s = Symbol.name (symbolSource s);

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
               caseConst} = data

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
             rep} = newtype

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

fun definedSymbolTableSource s =
    case s of
      DataSource x => definedSymbolTableData x
    | NewtypeSource x => definedSymbolTableNewtype x
    | ValueSource x => definedSymbolTableValue x;

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
               caseConst} = data

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
             rep} = newtype

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

fun symbolTableSource s =
    case s of
      DataSource x => symbolTableData x
    | NewtypeSource x => symbolTableNewtype x
    | ValueSource x => symbolTableValue x;

fun symbolTableSourceList sl =
    SymbolTable.unionList (List.map symbolTableSource sl);

(* All symbols that explicitly appear in source declarations (type *)
(* signatures of "where" values are not included in this, because *)
(* they are commented out in the generated Haskell code). *)

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
end;

fun explicitSymbolTableSource s =
    case s of
      DataSource x => explicitSymbolTableData x
    | NewtypeSource x => explicitSymbolTableNewtype x
    | ValueSource x => explicitSymbolTableValue x;

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

fun destData th =
    let
      val Sequent.Sequent {hyp,concl} = Thm.sequent th

      val () =
          if TermAlphaSet.null hyp then ()
          else raise Error "hypotheses"

      val {dataType,constructors,caseConst} = Term.destCaseDef concl

      val (name,parms) = Type.destOp dataType

      val parms = List.map Type.destVar parms
    in
      Data
        {name = name,
         parameters = parms,
         constructors = constructors,
         caseConst = caseConst}
    end
    handle Error err =>
      raise Error ("bad data theorem: " ^ err);

fun destNewtype int th =
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
    in
      Newtype
        {name = name,
         parameters = parms,
         repType = repTy,
         abs = abs,
         rep = rep}
    end
    handle Error err =>
      raise Error ("bad newtype theorem: " ^ err);

local
  fun destEqn tm =
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
  fun destValue th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val (fns,eqns) = unzip (List.map destEqn (Term.stripConj concl))

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

fun destSource int th =
    let
      val dataResult =
          Left (destData th)
          handle Error err => Right err

      val newtypeResult =
          Left (destNewtype int th)
          handle Error err => Right err

      val valueResult =
          Left (destValue th)
          handle Error err => Right err
    in
      case (dataResult,newtypeResult,valueResult) of
        (Left x, _, _) => DataSource x
      | (Right _, Left x, _) => NewtypeSource x
      | (Right _, Right _, Left x) => ValueSource x
      | (Right e1, Right e2, Right e3) =>
        let
          val err =
              "bad source theorem:\n  " ^ e1 ^ "\n  " ^ e2 ^ "\n  " ^ e3 ^
              "\n" ^ Print.toString Thm.pp th
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

fun symbolTableModule module =
    let
      val Module {namespace = _, source, submodules} = module

      val sym = symbolTableSourceList source

      val syml = List.map symbolTableModule submodules
    in
      SymbolTable.unionList (sym :: syml)
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
  val mkSymSrc =
      let
        fun mk src = (symbolSource src, src)
      in
        fn srcl => SymbolMap.fromList (List.map mk srcl)
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
      in
        List.foldl addScc []
      end;
in
  fun sortSource srcl =
      let
        val symSrc = mkSymSrc srcl

        val syms = SymbolSet.domain symSrc

        val graph = mkGraph syms symSrc

        val sccl = SymbolGraph.preOrderSCC graph (SymbolGraph.vertices graph)
      in
        linearize symSrc sccl
      end;
end;

local
  fun init int src =
      let
        val n =
            case symbolSource src of
              Symbol.TypeOp t =>
              Interpretation.interpretTypeOp int (TypeOp.name t)
            | Symbol.Const c =>
              Interpretation.interpretConst int (Const.name c)

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

fun mkSource int src =
    let
      val ths = ThmSet.toList (Thms.thms src)

      val src = List.map (destSource int) ths

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

fun destTests show =
    let
      fun ppTest (kind,n,test) =
          Print.inconsistentBlock 2
            [Print.ppString (capitalize kind),
             Print.ppString " ",
             Print.ppInt n,
             Print.ppString ":",
             Print.newline,
             Term.ppWithShow show test]

      fun destTest al pl th =
          let
            val Sequent.Sequent {hyp,concl} = Thm.sequent th

            val () =
                if TermAlphaSet.null hyp then ()
                else raise Error "hypotheses"

            val () =
                if VarSet.null (Term.freeVars concl) then ()
                else raise Error "free variables"

            val (vs,body) = Term.stripForall concl

            val args = map Term.mkVar vs

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

      fun dest al pl ths =
          case ths of
            [] => List.revAppend (al, List.rev pl)
          | th :: ths =>
            let
              val (al,pl) = destTest al pl th
            in
              dest al pl ths
            end
    in
      dest [] []
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
in
  fun fromPackage repo namever =
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

        val src =
            let
              val file = Package.joinDirectory pkg {filename = srcFile}

              val ths = derivedTheorems thy file
            in
              mkSource thyInt ths
            end

        val tests =
            case testFile of
              NONE => []
            | SOME filename =>
              let
                val file = Package.joinDirectory pkg {filename = filename}

                val ths = Thms.thms (derivedTheorems thy file)

                val tests = destTests (Package.show pkg) (ThmSet.toList ths)

                val () =
                    if not (List.null tests) then ()
                    else raise Error ("no tests defined in " ^ filename)
              in
                tests
              end

        val (deps,depInt) =
            let
              val reqs = Package.requires pkg

              val sym =
                  SymbolTable.union
                    (symbolTableModule src)
                    (symbolTableTests tests)
            in
              mkDepends repo reqs thy sym
            end

        val int =
            let
              val rewrs =
                  primitiveRewrites @
                  Interpretation.toRewriteList depInt @
                  Interpretation.toRewriteList thyInt
            in
              Interpretation.fromRewriteList rewrs
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

              val n = Interpretation.interpretConst int n

              val n =
                  case total Name.destCase n of
                    SOME (n,_) => n
                  | NONE => n
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
            in
              abbreviateNamespaces (NamespaceSet.difference white black)
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

(***
local
  local
    fun mkName root ns =
        curry Name.mk (Namespace.append root (Namespace.fromList ns));

    val primitiveRoot = Namespace.fromList ["OpenTheory"];

    val mkPrimitive = mkName primitiveRoot;
  in
    val mkNative = mkName Namespace.global [];

    val mkPrimitiveByte = mkPrimitive ["Byte"]
    and mkPrimitiveNatural = mkPrimitive ["Natural"]
    and mkPrimitiveRandom = mkPrimitive ["Random"]
    and mkPrimitiveWord16 = mkPrimitive ["Word16"];
  end;

  val typeOpMapping =
      List.map Interpretation.TypeOpRewrite
        [(* Native types *)
         (Name.optionTypeOp, mkNative "Maybe"),
         (* Primitive types *)
         (Name.byteTypeOp, mkPrimitiveByte "Byte"),
         (Name.naturalTypeOp, mkPrimitiveNatural "Natural"),
         (Name.randomTypeOp, mkPrimitiveRandom "Random"),
         (Name.word16TypeOp, mkPrimitiveWord16 "Word16")];

  val constMapping =
      List.map Interpretation.ConstRewrite
        [(* Native constants *)
         (Name.addByteConst, mkNative "+"),
         (Name.addWord16Const, mkNative "+"),
         (Name.appendStreamConst, mkNative "++"),
         (Name.consStreamConst, mkNative ":"),
         (Name.headStreamConst, mkNative "head"),
         (Name.leByteConst, mkNative "<="),
         (Name.leWord16Const, mkNative "<="),
         (Name.ltByteConst, mkNative "<"),
         (Name.ltWord16Const, mkNative "<"),
         (Name.mapStreamConst, mkNative "map"),
         (Name.multiplyByteConst, mkNative "*"),
         (Name.multiplyWord16Const, mkNative "*"),
         (Name.noneConst, mkNative "Nothing"),
         (Name.someConst, mkNative "Just"),
         (Name.subtractByteConst, mkNative "-"),
         (Name.subtractWord16Const, mkNative "-"),
         (Name.tailStreamConst, mkNative "tail"),
         (* Primitive constants *)
         (Name.andByteConst, mkPrimitiveByte "and"),
         (Name.andWord16Const, mkPrimitiveWord16 "and"),
         (Name.bitConst, mkPrimitiveRandom "bit"),
         (Name.bitByteConst, mkPrimitiveByte "bit"),
         (Name.bitWord16Const, mkPrimitiveWord16 "bit"),
         (Name.fromBytesWord16Const, mkPrimitiveWord16 "fromBytes"),
         (Name.fromNaturalByteConst, mkPrimitiveByte "fromNatural"),
         (Name.fromNaturalWord16Const, mkPrimitiveWord16 "fromNatural"),
         (Name.notByteConst, mkPrimitiveByte "not"),
         (Name.notWord16Const, mkPrimitiveWord16 "not"),
         (Name.orByteConst, mkPrimitiveByte "or"),
         (Name.orWord16Const, mkPrimitiveWord16 "or"),
         (Name.shiftLeftByteConst, mkPrimitiveByte "shiftLeft"),
         (Name.shiftLeftWord16Const, mkPrimitiveWord16 "shiftLeft"),
         (Name.shiftRightByteConst, mkPrimitiveByte "shiftRight"),
         (Name.shiftRightWord16Const, mkPrimitiveWord16 "shiftRight"),
         (Name.splitConst, mkPrimitiveRandom "split"),
         (Name.toBytesWord16Const, mkPrimitiveWord16 "toBytes")];
in
  val primitiveInt =
      Interpretation.fromRewriteList
        (typeOpMapping @ constMapping);
end;
***)

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
      case explode (Name.destGlobal n) of
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
          Print.program (map Print.ppChar (Char.toLower c :: cs))
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
    in
      Print.program
        (ppPackageName name ::
         (if PackageVersion.equal newest oldest then
            [ppSyntax " == ",
             PackageVersion.pp newest]
          else
            [ppSyntax " >= ",
             PackageVersion.pp oldest,
             ppSyntax " && <= ",
             PackageVersion.pp newest]))
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
               caseConst = _} = data
      in
        Print.inconsistentBlock 2
          [ppDecl exp (name,parms),
           ppCons exp cons]
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
               rep} = newtype
      in
        Print.inconsistentBlock 2
          [ppDecl exp (name,parms),
           Print.break,
           ppIso exp (abs,(rep,repType))]
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

  fun ppDecl ns (tm,ty) =
      Print.inconsistentBlock 2
        [ppTerm ns tm,
         ppSyntax " ::",
         Print.break,
         ppType ns ty];

  fun ppEquation ns tm eqn =
      let
        val Equation {arguments = args, body = rtm, whereValues = values} = eqn

        val args = List.map (anonymize (bodiesEquation eqn)) args

        val ltm = Term.listMkApp (tm,args)
      in
        Print.consistentBlock 2
          (ppTerm ns ltm ::
           ppSyntax " =" ::
           Print.break ::
           ppTerm ns rtm ::
           ppWherevalues ns values)
      end

  and ppWherevalues ns values =
      let
        fun ppSpaceVal value =
            Print.sequence (Print.newlines 2) (ppWhereValue ns value)
      in
        case values of
          [] => []
        | value :: values =>
          [Print.newline,
           Print.consistentBlock 0
             (ppSyntax "where" ::
              Print.newline ::
              ppWhereValue ns value ::
              List.map ppSpaceVal values)]
      end

  and ppWhereValue ns value =
      let
        val WhereValue {name, equations = eqns} = value

        val tm = Term.mkVar name
        and ty = Var.typeOf name
      in
        Print.inconsistentBlock 2
          (Print.ppBracket "{-" "-}" (ppDecl ns) (tm,ty) ::
           List.map (Print.sequence Print.newline o ppEquation ns tm) eqns)
      end;
in
  fun ppValue ns value =
      let
        val Value {name, ty, equations = eqns} = value

        val tm = Term.mkConst (name,ty)
      in
        Print.inconsistentBlock 0
          (ppDecl ns (tm,ty) ::
           List.map (Print.sequence Print.newline o ppEquation ns tm) eqns)
    end;
end;

fun ppSource ns s =
    case s of
      DataSource x => ppData ns x
    | NewtypeSource x => ppNewtype ns x
    | ValueSource x => ppValue ns x;

local
  fun ppSpaceSource ns s =
      Print.sequence
        (Print.sequence Print.newline Print.newline)
        (ppSource ns s);
in
  fun ppSourceList ns sl =
      case sl of
        [] => Print.skip
      | s :: sl =>
        Print.program (ppSource ns s :: List.map (ppSpaceSource ns) sl);
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
        ppSyntax "random >= 1.0.1.1 && < 2.0," ::
        Print.newline ::
        ppSyntax "QuickCheck >= 2.4.0.1 && < 3.0," ::
        Print.newline ::
        ppSyntax "opentheory-primitive >= 1.0 && < 2.0" ::
        List.map ppExtraDepend deps;
  end;

  fun ppExposedModules mods =
      case NamespaceSet.toList mods of
        [] => []
      | ns :: nss =>
        ppNamespace ns ::
        List.map (Print.sequence Print.newline o ppNamespace) nss;

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
               Print.newline,
               ppTag ("hs-source-dirs","src"),
               Print.newline,
               Print.newline,
               ppTags tags [ghcOptionsTag],
               Print.newline,
               Print.newline,
               ppSection "exposed-modules:" (ppExposedModules mods)]] @
           (if List.null tests then []
            else
              [Print.newline,
               Print.newline,
               ppSection ("executable " ^ name ^ "-test")
                 [ppSection "build-depends:" (ppBuildDepends deps),
                  Print.newline,
                  Print.newline,
                  ppTag ("hs-source-dirs","src, testsrc"),
                  Print.newline,
                  Print.newline,
                  ppTags tags [ghcOptionsTag],
                  Print.newline,
                  Print.newline,
                  ppTag ("main-is","Main.hs")]]))
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Writing a Haskell package to disk.                                        *)
(* ------------------------------------------------------------------------- *)

fun mkSubDirectory {directory = dir} sub =
    let
      val dir = OS.Path.concat (dir,sub)

      val () = OS.FileSys.mkDir dir
    in
      {directory = dir}
    end;

fun outputCabal {directory = dir} tags deps src tests =
    let
      val ss = Print.toStream ppCabal (tags,deps,src,tests)

      val file =
          let
            val name = PackageName.fromString (nameTags tags)

            val base = Print.toLine ppPackageName name

            val f = OS.Path.joinBaseExt {base = base, ext = SOME "cabal"}
          in
            OS.Path.joinDirFile {dir = dir, file = f}
          end

      val () = Stream.toTextFile {filename = file} ss
    in
      ()
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
      in
        List.app (outputMod int tags dir namespace) submodules
      end;
end;

local
  fun outputMain int tags {directory = dir} tests =
      let
        val ss = Print.toStream (ppTests int) (tags,tests)

        val file =
            let
              val f = OS.Path.joinBaseExt {base = "Main", ext = SOME "hs"}
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
      else
        let
          val dir = mkSubDirectory dir "testsrc"
        in
          outputMain int tags dir tests
        end;
end;

fun writePackage haskell =
    let
      val Haskell
            {system = sys,
             information = info,
             depends = deps,
             interpretation = int,
             source = src,
             tests} = haskell

      val tags = mkTags info

      val dir =
          let
            val name = Print.toLine ppPackageName (nameInformation info)

            val dir = {directory = OS.FileSys.getDir ()}
          in
            mkSubDirectory dir name
          end

      val () = outputCabal dir tags deps src tests

      val () =
          let
            val url = licenseUrlInformation info
          in
            outputLicense sys url dir tags
          end

      val () = outputSetup dir

      val () = outputSource int dir tags src

      val () = outputTests int dir tags tests
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Exporting a theory package as a Haskell package.                          *)
(* ------------------------------------------------------------------------- *)

fun exportPackage repo namever =
    let
      val haskell = fromPackage repo namever

      val () = writePackage haskell
    in
      name haskell
    end;

end
