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
and provenanceTag = "provenance"
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
(* Anonymizing unused variables.                                             *)
(* ------------------------------------------------------------------------- *)

local
  val anonymous = Name.mkGlobal "_";
in
  fun anonymize tms =
      let
        val fvs = Term.freeVarsList tms

        fun anonVar v =
            if VarSet.member v fvs then NONE
            else SOME (v, Term.mkVar (Var.mk (anonymous, Var.typeOf v)))
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
end;

(* ------------------------------------------------------------------------- *)
(* Exporting various OpenTheory names to Haskell.                            *)
(* ------------------------------------------------------------------------- *)

val exportPackageName = PackageName.mkHaskellName;

fun exportTypeOpName int n = Interpretation.interpretTypeOp int n;

fun exportConstName int n = Interpretation.interpretConst int n;

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
         (Name.boolTypeOp, mkNative "Bool"),
         (Name.funTypeOp, mkNative "->"),
         (Name.listTypeOp, mkNative "List"),
         (Name.optionTypeOp, mkNative "Maybe"),
         (Name.pairTypeOp, mkNative "Pair"),
         (Name.streamTypeOp, mkNative "List"),
         (* Primitive types *)
         (Name.byteTypeOp, mkPrimitiveByte "Byte"),
         (Name.naturalTypeOp, mkPrimitiveNatural "Natural"),
         (Name.randomTypeOp, mkPrimitiveRandom "Random"),
         (Name.word16TypeOp, mkPrimitiveWord16 "Word16")];

  val constMapping =
      List.map Interpretation.ConstRewrite
        [(* Native constants *)
         (Name.addConst, mkNative "+"),
         (Name.addByteConst, mkNative "+"),
         (Name.addWord16Const, mkNative "+"),
         (Name.allConst, mkNative "all"),
         (Name.anyConst, mkNative "any"),
         (Name.appendConst, mkNative "++"),
         (Name.appendStreamConst, mkNative "++"),
         (Name.concatConst, mkNative "concat"),
         (Name.conjConst, mkNative "&&"),
         (Name.consConst, mkNative ":"),
         (Name.consStreamConst, mkNative ":"),
         (Name.disjConst, mkNative "||"),
         (Name.divConst, mkNative "div"),
         (Name.eqConst, mkNative "=="),
         (Name.falseConst, mkNative "False"),
         (Name.fstConst, mkNative "fst"),
         (Name.headConst, mkNative "head"),
         (Name.headStreamConst, mkNative "head"),
         (Name.leConst, mkNative "<="),
         (Name.leByteConst, mkNative "<="),
         (Name.leWord16Const, mkNative "<="),
         (Name.ltConst, mkNative "<"),
         (Name.ltByteConst, mkNative "<"),
         (Name.ltWord16Const, mkNative "<"),
         (Name.mapConst, mkNative "map"),
         (Name.mapStreamConst, mkNative "map"),
         (Name.modConst, mkNative "mod"),
         (Name.multiplyConst, mkNative "*"),
         (Name.multiplyByteConst, mkNative "*"),
         (Name.multiplyWord16Const, mkNative "*"),
         (Name.negConst, mkNative "not"),
         (Name.nilConst, mkNative "[]"),
         (Name.noneConst, mkNative "Nothing"),
         (Name.pairConst, mkNative ","),
         (Name.sndConst, mkNative "snd"),
         (Name.someConst, mkNative "Just"),
         (Name.subtractConst, mkNative "-"),
         (Name.subtractByteConst, mkNative "-"),
         (Name.subtractWord16Const, mkNative "-"),
         (Name.tailConst, mkNative "tail"),
         (Name.tailStreamConst, mkNative "tail"),
         (Name.trueConst, mkNative "True"),
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
                  (exportPackageName n, htags)
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
              val tag = PackageName.fromString provenanceTag

              val () = checkNoTag tag htags

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
         interpretation = interpretation}
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

  fun interpretSymbol repo sm (th,(ths,syms)) =
      let
        val nv = PackageTheorems.nameVersion th

        val n = PackageNameVersion.name nv
      in
        case PackageNameMap.peek sm n of
          NONE => (ths,syms)
        | SOME ss =>
          let
            val pkg =
                case Repository.peek repo nv of
                  SOME p => p
                | NONE => raise Bug "Haskell.mkDepends.interpretSymbol.pkg"

            val {information = _, srcFilename = _, interpretation = int} =
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

            val sm = SymbolSet.map interpret ss
          in
            (th :: ths, PackageNameMap.insert syms (n,sm))
          end
      end;

  fun checkSymbol sym th int =
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

        val sm =
            case PackageNameMap.peek sym n of
              NONE => raise Bug "Haskell.mkDepends.checkSymbol"
            | SOME x => x
      in
        SymbolMap.all check sm
      end;

  fun mkSymbol repo ths sym =
      let
        val sym = SymbolTable.symbols (SymbolTable.undefined sym)

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

        fun addName (_,sm,rws) = SymbolMap.foldr addSym rws sm

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
                (* If the Haskell export was not set up for this version then *)
                (* we don't need to check the theory constraints *)
                case mkInformation repo pkg of
                  NONE => SOME (th,vs)
                | SOME info =>
                  let
                    val {information = _,
                         srcFilename = _,
                         interpretation = int} = info
                  in
                    case total (PackageTheorems.addVersion vs) th of
                      NONE => NONE
                    | SOME vs =>
                      if not (checkSymbol sym th int) then NONE
                      else SOME (th,vs)
                  end
              end

        fun push oldest ths vs =
            if Queue.null ths then oldest
            else
              let
                val (th,ths) = Queue.hdTl ths

                val nv = PackageTheorems.nameVersion th
              in
                case check nv vs of
                  NONE =>
                  let
                    val oldest = recordOldest oldest nv
                  in
                    push oldest ths vs
                  end
                | SOME (th,vs) =>
                  let
                    val ths = Queue.add th ths
                  in
                    push oldest ths vs
                  end
              end
      in
        push (PackageNameMap.new ())
      end;

  fun destOldest oldest =
      let
        fun mk th =
            let
              val nv = PackageTheorems.nameVersion th

              val n = PackageNameVersion.name nv
              and new = PackageNameVersion.version nv

              val old =
                  case PackageNameMap.peek oldest n of
                    SOME v => v
                  | NONE => raise Bug "Haskell.mkDepends.mk"
            in
              Depend
                {name = n,
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

        val (ths,sym) = mkSymbol repo ths sym

        val oldest = mkOldest repo sym (Queue.fromList ths) vs

        val deps = destOldest oldest ths
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

fun namespaceSource s = Name.namespace (nameSource s);

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

(* All symbols in uncommented-out source declarations, which means skipping *)
(* symbols that appear only in the types of where declarations. *)

val uncommentedSymbolTableData = symbolTableData;

val uncommentedSymbolTableNewtype = symbolTableNewtype;

local
  fun destSpecialTerm tm =
      if Term.isNumeral tm then SOME []
      else
        case total Term.destCond tm of
          SOME (c,a,b) => SOME [c,a,b]
        | NONE =>
          case total Term.destLet tm of
            SOME (v,x,y) => SOME [v,x,y]
          | NONE =>
            case total Term.destGenAbs tm of
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
  fun uncommentedSymbolTableValue value =
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

fun uncommentedSymbolTableSource s =
    case s of
      DataSource x => uncommentedSymbolTableData x
    | NewtypeSource x => uncommentedSymbolTableNewtype x
    | ValueSource x => uncommentedSymbolTableValue x;

fun uncommentedSymbolTableSourceList sl =
    SymbolTable.unionList (List.map uncommentedSymbolTableSource sl);

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

fun destNewtype th =
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

        val () =
            if not (Name.isCase (Const.name name)) then ()
            else raise Error "case constant"

        val () =
            case total (Type.destOp o Type.rangeFun) ty of
              NONE => ()
            | SOME (ot,_) =>
              let
                val tn = TypeOp.name ot
                and cn = Const.name name
              in
                if not (Name.equal cn tn) then ()
                else raise Error "newtype constructor constant"
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

fun destSource th =
    let
      val dataResult =
          Left (destData th)
          handle Error err => Right err

      val newtypeResult =
          Left (destNewtype th)
          handle Error err => Right err

      val valueResult =
          Left (destValue th)
          handle Error err => Right err
    in
      case (dataResult,newtypeResult,valueResult) of
        (Left x, Right _, Right _) => DataSource x
      | (Right _, Left x, Right _) => NewtypeSource x
      | (Right _, Right _, Left x) => ValueSource x
      | (Right e1, Right e2, Right e3) =>
        let
          val err =
              "bad source theorem:\n  " ^ e1 ^ "\n  " ^ e2 ^ "\n  " ^ e3 ^
              "\n" ^ Print.toString Thm.pp th
        in
          raise Error err
        end
      | (x1,x2,x3) =>
        let
          val bug =
              "ambiguous source theorem:\n"

          val bug =
              case x1 of
                Left _ => bug
              | Right e => bug ^ "  " ^ e ^ "\n"

          val bug =
              case x2 of
                Left _ => bug
              | Right e => bug ^ "  " ^ e ^ "\n"

          val bug =
              case x3 of
                Left _ => bug
              | Right e => bug ^ "  " ^ e ^ "\n"

          val bug = bug ^ Print.toString Thm.pp th
        in
          raise Bug bug
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

        fun addRewrite (value,(values,rewr)) =
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

              val name = Name.mkGlobal (Name.component (Const.name name))

              fun mkVar t = Var.mk (name,t)

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
        val sym = addSymbolTableValue sym value
        and subs = subs @ [value]
      in
        (sym,eqn,subs)
      end;

  fun pullValues (src,(values,others)) =
      case src of
        ValueSource value =>
        let
          val Value {name,...} = value

          val name = Const.name name

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

  fun checkValue eqns value =
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
        val Value {name, ty = _, equations = _} = value

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
              | value :: _ =>
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

  fun mergeSubvalues values name =
      let
        val (value,subvalues) = NameMap.get values name

        val Value {name, ty, equations = eqns} = value

        val eqns = addValues subvalues eqns

        val value = Value {name = name, ty = ty, equations = eqns}
      in
        value
      end;

  fun groupValues (name,_,(values,acc)) =
      let
        val value = mergeSubvalues values name

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
            val subvalues' = value :: subvalues'

            val values = NameMap.insert values (name',(value',subvalues'))
          in
            (values,acc)
          end
      end;
in
  fun groupSource src =
      let
        val values = NameMap.new ()

        val (values,src) = List.foldl pullValues (values,[]) (List.rev src)

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
  fun init src = (Namespace.toList (namespaceSource src), src);

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
  fun mkModule source =
      let
        val namespace = Namespace.global
        and source = List.map init source
      in
        mkTree namespace source
      end;
end;

fun mkSource src =
    let
      val ths = ThmSet.toList (Thms.thms src)

      val src = List.map destSource ths

      val src = groupSource src

      val src = sortSource src
    in
      mkModule src
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

fun symbolTableModule module =
    let
      val Module {namespace = _, source, submodules} = module

      val sym = symbolTableSourceList source

      val syml = List.map symbolTableModule submodules
    in
      SymbolTable.unionList (sym :: syml)
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

(***
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

            val isAssert = not (Term.isForall concl)

            val (args,body) =
                if isAssert then ([],concl)
                else
                  let
                    val (v,body) = Term.destForall concl

                    val () =
                        if Type.isRandom (Var.typeOf v) then ()
                        else raise Error "bad quantified variable type"

                    val arg = Term.mkVar v
                  in
                    ([arg],body)
                  end

            val eqn =
                Equation
                  {arguments = args,
                   body = body,
                   whereValues = []}

            val n = length (if isAssert then al else pl)

            val kind = if isAssert then "assertion" else "proposition"

            val name = kind ^ Int.toString n

            val const = Const.mkUndef (Name.mk (haskellTestNamespace,name))

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
***)

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

(***
fun destTestTheory show test =
    let
      val art = Theory.article test

      val ths = ThmSet.toList (Thms.thms (Article.thms art))

      val tests = destTests show ths

      val () =
          if not (List.null tests) then ()
          else raise Error "no tests defined"
    in
      tests
    end;
***)

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

      val {information = info, srcFilename, interpretation = int} =
          case mkInformation repo pkg of
            SOME x => x
          | NONE =>
            let
              val err = "no haskell-src-file information"
            in
              raise Error err
            end

      val (_,thy) =
          let
            val sav = false

            val fndr = Repository.finder repo

            val graph = TheoryGraph.empty {savable = sav}

            val imps = TheorySet.empty
          in
            TheoryGraph.importPackage fndr graph
              {imports = imps,
               interpretation = int,
               package = pkg}
          end

      val src =
          let
            val sav = false

            val imp = Theory.article thy

            val {filename} = Package.joinDirectory pkg {filename = srcFilename}

            val art =
                Article.fromTextFile
                  {savable = sav,
                   import = imp,
                   interpretation = int,
                   filename = filename}

            val ths = Article.thms art
          in
            mkSource ths
          end

      val tests = []  (*** destTestTheory (Package.show pkg) test ***)

      val (deps,int) =
          let
            val reqs = Package.requires pkg

            val sym = symbolTableModule src
          in
            mkDepends repo reqs thy sym
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

(* ------------------------------------------------------------------------- *)
(* Creating the symbol name mapping in preparation for printing.             *)
(* ------------------------------------------------------------------------- *)

datatype symbolExport =
    SymbolExport of
      {interpretation : Interpretation.interpretation,
       namespace : Namespace.namespace,
       importNamespaces : Namespace.namespace option NamespaceMap.map,
       exportTypeOps : Name.name NameMap.map,
       exportConsts : Name.name NameMap.map};

fun namespaceSymbolExport (SymbolExport {namespace = ns, ...}) = ns;

fun importNamespacesSymbolExport exp =
    let
      val SymbolExport {importNamespaces,...} = exp
    in
      NamespaceMap.toList importNamespaces
    end;

local
  fun shortenName namespace imp n =
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
               importNamespaces = imp,
               exportTypeOps,
               ...} = exp

        val n =
            case NameMap.peek exportTypeOps n of
              SOME n => n
            | NONE => exportTypeOpName int n
      in
        shortenName ns imp n
      end;

  fun constNameSymbolExport exp n =
      let
        val SymbolExport
              {interpretation = int,
               namespace = ns,
               importNamespaces = imp,
               exportConsts,
               ...} = exp

        val n =
            case NameMap.peek exportConsts n of
              SOME n => n
            | NONE => exportConstName int n
      in
        shortenName ns imp n
      end;
end;

local
  val targetNamespaces =
      let
        fun add (_,t,s) = NamespaceSet.add s (Name.namespace t)
      in
        NameMap.foldl add NamespaceSet.empty
      end;

  fun addTypeOp (t,ns) =
      let
        val n = TypeOp.name t
      in
        NameSet.add ns n
      end;

  fun mkTypeOpMap int table =
      let
        val ts = SymbolTable.typeOps table

        val ns = TypeOpSet.foldl addTypeOp NameSet.empty ts
      in
        NameSet.map (exportTypeOpName int) ns
      end;

  fun addConst (c,ns) =
      let
        val n = Const.name c
      in
        case total Name.destCase n of
          SOME (_,nl) => NameSet.addList ns nl
        | NONE => NameSet.add ns n
      end;

  fun mkConstMap int table =
      let
        val cs = SymbolTable.consts table

        val ns = ConstSet.foldl addConst NameSet.empty cs
      in
        NameSet.map (exportConstName int) ns
      end;

  fun shortenNamespaces namespace =
      let
        val shorten = Namespace.rewrite (namespace,Namespace.global)
      in
        NamespaceSet.map shorten
      end;
in
  fun mkSymbolExport int namespace source =
      let
        val liveTable = uncommentedSymbolTableSourceList source
        and defTable = definedSymbolTableSourceList source

        val ts = mkTypeOpMap int liveTable
        and cs = mkConstMap int liveTable

        val white =
            NamespaceSet.union
              (targetNamespaces ts)
              (targetNamespaces cs)

        val black =
            NamespaceSet.add
              (targetNamespaces (mkConstMap int defTable))
              Namespace.global

        val ns =
            shortenNamespaces namespace (NamespaceSet.difference white black)
      in
        SymbolExport
          {interpretation = int,
           namespace = namespace,
           importNamespaces = ns,
           exportTypeOps = ts,
           exportConsts = cs}
      end;
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
               description,
               author,
               license,
               licenseUrl = _,
               provenance,
               tags = otags} = info

        val name = PackageName.toString name
        and version = PackageVersion.toString version
        and author = PackageAuthor.toString author
        and provenance =
            "automatically generated from the OpenTheory package " ^
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
               (provenanceTag,provenance),
               (stabilityTag,"provisional"),
               (synopsisTag,description),
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

(* Names *)

val ppPackageName = PackageName.pp;

fun ppPackageTestName name =
    Print.sequence
      (ppPackageName name)
      (Print.ppString "-test");

fun ppNamespace ns = Namespace.pp ns;

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

  fun ppModuleImport exp =
      let
        val import = importNamespacesSymbolExport exp
      in
        Print.inconsistentBlock 0
          (List.map ppImportNamespace import)
      end;

  fun ppTypeOpName exp n =
      Name.pp (typeOpNameSymbolExport exp n);

  fun ppConstName exp n =
      let
        val n = constNameSymbolExport exp n
      in
        if not (isSymbolName n) then Name.pp n
        else Print.ppBracket "(" ")" Name.pp n
      end;
end;

fun ppVarName n =
    if Name.isGlobal n then Name.pp n
    else raise Error "non-global variable name";

(* Types *)

fun ppTypeOp exp ot = ppTypeOpName exp (TypeOp.name ot);

val ppTypeVar = Name.pp;

val ppTypeVarList =
    let
      fun ppSpaceTypeVar v = Print.sequence Print.space (ppTypeVar v)
    in
      fn vl => Print.program (List.map ppSpaceTypeVar vl)
    end;

local
  fun destList ty =
      Type.destList ty
      handle Error _ => Type.destStream ty;

  val isList = can destList;

  fun destApp ty =
      if Type.isFun ty then raise Error "Haskell.destApp: fun"
      else if isList ty then raise Error "Haskell.destApp: list"
      else if Type.isPair ty then raise Error "Haskell.destApp: pair"
      else
        case Type.dest ty of
          TypeTerm.VarTy' _ => raise Error "Haskell.destApp: variable"
        | TypeTerm.OpTy' (ot,tys) =>
          if List.null tys then raise Error "Haskell.destApp: nullary"
          else (ot,tys);

  fun ppBasic ns ty =
      case Type.dest ty of
        TypeTerm.VarTy' n => ppTypeVar n
      | TypeTerm.OpTy' (ot,tys) =>
        if List.null tys then ppTypeOp ns ot
        else Print.ppBracket "(" ")" (ppGen ns) ty

  and ppList ns ty =
      case total destList ty of
        NONE => ppBasic ns ty
      | SOME a =>
        Print.inconsistentBlock 1
          [Print.ppString "[",
           ppGen ns a,
           Print.ppString "]"]

  and ppPair ns ty =
      if not (Type.isPair ty) then ppList ns ty
      else
        let
          val (a,b) = Type.destPair ty
        in
          Print.inconsistentBlock 1
            [Print.ppString "(",
             ppApp ns a,
             Print.ppString ",",
             Print.break,
             ppFun ns b,
             Print.ppString ")"]
        end

  and ppArguments ns tys =
      let
        val brk = Print.ppBreak (Print.Break {size = 1, extraIndent = 2})

        fun ppArg ty = Print.sequence brk (ppPair ns ty)
      in
        Print.program (List.map ppArg tys)
      end

  and ppApp ns ty =
      case total destApp ty of
        NONE => ppPair ns ty
      | SOME (ot,tys) =>
        Print.inconsistentBlock 0
          [ppTypeOp ns ot,
           ppArguments ns tys]

  and ppFun ns ty =
      if not (Type.isFun ty) then ppApp ns ty
      else
        let
          fun ppDom d =
              Print.program
                [ppApp ns d,
                 Print.space,
                 Print.ppString "->",
                 Print.ppBreak (Print.Break {size = 1, extraIndent = 2})]

          val (ds,r) = Type.stripFun ty
        in
          Print.inconsistentBlock 0
            [Print.program (List.map ppDom ds),
             ppApp ns r]
        end

  and ppGen ns ty = ppFun ns ty;
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
  val infixes =
      Print.Infixes
        [(* http://zvon.org/other/haskell/Outputprelude/index.html *)
         {token = ".", precedence = 9, assoc = Print.RightAssoc},
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

  fun ppInfixToken (_,s) =
      let
        val ps = [Print.ppString s]

        val ps =
            if isSymbolString s then ps
            else let val p = Print.ppChar #"`" in p :: ps @ [p] end
      in
        Print.program (Print.space :: ps @ [Print.break])
      end

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

  fun isLetCondCase tm =
      Term.isLet tm orelse Term.isCond tm orelse Term.isCase tm;
in
  fun ppTerm exp =
      let
        fun destInfix tm =
            let
              val (t,b) = Term.destApp tm

              val (t,a) = Term.destApp t

              val (c,_) = Term.destConst t

              val n = constNameSymbolExport exp (Const.name c)

              val (ns,s) = Name.dest n

              val () =
                  if Namespace.isGlobal ns then ()
                  else raise Error "Haskell.ppTerm.destInfix: not global"

              val () =
                  if StringSet.member s infixTokens then ()
                  else raise Error "Haskell.ppTerm.destInfix: not infix"
            in
              (s,a,b)
            end

        val isInfix = can destInfix

        val ppInfix = Print.ppInfixes infixes (total destInfix) ppInfixToken

        fun destGenApp tm =
            if Term.isNumeral tm then
              raise Error "Haskell.ppTerm.destGenApp: numeral"
            else if Term.isCond tm then
              raise Error "Haskell.ppTerm.destGenApp: cond"
            else if Term.isPair tm then
              raise Error "Haskell.ppTerm.destGenApp: pair"
            else if Term.isLet tm then
              raise Error "Haskell.ppTerm.destGenApp: let"
            else if isInfix tm then
              raise Error "Haskell.ppTerm.destGenApp: infix"
            else if Term.isGenAbs tm then
              raise Error "Haskell.ppTerm.destGenApp: abstraction"
            else if Term.isCase tm then
              raise Error "Haskell.ppTerm.destGenApp: case"
            else
              Term.destApp tm

        val stripGenApp =
            let
              fun strip acc tm =
                  case total destGenApp tm of
                    NONE => (tm,acc)
                  | SOME (f,x) => strip (x :: acc) f
            in
              strip []
            end

        fun ppBasicTerm tm =
            case total Term.destNumeral tm of
              SOME i => Print.ppInt i
            | NONE =>
              case total Term.destPair tm of
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

              val (tm,xs) = stripGenApp tm
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
              val (vs,body) = Term.stripGenAbs tm
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
                  case total Term.destLet b of
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
                  case total Term.destCond b of
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
            case total Term.destLet tm of
              SOME (v,t,b) =>
                Print.consistentBlock 0 (ppLetTerm (v,t,b,r))
            | NONE =>
              case total Term.destCond tm of
                SOME (c,a,b) =>
                Print.consistentBlock 0 (ppCondTerm (true,c,a,b,r))
              | NONE =>
                case total Term.destCase tm of
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
            [Print.ppString " == ",
             PackageVersion.pp newest]
          else
            [Print.ppString " >= ",
             PackageVersion.pp oldest,
             Print.ppString " && <= ",
             PackageVersion.pp newest]))
    end;

local
  fun ppDecl exp (name,parms) =
      Print.inconsistentBlock 2
        [Print.ppString "data ",
         ppTypeOp exp name,
         ppTypeVarList parms,
         Print.ppString " ="];

  fun ppCon exp prefix (c,tys) =
      Print.program
        [Print.newline,
         Print.ppString prefix,
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
        [Print.ppString "newtype ",
         ppTypeOp exp name,
         ppTypeVarList parms,
         Print.ppString " ="];

  fun ppRep exp (rep,repType) =
      Print.inconsistentBlock 2
        [ppConst exp rep,
         Print.space,
         Print.ppString "::",
         Print.break,
         ppType exp repType];

  fun ppIso exp (abs,rep) =
      Print.consistentBlock 0
        [ppConst exp abs,
         Print.space,
         Print.ppString "{",
         Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
         ppRep exp rep,
         Print.break,
         Print.ppString "}"];
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
         Print.ppString " ::",
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
           Print.ppString " =" ::
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
             (Print.ppString "where" ::
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

fun ppModule int (tags,namespace,source) =
    let
      val exp = mkSymbolExport int namespace source
    in
      Print.inconsistentBlock 0
        [Print.ppString "{- |",
         Print.newline,
         ppTag (moduleTag,"$Header$"),
         Print.newline,
         ppTags tags
           [descriptionTag,
            licenseTag],
         Print.newlines 2,
         ppTags tags
           [maintainerTag,
            stabilityTag,
            portabilityTag],
         Print.newline,
         Print.ppString "-}",
         Print.newline,
         ppModuleDeclaration exp,
         Print.newlines 2,
         ppModuleImport exp,
         Print.newline,
         ppSourceList exp source]
    end;

(***
local
  fun mkTestsSymbolExport tests =
      let
        val namespace = haskellTestNamespace
        and source = List.map (fn Test {value,...} => ValueSource value) tests
      in
        mkSymbolExport namespace source
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
          [Print.ppString "Primitive.Test.",
           Print.ppString invoke,
           Print.ppString " \"",
           Print.ppString (escapeString desc),
           Print.ppString "\\n  \" ",
           Print.ppString name,
           Print.newline]
      end;

  fun ppMain tests =
      Print.inconsistentBlock 0
        [Print.ppString "main :: IO ()",
         Print.newline,
         Print.inconsistentBlock 4
           [Print.ppString "main =",
            Print.newline,
            Print.consistentBlock 3
              [Print.ppString "do ",
               Print.program (List.map ppInvokeTest tests),
               Print.ppString "return ()"]]];
in
  fun ppTests (tags,tests) =
      let
        val desc = getTag tags descriptionTag
        and exp = mkTestsSymbolExport tests
      in
        Print.inconsistentBlock 0
          [Print.ppString "{- |",
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
           Print.ppString "-}",
           Print.newline,
           Print.ppString "module Main",
           Print.newline,
           Print.ppString "  ( main )",
           Print.newline,
           Print.ppString "where",
           Print.newlines 2,
           ppModuleImport exp,
           Print.ppString "import qualified OpenTheory.Primitive.Test as Primitive.Test",
           Print.newline,
           Print.newline,
           Print.program (List.map (ppTestSpace exp) tests),
           ppMain tests]
      end;
end;
***)

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
        [Print.ppString s,
         Print.newline,
         Print.program pps];

  local
    fun ppExtraDepend dep =
        Print.program
          [Print.ppString ",",
           Print.newline,
           ppDepend dep];
  in
    fun ppBuildDepends deps =
        Print.ppString "base >= 4.0 && < 5.0," ::
        Print.newline ::
        Print.ppString "random >= 1.0.1.1 && < 2.0," ::
        Print.newline ::
        Print.ppString "QuickCheck >= 2.4.0.1 && < 3.0," ::
        Print.newline ::
        Print.ppString "opentheory-primitive >= 1.0 && < 2.0" ::
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
          (Print.ppString x ::
           List.map (Print.sequence Print.break o Print.ppString) xs);
in
  fun ppCabal (tags,deps,source) =
      let
        val name = nameTags tags
        and nameVersion = nameVersionTags tags
        and desc = descriptionTags tags
        and mods = exposedModule source

        val extraTags = StringSet.difference (allTags tags) nonExtraTags
      in
        Print.inconsistentBlock 0
          [ppTags tags (initialTags @ StringSet.toList extraTags),
           Print.newline,
           Print.inconsistentBlock 2
             [Print.ppString descriptionTag,
              Print.ppString ":",
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
              ppSection "exposed-modules:" (ppExposedModules mods)] (***,
           Print.newline,
           Print.newline,
           ppSection ("executable " ^ Print.toString ppPackageTestName name)
             [ppSection "build-depends:" (ppBuildDepends deps),
              Print.newline,
              Print.newline,
              ppTag ("hs-source-dirs","src, testsrc"),
              Print.newline,
              Print.newline,
              ppTags tags [ghcOptionsTag],
              Print.newline,
              Print.newline,
              ppTag ("main-is","Test.hs")] ***) ]
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

fun outputCabal {directory = dir} tags deps source =
    let
      val ss = Print.toStream ppCabal (tags,deps,source)

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

  fun outputSrc int tags {directory = dir} sub namespace source =
      let
        val ss = Print.toStream (ppModule int) (tags,namespace,source)

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

(***
local
  fun outputMain tags {directory = dir} tests =
      let
        val ss = Print.toStream ppTests (tags,tests)

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
  fun outputTests dir tags tests =
      let
        val dir = mkSubDirectory dir "testsrc"
      in
        outputMain tags dir tests
      end;
end;
***)

fun writePackage haskell =
    let
      val Haskell
            {system = sys,
             information = info,
             depends = deps,
             interpretation = int,
             source = src,
             tests = _} = haskell

      val tags = mkTags info

      val dir =
          let
            val name = Print.toLine ppPackageName (nameInformation info)

            val dir = {directory = OS.FileSys.getDir ()}
          in
            mkSubDirectory dir name
          end

      val () = outputCabal dir tags deps src

      val () =
          let
            val url = licenseUrlInformation info
          in
            outputLicense sys url dir tags
          end

      val () = outputSetup dir

      val () = outputSource int dir tags src
(***
      val () = outputTests dir tags tests
***)
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
      ()
    end;

end
