(* ========================================================================= *)
(* GENERATING HASKELL PROJECTS FROM THEORIES                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure Haskell :> Haskell =
struct

open Useful;

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

             val tySub = TypeSubst.emptyMap
             and tmSub = TermSubst.fromListTermMap (List.mapPartial anonVar vl)

             val sub = TermSubst.mk (tySub,tmSub)
           in
             Option.getOpt (TermSubst.subst sub pat, pat)
           end
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Exporting various OpenTheory names to Haskell.                            *)
(* ------------------------------------------------------------------------- *)

fun exportPackageName name =
    case PackageName.exportHaskell name of
      SOME n => n
    | NONE =>
      let
        val err = "non-Haskell package name: " ^ PackageName.toString name
      in
        raise Error err
      end;

val opentheoryNamespace = Namespace.fromList ["OpenTheory"];

local
  val haskellNamespace = Namespace.fromList ["Haskell"];
in
  val haskellTestNamespace = Namespace.mkNested (haskellNamespace,"Test");

  fun exportNamespace ns =
      case Namespace.rewrite (haskellNamespace,opentheoryNamespace) ns of
        SOME ns => ns
      | NONE => raise Error ("non-Haskell namespace: " ^ Namespace.toString ns);
end;

local
  local
    fun mkName root ns =
        curry Name.mk (Namespace.append root (Namespace.fromList ns));

    val primitiveRoot = Namespace.mkNested (opentheoryNamespace,"Primitive");

    val mkPrimitive = mkName primitiveRoot;
  in
    val mkNative = mkName Namespace.global [];

    val mkPrimitiveByte = mkPrimitive ["Byte"]
    and mkPrimitiveNatural = mkPrimitive ["Natural"]
    and mkPrimitiveRandom = mkPrimitive ["Random"]
    and mkPrimitiveWord16 = mkPrimitive ["Word16"];
  end;

  val typeOpMapping =
      NameMap.fromList
        [(* Native types *)
         (Name.boolTypeOp, mkNative "Bool"),
         (Name.funTypeOp, mkNative "->"),
         (Name.listTypeOp, mkNative "List"),
         (Name.optionTypeOp, mkNative "Maybe"),
         (Name.pairTypeOp, mkNative "Pair"),
         (* Primitive types *)
         (Name.byteTypeOp, mkPrimitiveByte "Byte"),
         (Name.naturalTypeOp, mkPrimitiveNatural "Natural"),
         (Name.randomTypeOp, mkPrimitiveRandom "Random"),
         (Name.word16TypeOp, mkPrimitiveWord16 "Word16")];

  val constMapping =
      NameMap.fromList
        [(* Native constants *)
         (Name.addConst, mkNative "+"),
         (Name.addByteConst, mkNative "+"),
         (Name.addWord16Const, mkNative "+"),
         (Name.appendConst, mkNative "++"),
         (Name.conjConst, mkNative "&&"),
         (Name.consConst, mkNative ":"),
         (Name.disjConst, mkNative "||"),
         (Name.divConst, mkNative "div"),
         (Name.eqConst, mkNative "=="),
         (Name.falseConst, mkNative "False"),
         (Name.leConst, mkNative "<="),
         (Name.leByteConst, mkNative "<="),
         (Name.leWord16Const, mkNative "<="),
         (Name.ltConst, mkNative "<"),
         (Name.ltByteConst, mkNative "<"),
         (Name.ltWord16Const, mkNative "<"),
         (Name.modConst, mkNative "mod"),
         (Name.multiplyConst, mkNative "*"),
         (Name.multiplyByteConst, mkNative "*"),
         (Name.multiplyWord16Const, mkNative "*"),
         (Name.negConst, mkNative "not"),
         (Name.nilConst, mkNative "[]"),
         (Name.noneConst, mkNative "Nothing"),
         (Name.pairConst, mkNative ","),
         (Name.someConst, mkNative "Just"),
         (Name.subtractConst, mkNative "-"),
         (Name.subtractByteConst, mkNative "-"),
         (Name.subtractWord16Const, mkNative "-"),
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

  fun exportName n =
      let
        val (ns,n) = Name.dest n

        val ns = exportNamespace ns
      in
        Name.mk (ns,n)
      end;
in
  fun exportTypeOpName n =
      case NameMap.peek typeOpMapping n of
        SOME n => n
      | NONE =>
        exportName n
        handle Error err =>
          let
            val err = "bad type operator name: " ^ Name.toString n ^ "\n" ^ err
          in
            raise Error err
          end;

  fun exportConstName n =
      let
        val () =
            if not (Name.isCase n) then ()
            else
              let
                val err = "case constant name: " ^ Name.toString n
              in
                raise Error err
              end
      in
        case NameMap.peek constMapping n of
          SOME n => n
        | NONE =>
          exportName n
          handle Error err =>
            let
              val err = "bad constant name: " ^ Name.toString n ^ "\n" ^ err
            in
              raise Error err
            end
      end;
end;

(* ------------------------------------------------------------------------- *)
(* A type of Haskell packages.                                               *)
(* ------------------------------------------------------------------------- *)

datatype depend =
    Depend of
      {name : PackageName.name,
       oldest : PackageVersion.version,
       newest : PackageVersion.version};

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

datatype module =
    Module of
      {namespace : Namespace.namespace,
       source : source list,
       submodules : module list};

datatype test =
    Test of
      {name : string,
       description : string,
       value : value};

datatype haskell =
     Haskell of
       {package : Package.package,
        depends : depend list,
        source : module,
        tests : test list};

(* ------------------------------------------------------------------------- *)
(* Haskell package dependencies.                                             *)
(* ------------------------------------------------------------------------- *)

fun mkDepends dir pkg thy =
    let
      fun checkPrevious oldest ths vs =
          if Queue.null ths then oldest
          else
            let
              val (th,ths) = Queue.hdTl ths

              val nv = PackageTheorems.package th
            in
              case Directory.previousNameVersion dir nv of
                NONE =>
                let
                  val n = PackageNameVersion.name nv
                  and v = PackageNameVersion.version nv

                  val oldest = PackageNameMap.insert oldest (n,v)
                in
                  checkPrevious oldest ths vs
                end
              | SOME nv' =>
                let
                  val info = Directory.get dir nv'

                  val th = PackageInfo.theorems info
                in
                  case total (PackageTheorems.addVersion vs) th of
                    NONE =>
                    let
                      val n = PackageNameVersion.name nv
                      and v = PackageNameVersion.version nv

                      val oldest = PackageNameMap.insert oldest (n,v)
                    in
                      checkPrevious oldest ths vs
                    end
                  | SOME vs =>
                    let
                      val ths = Queue.add th ths
                    in
                      checkPrevious oldest ths vs
                    end
                end
            end

      val ths =
          case Directory.requiresTheorems dir (Package.requires pkg) of
            SOME ths => ths
          | NONE => raise Error "required theories not installed"

      val asms =
          Sequents.sequents (Summary.requires (Theory.summary thy))

      val vs =
          PackageTheorems.mkVersions asms ths
          handle Error err =>
            raise Error ("required theories not up to date:\n" ^ err)

      val ths =
          let
            fun isHaskell th =
                let
                  val nv = PackageTheorems.package th

                  val n = PackageNameVersion.name nv
                in
                  PackageName.isHaskell n
                end
          in
            List.filter isHaskell ths
          end

      val oldest =
          checkPrevious (PackageNameMap.new ()) (Queue.fromList ths) vs

      fun mk th =
          let
            val nv = PackageTheorems.package th

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
      List.map mk ths
    end;

(* ------------------------------------------------------------------------- *)
(* Symbols in Haskell declarations.                                          *)
(* ------------------------------------------------------------------------- *)

fun symbolData (Data {name,...}) = Symbol.TypeOp name;

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

fun symbolNewtype (Newtype {name,...}) = Symbol.TypeOp name;

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

fun symbolValue (Value {name,...}) = Symbol.Const name;

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

fun definedSymbolTableValue value =
    let
      val Value {name,...} = value

      val sym = SymbolTable.empty

      val sym = SymbolTable.addConst sym name
    in
      sym
    end;

fun symbolSource s =
    case s of
      DataSource x => symbolData x
    | NewtypeSource x => symbolNewtype x
    | ValueSource x => symbolValue x;

fun nameSource s = Symbol.name (symbolSource s);

fun namespaceSource s = Name.namespace (nameSource s);

fun namespaceSourceList sl =
    NamespaceSet.fromList (List.map namespaceSource sl);

fun symbolTableSource s =
    case s of
      DataSource x => symbolTableData x
    | NewtypeSource x => symbolTableNewtype x
    | ValueSource x => symbolTableValue x;

fun definedSymbolTableSource s =
    case s of
      DataSource x => definedSymbolTableData x
    | NewtypeSource x => definedSymbolTableNewtype x
    | ValueSource x => definedSymbolTableValue x;

fun symbolTableSourceList sl =
    SymbolTable.unionList (List.map symbolTableSource sl);

fun definedSymbolTableSourceList sl =
    SymbolTable.unionList (List.map definedSymbolTableSource sl);

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
(* Converting theorems into Haskell declarations.                            *)
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
              if not (Name.equal (Const.name name) (TypeOp.name ot)) then ()
              else raise Error "newtype constructor constant"
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

local
  fun ppProposition show (n,prop) =
      Print.inconsistentBlock 2
        [Print.ppString "Proposition ",
         Print.ppInt n,
         Print.ppString ":",
         Print.newline,
         Term.ppWithShow show prop]
in
  fun destTest show th n =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val () =
            if VarSet.null (Term.freeVars concl) then ()
            else raise Error "free variables"

        val (v,body) = Term.destForall concl

        val () =
            if Type.isRandom (Var.typeOf v) then ()
            else raise Error "bad quantified variable type"

        val arg = Term.mkVar v

        val eqn =
            Equation
              {arguments = [arg],
               body = body,
               whereValues = []}

        val name = "proposition" ^ Int.toString n

        val const = Const.mkUndef (Name.mk (haskellTestNamespace,name))

        val ty = Type.mkFun (Term.typeOf arg, Term.typeOf body)

        val value =
            Value
              {name = const,
               ty = ty,
               equations = [eqn]}

        val desc = Print.toString (ppProposition show) (n,concl)

        val test = Test {name = name, description = desc, value = value}

        val n = n + 1
      in
        (test,n)
      end
      handle Error err =>
        raise Error ("bad test theorem: " ^ err);
end;

(* ------------------------------------------------------------------------- *)
(* Sorting Haskell declarations into a module hierarchy.                     *)
(* ------------------------------------------------------------------------- *)

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
(* Converting a theory to a Haskell package.                                 *)
(* ------------------------------------------------------------------------- *)

local
  fun getTheory name thys =
      case Theory.peekTheory name thys of
        SOME thy => thy
      | NONE =>
        let
          val err = "missing " ^ PackageName.toString name ^ " block in theory"
        in
          raise Error err
        end;
in
  fun splitTheories thy =
      let
        val thys =
            case Theory.node thy of
              Theory.Package {theories,...} => theories
            | _ => raise Bug "Haskell.theories: not a package theory"

        val src = getTheory PackageName.srcHaskellTheory thys
        and test = getTheory PackageName.testHaskellTheory thys
      in
        {src = src,
         test = test}
      end;
end;

fun destSourceTheory src =
    let
      val art = Theory.article src

      val ths = ThmSet.toList (Thms.thms (Article.thms art))

      val src = List.map destSource ths

      val src = groupSource src

      val src = sortSource src
    in
      src
    end;

fun destTestTheory show test =
    let
      val art = Theory.article test

      val ths = ThmSet.toList (Thms.thms (Article.thms art))

      val (tests,n) = maps (destTest show) ths 0

      val () =
          if n > 0 then ()
          else raise Error "no tests defined"
    in
      tests
    end;

fun convert dir pkg thy =
    let
      val {src,test} = splitTheories thy

      val deps = mkDepends dir pkg thy
      and source = mkModule (destSourceTheory src)
      and tests = destTestTheory (Package.show pkg) test
    in
      Haskell
        {package = pkg,
         depends = deps,
         source = source,
         tests = tests}
    end;

(* ------------------------------------------------------------------------- *)
(* Creating the symbol name mapping in preparation for printing.             *)
(* ------------------------------------------------------------------------- *)

datatype symbolExport =
    SymbolExport of
      {namespace : Namespace.namespace,
       importNamespaces : NamespaceSet.set,
       exportTypeOps : Name.name NameMap.map,
       exportConsts : Name.name NameMap.map};

val importSymbolTableData = symbolTableData;

val importSymbolTableNewtype = symbolTableNewtype;

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
  fun importSymbolTableValue value =
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

fun importSymbolTableSource s =
    case s of
      DataSource x => importSymbolTableData x
    | NewtypeSource x => importSymbolTableNewtype x
    | ValueSource x => importSymbolTableValue x;

fun importSymbolTableSourceList sl =
    SymbolTable.unionList (List.map importSymbolTableSource sl);

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

  fun mkTypeOpMap table =
      let
        val ts = SymbolTable.typeOps table

        val ns = TypeOpSet.foldl addTypeOp NameSet.empty ts
      in
        NameSet.map exportTypeOpName ns
      end;

  fun addConst (c,ns) =
      let
        val n = Const.name c
      in
        case total Name.destCase n of
          SOME (_,nl) => NameSet.addList ns nl
        | NONE => NameSet.add ns n
      end;

  fun mkConstMap table =
      let
        val cs = SymbolTable.consts table

        val ns = ConstSet.foldl addConst NameSet.empty cs
      in
        NameSet.map exportConstName ns
      end;
in
  fun mkSymbolExport namespace source =
      let
        val namespace = exportNamespace namespace

        val impTable = importSymbolTableSourceList source
        and defTable = definedSymbolTableSourceList source

        val ts = mkTypeOpMap impTable
        and cs = mkConstMap impTable

        val white =
            NamespaceSet.union
              (targetNamespaces ts)
              (targetNamespaces cs)

        val black =
            NamespaceSet.add
              (targetNamespaces (mkConstMap defTable))
              Namespace.global

        val ns = NamespaceSet.difference white black
      in
        SymbolExport
          {namespace = namespace,
           importNamespaces = ns,
           exportTypeOps = ts,
           exportConsts = cs}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Printing Haskell source code.                                             *)
(* ------------------------------------------------------------------------- *)

(* Names *)

fun ppPackageName name = PackageName.pp (exportPackageName name);

fun ppPackageTestName name =
    Print.sequence
      (PackageName.pp (exportPackageName name))
      (Print.ppString "-test");

fun ppNamespace ns = Namespace.pp (exportNamespace ns);

local
  fun shortenNamespace namespace ns =
      case Namespace.rewrite (namespace,Namespace.global) ns of
        SOME ns => ns
      | NONE =>
        case Namespace.rewrite (opentheoryNamespace,Namespace.global) ns of
          SOME ns => ns
        | NONE =>
          let
            val bug =
                "Haskell.shortenNamespace: " ^
                Print.toString Namespace.pp ns
          in
            raise Bug bug
          end;

  fun shortenName namespace n =
      let
        val (ns,c) = Name.dest n
      in
        if Namespace.isGlobal ns then n
        else Name.mk (shortenNamespace namespace ns, c)
      end;

  fun ppImportNamespace namespace ns =
      let
        val ns' = shortenNamespace namespace ns

        val ppImportAs =
            Print.inconsistentBlock 2
              [Print.ppString "import",
               Print.space,
               Print.ppString "qualified",
               Print.space,
               Namespace.pp ns,
               Print.break,
               Print.ppString "as",
               Print.space,
               Namespace.pp ns']
      in
        Print.sequence ppImportAs Print.newline
      end;
in
  fun ppModuleDeclaration exp =
      let
        val SymbolExport {namespace,exportTypeOps,...} = exp
      in
        Print.inconsistentBlock 0
          [Print.ppString "module ",
           Namespace.pp namespace,
           Print.newline,
           Print.ppString "where"]
      end;

  fun ppModuleImport exp =
      let
        val SymbolExport {namespace,importNamespaces,...} = exp

        val import = NamespaceSet.toList importNamespaces
      in
        Print.inconsistentBlock 0
          (List.map (ppImportNamespace namespace) import)
      end;

  fun ppTypeOpName exp n =
      let
        val SymbolExport {namespace,exportTypeOps,...} = exp

        val n =
            case NameMap.peek exportTypeOps n of
              SOME n => n
            | NONE =>
              (* This "cache-miss" should only happen for commented-out *)
              (* types annotating nested declarations *)
              exportTypeOpName n
      in
        Name.pp (shortenName namespace n)
      end;

  fun ppConstName exp n =
      let
        val SymbolExport {namespace,exportConsts,...} = exp

        val n =
            case NameMap.peek exportConsts n of
              SOME n => n
            | NONE =>
              let
                val bug = "Haskell.ppConstName: " ^ Name.toString n
              in
                raise Bug bug
              end
      in
        Name.pp (shortenName namespace n)
      end;
end;

fun ppVarName n =
    if Name.isGlobal n then Name.pp n
    else raise Error "non-global variable name";

(* Types *)

fun ppTypeOp ns ot = ppTypeOpName ns (TypeOp.name ot);

val ppTypeVar = Name.pp;

val ppTypeVarList =
    let
      val ppSpace = Print.ppString " "

      fun ppSpaceTypeVar v = Print.sequence ppSpace (ppTypeVar v)
    in
      fn vl => Print.program (List.map ppSpaceTypeVar vl)
    end;

local
  fun destApp ty =
      if Type.isFun ty then raise Error "Haskell.destApp: fun"
      else if Type.isList ty then raise Error "Haskell.destApp: list"
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
      if not (Type.isList ty) then ppBasic ns ty
      else
        let
          val a = Type.destList ty
        in
          Print.inconsistentBlock 1
            [Print.ppString "[",
             ppGen ns a,
             Print.ppString "]"]
        end

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

fun ppConst ns c =
    let
      val n = Const.name c

      val p = ppConstName ns

      val p =
          if not (isSymbolName (exportConstName n)) then p
          else Print.ppBracket "(" ")" p
    in
      p n
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

  fun destInfixTerm tm =
      let
        val (t,b) = Term.destApp tm

        val (t,a) = Term.destApp t

        val (c,_) = Term.destConst t

        val n = exportConstName (Const.name c)
      in
        (n,a,b)
      end;

  fun destInfix tm =
      let
        val (n,a,b) = destInfixTerm tm

        val (_,s) = Name.dest n
      in
        if StringSet.member s infixTokens then (s,a,b)
        else raise Error "Haskell.ppTerm.destInfix"
      end;

  val isInfix = can destInfix;

  fun ppInfixToken (_,s) =
      let
        val l = [Print.ppString s]

        val l =
            if isSymbolString s then l
            else [Print.ppString "`"] @ l @ [Print.ppString "`"]

        val l = [Print.space] @ l @ [Print.break]
      in
        Print.program l
      end;

  val ppInfix = Print.ppInfixes infixes (total destInfix) ppInfixToken;

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
        Term.destApp tm;

  val stripGenApp =
      let
        fun strip acc tm =
            case total destGenApp tm of
              NONE => (tm,acc)
            | SOME (f,x) => strip (x :: acc) f
      in
        strip []
      end;

  fun isLetCondCase tm =
      Term.isLet tm orelse Term.isCond tm orelse Term.isCase tm;

  val ppSyntax = Print.ppString;
in
  fun ppTerm namespace =
      let
        fun ppBasicTerm tm =
            case total Term.destNumeral tm of
              SOME i => Print.ppInt i
            | NONE =>
              case total Term.destPair tm of
                SOME x_y => ppPair x_y
              | NONE =>
                case Term.dest tm of
                  TypeTerm.Var' v => ppVar v
                | TypeTerm.Const' (c,_) => ppConst namespace c
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

        and ppNormalTerm tm = ppInfixTerm (tm,false)

        and ppBracketTerm tm = Print.ppBracket "(" ")" ppNormalTerm tm
      in
        ppNormalTerm
      end;
end;

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
  fun ppDecl ns (name,parms) =
      Print.inconsistentBlock 2
        [Print.ppString "data ",
         ppTypeOp ns name,
         ppTypeVarList parms,
         Print.ppString " ="];

  fun ppCon ns prefix (c,tys) =
      Print.program
        [Print.newline,
         Print.ppString prefix,
         Print.inconsistentBlock 4
           [ppConst ns c,
            ppTypeList ns tys]];

  fun ppCons ns cs =
      case cs of
        [] => raise Error "datatype has no constructors"
      | c :: cs =>
        Print.program (ppCon ns "  " c :: List.map (ppCon ns "| ") cs);
in
  fun ppData ns data =
      let
        val Data
              {name,
               parameters = parms,
               constructors = cons,
               caseConst = _} = data
      in
        Print.inconsistentBlock 2
          [ppDecl ns (name,parms),
           ppCons ns cons]
    end;
end;

local
  fun ppDecl ns (name,parms) =
      Print.inconsistentBlock 2
        [Print.ppString "newtype ",
         ppTypeOp ns name,
         ppTypeVarList parms,
         Print.ppString " ="];

  fun ppRep ns (rep,repType) =
      Print.inconsistentBlock 2
        [ppConst ns rep,
         Print.space,
         Print.ppString "::",
         Print.break,
         ppType ns repType];

  fun ppIso ns (abs,rep) =
      Print.consistentBlock 0
        [ppConst ns abs,
         Print.space,
         Print.ppString "{",
         Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
         ppRep ns rep,
         Print.break,
         Print.ppString "}"];
in
  fun ppNewtype ns newtype =
      let
        val Newtype
              {name,
               parameters = parms,
               repType,
               abs,
               rep} = newtype
      in
        Print.inconsistentBlock 2
          [ppDecl ns (name,parms),
           Print.break,
           ppIso ns (abs,(rep,repType))]
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

fun ppTag (s,pp) =
    Print.inconsistentBlock 2
      [Print.ppString s,
       Print.ppString ": ",
       pp];

fun ppTags spps =
    case spps of
      [] => Print.skip
    | spp :: spps =>
      Print.inconsistentBlock 0
        (ppTag spp ::
         List.map (Print.sequence Print.newline o ppTag) spps);

fun ppModule (pkg,namespace,source) =
    let
      val {description} = Package.description pkg
      and {license} = Package.license pkg
      and auth = Package.author pkg
      and exp = mkSymbolExport namespace source
    in
      Print.inconsistentBlock 0
        [Print.ppString "{- |",
         Print.newline,
         ppTags
           [("Module", Print.ppString "$Header$"),
            ("Description", Print.ppString description),
            ("License", Print.ppString license)],
         Print.newlines 2,
         ppTags
           [("Maintainer", PackageAuthor.pp auth),
            ("Stability", Print.ppString "provisional"),
            ("Portability", Print.ppString "portable")],
         Print.newline,
         Print.ppString "-}",
         Print.newline,
         ppModuleDeclaration exp,
         Print.newlines 2,
         ppModuleImport exp,
         Print.newline,
         ppSourceList exp source]
    end;

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
        val Test {name, description = desc, ...} = test
      in
        Print.program
          [Print.ppString "Primitive.Test.check \"",
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
  fun ppTests (pkg,tests) =
      let
        val {description} = Package.description pkg
        and {license} = Package.license pkg
        and auth = Package.author pkg
        and exp = mkTestsSymbolExport tests
      in
        Print.inconsistentBlock 0
          [Print.ppString "{- |",
           Print.newline,
           ppTags
             [("Module", Print.ppString "Main"),
              ("Description", Print.ppString (description ^ " - testing")),
              ("License", Print.ppString license)],
           Print.newlines 2,
           ppTags
             [("Maintainer", PackageAuthor.pp auth),
              ("Stability", Print.ppString "provisional"),
              ("Portability", Print.ppString "portable")],
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

(* Cabal *)

local
  val ppVersion = PackageVersion.pp;

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
in
  fun ppCabal (pkg,deps,source) =
      let
        val name = Package.name pkg
        and version = Package.version pkg
        and {description} = Package.description pkg
        and {license} = Package.license pkg
        and auth = Package.author pkg
        and mods = exposedModule source
      in
        Print.inconsistentBlock 0
          [ppTags
             [("Name", ppPackageName name),
              ("Version", ppVersion version),
              ("Description", Print.ppString description),
              ("License", Print.ppString license),
              ("License-file", Print.ppString "LICENSE"),
              ("Cabal-version", Print.ppString ">= 1.8.0.6"),
              ("Build-type", Print.ppString "Simple"),
              ("Author", PackageAuthor.pp auth),
              ("Maintainer", PackageAuthor.pp auth)],
           Print.newline,
           Print.newline,
           ppSection "Library"
             [ppSection "Build-depends:" (ppBuildDepends deps),
              Print.newline,
              Print.newline,
              ppTag ("hs-source-dirs", Print.ppString "src"),
              Print.newline,
              Print.newline,
              ppTag ("ghc-options", Print.ppString "-Wall -Werror"),
              Print.newline,
              Print.newline,
              ppSection "Exposed-modules:" (ppExposedModules mods)],
           Print.newline,
           Print.newline,
           ppSection ("Executable " ^ Print.toString ppPackageTestName name)
             [ppSection "Build-depends:" (ppBuildDepends deps),
              Print.newline,
              Print.newline,
              ppTag ("hs-source-dirs", Print.ppString "src, testsrc"),
              Print.newline,
              Print.newline,
              ppTag ("ghc-options", Print.ppString "-Wall -Werror"),
              Print.newline,
              Print.newline,
              ppTag ("Main-is", Print.ppString "Test.hs")]]
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

fun outputCabal {directory = dir} pkg deps source =
    let
      val ss = Print.toStream ppCabal (pkg,deps,source)

      val file =
          let
            val base = Print.toLine ppPackageName (Package.name pkg)

            val f = OS.Path.joinBaseExt {base = base, ext = SOME "cabal"}
          in
            OS.Path.joinDirFile {dir = dir, file = f}
          end

      val () = Stream.toTextFile {filename = file} ss
    in
      ()
    end;

fun outputLicense dir {directory} pkg =
    let
      val {license} = Package.license pkg

      val license = Directory.getLicense dir {name = license}

      val {url} = DirectoryConfig.urlLicense license

      val file = OS.Path.joinDirFile {dir = directory, file = "LICENSE"}

      val {curl = cmd} = DirectorySystem.curl (Directory.system dir)

      val cmd = cmd ^ " " ^ url ^ " --output " ^ file

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)
    in
      if OS.Process.isSuccess (OS.Process.system cmd) then ()
      else raise Error "downloading the license file failed"
    end;

local
  fun exportSubNamespace parent ns =
      let
        val xparent =
            if Namespace.isGlobal parent then parent
            else exportNamespace parent

        val xns = exportNamespace ns

        val (xns_nested,xns_sub) = Namespace.destNested xns

        val () =
            if Namespace.equal xparent xns_nested then ()
            else raise Bug "Haskell.exportSubNamespace"
      in
        xns_sub
      end;

  fun outputSrc pkg {directory = dir} sub namespace source =
      let
        val ss = Print.toStream ppModule (pkg,namespace,source)

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

  fun outputMod pkg dir parent module =
      let
        val Module {namespace,source,submodules} = module

        val sub = exportSubNamespace parent namespace

        val () =
            if List.null source then ()
            else outputSrc pkg dir sub namespace source

        val () =
            if List.null submodules then ()
            else
              let
                val dir = mkSubDirectory dir sub
              in
                List.app (outputMod pkg dir namespace) submodules
              end
      in
        ()
      end;
in
  fun outputSource dir pkg module =
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
        List.app (outputMod pkg dir namespace) submodules
      end;
end;

local
  fun outputMain pkg {directory = dir} tests =
      let
        val ss = Print.toStream ppTests (pkg,tests)

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
  fun outputTests dir pkg tests =
      let
        val dir = mkSubDirectory dir "testsrc"
      in
        outputMain pkg dir tests
      end;
end;

fun toPackage dir haskell =
    let
      val Haskell {package,depends,source,tests} = haskell

      val directory =
          let
            val name = Print.toLine ppPackageName (Package.name package)

            val directory = {directory = OS.FileSys.getDir ()}
          in
            mkSubDirectory directory name
          end

      val () = outputCabal directory package depends source

      val () = outputLicense dir directory package

      val () = outputSource directory package source

      val () = outputTests directory package tests
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Export a theory to a Haskell package.                                     *)
(* ------------------------------------------------------------------------- *)

fun export dir namever =
    let
      val info =
          case Directory.peek dir namever of
            SOME i => i
          | NONE =>
            let
              val err =
                  "theory " ^ PackageNameVersion.toString namever ^
                  " is not installed"
            in
              raise Error err
            end

      val pkg = PackageInfo.package info

      val importer = Directory.importer dir

      val graph = TheoryGraph.empty {savable = false}

      val imps = TheorySet.empty

      val int = Interpretation.natural

      val (_,thy) =
          TheoryGraph.importPackageInfo importer graph
            {imports = imps,
             interpretation = int,
             info = info}

      val haskell = convert dir pkg thy

      val () = toPackage dir haskell
    in
      ()
    end;

end
