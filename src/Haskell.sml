(* ========================================================================= *)
(* GENERATING HASKELL PROJECTS FROM THEORIES                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Haskell :> Haskell =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A prefix marking theories that can be exported as Haskell packages.       *)
(* ------------------------------------------------------------------------- *)

val prefix = PackageName.haskellExport;

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
    let
      val old = PackageName.haskellExport
      and new = PackageName.newHaskellExport
    in
      if PackageName.equal name old then new
      else
        case PackageName.destStrictPrefix old name of
          SOME n => PackageName.append new n
        | NONE =>
          let
            val err =
                "non-Haskell package name: " ^ PackageName.toString name
          in
            raise Error err
          end
    end;

val opentheoryNamespace = Namespace.fromList ["OpenTheory"];

local
  val haskellNamespace = Namespace.fromList ["Haskell"];
in
  fun exportNamespace ns =
      case Namespace.rewrite (haskellNamespace,opentheoryNamespace) ns of
        SOME ns => ns
      | NONE => raise Error ("non-Haskell namespace: " ^ Namespace.toString ns);
end;

local
  val mkOpentheoryNamespace = Namespace.append opentheoryNamespace;

  fun mkOpentheoryName ns s =
      Name.mk (mkOpentheoryNamespace ns, s);

  val mkNaturalName = mkOpentheoryName Namespace.natural;

  val typeOpMapping =
      NameMap.fromList
        [(Name.boolTypeOp, Name.mkGlobal "Bool"),
         (Name.funTypeOp, Name.mkGlobal "->"),
         (Name.listTypeOp, Name.mkGlobal "List"),
         (Name.naturalTypeOp, mkNaturalName "Natural"),
         (Name.optionTypeOp, Name.mkGlobal "Maybe"),
         (Name.pairTypeOp, Name.mkGlobal "Pair")];

  val constMapping =
      NameMap.fromList
        [(Name.bit0Const, Name.mkGlobal "--bit0--"),
         (Name.bit1Const, Name.mkGlobal "--bit1--"),
         (Name.condConst, Name.mkGlobal "--cond--"),
         (Name.conjConst, Name.mkGlobal "&&"),
         (Name.consConst, Name.mkGlobal ":"),
         (Name.eqConst, Name.mkGlobal "=="),
         (Name.forallConst, Name.mkGlobal "--forall--"),
         (Name.nilConst, Name.mkGlobal "[]"),
         (Name.noneConst, Name.mkGlobal "Nothing"),
         (Name.pairConst, Name.mkGlobal ","),
         (Name.selectConst, Name.mkGlobal "--select--"),
         (Name.someConst, Name.mkGlobal "Just"),
         (Name.sucConst, mkNaturalName "suc"),
         (Name.zeroConst, mkNaturalName "zero")];

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

(***
local
  fun exportTypeVarName v =
      let
        val (ns,s) = Name.dest v

        val () =
            if Namespace.isGlobal ns then ()
            else raise Error "non-global type variable"

        val cs = String.explode s

        val cs = dropWhile (not o Char.isAlpha) cs

        val () =
            if List.all Char.isAlphaNum cs then ()
            else raise Error "non-alphanumerical chars in type variable"

        val cs =
            case cs of
              [] => raise Error "empty type variable"
            | c :: ct =>
              if Char.isLower c then cs else Char.toLower c :: ct

        val s' = String.implode cs
      in
        if s = s' then NONE else SOME (Name.mkGlobal s')
      end;
in
  fun exportTypeVarNames vs =
      let
        fun add (v,acc) =
            case exportTypeVarName v of
              NONE => acc
            | SOME v' =>
              let
                val () =
                    if not (NameSet.member v' vs) then ()
                    else raise Error "type variable name clash"
              in
                (v, Type.mkVar v') :: acc
              end

        val tymap = NameSet.foldr add [] vs
      in
        TypeSubst.mk (TypeSubst.fromListMap tymap)
      end;
end;
***)

local
  fun addName (n,ns) = NameSet.add ns n;

  fun addTypeOp (ot,ns) =
      let
        val n = TypeOp.name ot

        val n = exportTypeOpName n
      in
        addName (n,ns)
      end;

  fun addConst (c,ns) =
      let
        val n = Const.name c

        val nl =
            case total Name.destCase n of
              SOME (_,nl) => nl
            | NONE => [n]

        val nl = List.map exportConstName nl
      in
        List.foldl addName ns nl
      end;
in
  fun exportSymbolTableNames table =
      let
        val ns = NameSet.empty

        val ns = TypeOpSet.foldl addTypeOp ns (SymbolTable.typeOps table)

        val ns = ConstSet.foldl addConst ns (SymbolTable.consts table)
      in
        ns
      end;
end;

fun exportSymbolTableNamespaces table =
    NameSet.namespace (exportSymbolTableNames table);

(* ------------------------------------------------------------------------- *)
(* A type of Haskell packages.                                               *)
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
       predicate : Term.term option,
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

datatype haskell =
     Haskell of
       {package : Package.package,
        source : module};

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
             predicate = pred,
             repType,
             abs,
             rep} = newtype

      val sym = SymbolTable.empty

      val sym = SymbolTable.addTypeOp sym name

      val sym =
          case pred of
            SOME tm => SymbolTable.addTerm sym tm
          | NONE => sym

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
             predicate = _,
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

      val (absRep,repAbs) =
          case total Term.destConj concl of
            SOME (ar,ra) => (ar, SOME ra)
          | NONE => (concl,NONE)

      val (abs,rep) =
          let
            val (a,t0) = Term.destForall absRep

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

      val pred =
          case repAbs of
            NONE => NONE
          | SOME tm =>
            let
              val (r,t0) = Term.destForall tm

              val (pred,t1) = Term.destImp t0

              val () =
                  let
                    val vs = Term.freeVars pred
                  in
                    if VarSet.subset vs (VarSet.singleton r) then ()
                    else raise Error "extra free vars in predicate"
                  end

              val (t2,t3) = Term.destEq t1

              val () =
                  if Term.equalVar r t3 then ()
                  else raise Error "bad rhs of repAbs"

              val (rep',t4) = Term.destApp t2

              val () =
                  if Term.equal rep' rep' then ()
                  else raise Error "different reps in absRep and repAbs"

              val (abs',t5) = Term.destApp t4

              val () =
                  if Term.equal abs' abs then ()
                  else raise Error "different abs in absRep and repAbs"

              val () =
                  if Term.equalVar r t5 then ()
                  else raise Error "bad var inside repAbs"
            in
              SOME (Term.mkAbs (r,pred))
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
         predicate = pred,
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

        val sccl = SymbolGraph.preOrderSCC graph (SymbolGraph.vertexList graph)
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

  and addTree namespace (n,nsource) =
      let
        val namespace = Namespace.mkNested (namespace,n)
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

        val src = getTheory PackageName.srcHaskellExport thys
        and test = getTheory PackageName.testHaskellExport thys
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

fun convert pkg thy =
    let
      val {src, test = _} = splitTheories thy

      val source = mkModule (destSourceTheory src)
    in
      Haskell
        {package = pkg,
         source = source}
    end;

(* ------------------------------------------------------------------------- *)
(* Printing Haskell source code.                                             *)
(* ------------------------------------------------------------------------- *)

(* Names *)

fun ppPackageName name = PackageName.pp (exportPackageName name);

local
  fun relativeNamespace namespace ns =
      case Namespace.rewrite (namespace,Namespace.global) ns of
        SOME ns => ns
      | NONE => ns;

  fun relativeName namespace n =
      let
        val (ns,n) = Name.dest n

        val ns = relativeNamespace namespace ns
      in
        Name.mk (ns,n)
      end;

  fun exportRelativeNamespace namespace =
      let
        val namespace = exportNamespace namespace
      in
        fn ns => relativeNamespace namespace (exportNamespace ns)
      end;

  fun exportRelativeTypeOpName namespace =
      let
        val namespace = exportNamespace namespace
      in
        fn n => relativeName namespace (exportTypeOpName n)
      end;

  fun exportRelativeConstName namespace =
      let
        val namespace = exportNamespace namespace
      in
        fn n => relativeName namespace (exportConstName n)
      end;

  fun exportImportNamespaces source =
      let
        val white =
            let
              val table = symbolTableSourceList source
            in
              exportSymbolTableNamespaces table
            end

        val black =
            let
              val table = definedSymbolTableSourceList source
            in
              exportSymbolTableNamespaces table
            end

        val black = NamespaceSet.add black Namespace.global
      in
        NamespaceSet.difference white black
      end;

  fun ppImportNamespace namespace ns =
      let
        val namespace = exportNamespace namespace

        val ns' = relativeNamespace namespace ns

        val ppImport =
            [Print.ppString "import",
             Print.space,
             Print.ppString "qualified",
             Print.space,
             Namespace.pp ns]

        val ppAs =
            if Namespace.equal ns' ns then []
            else
              [Print.break,
               Print.ppString "as",
               Print.space,
               Namespace.pp ns']

        val ppImportAs = Print.inconsistentBlock 2 (ppImport @ ppAs)
      in
        Print.sequence ppImportAs Print.newline
      end;
in
  fun ppFullNamespace ns = Namespace.pp (exportNamespace ns);

  fun ppModuleImport namespace source =
      let
        val import = exportImportNamespaces source

        val import = NamespaceSet.toList import
      in
        Print.inconsistentBlock 0
          (List.map (ppImportNamespace namespace) import)
      end;

  fun ppNamespace namespace ns =
      Namespace.pp (exportRelativeNamespace namespace ns);

  fun ppTypeOpName namespace n =
      Name.pp (exportRelativeTypeOpName namespace n);

  fun ppConstName namespace n =
      Name.pp (exportRelativeConstName namespace n);
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

fun ppConst ns c = ppConstName ns (Const.name c);

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
         {token = "seq", precedence = 0, assoc = Print.RightAssoc},
         {token = ",", precedence = ~1000, assoc = Print.RightAssoc}];

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

  fun ppInfixToken (_,s) = Print.ppString s;

  val ppInfix = Print.ppInfixes infixes (total destInfix) ppInfixToken;

  fun destGenApp tm =
      if Term.isNumeral tm then
        raise Error "Haskell.ppTerm.destGenApp: numeral"
      else if Term.isCond tm then
        raise Error "Haskell.ppTerm.destGenApp: cond"
      else if Term.isLet tm then
        raise Error "Haskell.ppTerm.destGenApp: let"
      else if isInfix tm then
        raise Error "Haskell.ppTerm.destGenApp: infix"
      else if Term.isGenAbs tm then
        raise Error "Haskell.ppTerm.destGenApp: abstraction"
      else if Term.isCase tm then
        raise Error "Haskell.ppTerm.destGenApp: case"
      else Term.destApp tm;

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
                     ppSyntax "<-",
                     Print.break,
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

        and ppCaseTerm (a,bs,r) =
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
                    val ty = Term.typeOf a

                    fun mkBranch (n,xs,t) =
                        let
                          val c = Const.mkUndef n

                          val ty = Type.listMkFun (List.map Term.typeOf xs, ty)

                          val pat = Term.listMkApp (Term.mkConst (c,ty), xs)
                        in
                          (pat,(t,true))
                        end
                  in
                    case List.rev (List.map mkBranch bs) of
                      [] => raise Bug "Haskell.ppTerm.ppCaseTerm: no branches"
                    | (pat,(t,_)) :: rest => List.rev ((pat,(t,r)) :: rest)
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
                  SOME (a,bs) => ppCaseTerm (a,bs,r)
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
               predicate = pred,
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

local
  fun ppModuleDeclaration namespace =
      Print.inconsistentBlock 0
        [Print.ppString "module ",
         ppFullNamespace namespace,
         Print.newline,
         Print.ppString "where"];
in
  fun ppModule (pkg,namespace,source) =
      let
        val {description} = Package.description pkg
        and {license} = Package.license pkg
        and {author} = Package.author pkg
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
             [("Maintainer", Print.ppString author),
              ("Stability", Print.ppString "provisional"),
              ("Portability", Print.ppString "portable")],
           Print.newline,
           Print.ppString "-}",
           Print.newline,
           ppModuleDeclaration namespace,
           Print.newlines 2,
           ppModuleImport namespace source,
           Print.newline,
           ppSourceList namespace source]
      end;
end;

(* Cabal *)

local
  val ppVersion = PackageVersion.pp;
(***
  fun ppVersion version =
      let
        val today = Date.fromTimeLocal (Time.now ())

        val y = Date.year today
        and m = Date.month today
        and d = Date.day today

        val m =
            case m of
              Date.Jan => 1
            | Date.Feb => 2
            | Date.Mar => 3
            | Date.Apr => 4
            | Date.May => 5
            | Date.Jun => 6
            | Date.Jul => 7
            | Date.Aug => 8
            | Date.Sep => 9
            | Date.Oct => 10
            | Date.Nov => 11
            | Date.Dec => 12
      in
        Print.program
          [PackageVersion.pp version,
           Print.ppString ".",
           Print.ppInt y,
           Print.ppString ".",
           Print.ppInt m,
           Print.ppString ".",
           Print.ppInt d]
      end;
***)

  fun ppSection s pps =
      Print.inconsistentBlock 2
        [Print.ppString s,
         Print.newline,
         Print.program pps];

  val ppBuildDepends =
      [Print.ppString "base >= 4.0 && < 5.0",
       Print.ppString ",",
       Print.newline,
       Print.ppString "opentheory >= 1.0 && < 2.0"];

  fun ppExposedModules mods =
      case NamespaceSet.toList mods of
        [] => []
      | ns :: nss =>
        ppFullNamespace ns ::
        List.map (Print.sequence Print.newline o ppFullNamespace) nss;
in
  fun ppCabal (pkg,source) =
      let
        val name = Package.name pkg
        and version = Package.version pkg
        and {description} = Package.description pkg
        and {license} = Package.license pkg
        and {author} = Package.author pkg
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
              ("Author", Print.ppString author),
              ("Maintainer", Print.ppString author)],
           Print.newline,
           Print.newline,
           ppSection "Library"
             [ppSection "Build-depends:" ppBuildDepends,
              Print.newline,
              Print.newline,
              ppTag ("hs-source-dirs", Print.ppString "src"),
              Print.newline,
              Print.newline,
              ppTag ("ghc-options", Print.ppString "-Wall -Werror"),
              Print.newline,
              Print.newline,
              ppSection "Exposed-modules:" (ppExposedModules mods)]]
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

fun outputCabal {directory = dir} pkg source =
    let
      val ss = Print.toStream ppCabal (pkg,source)

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

  fun outputMod pkg dir ns module =
      let
        val Module {namespace,source,submodules} = module

        val sub =
            let
              val pp =
                  if Namespace.isGlobal ns then ppFullNamespace
                  else ppNamespace ns
            in
              Print.toLine pp namespace
            end

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

fun toPackage dir haskell =
    let
      val Haskell {package,source} = haskell

      val directory =
          let
            val name = Print.toLine ppPackageName (Package.name package)

            val directory = {directory = OS.FileSys.getDir ()}
          in
            mkSubDirectory directory name
          end

      val () = outputCabal directory package source

      val () = outputLicense dir directory package

      val () = outputSource directory package source
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

      val haskell = convert pkg thy

      val () = toPackage dir haskell
    in
      ()
    end;

end
