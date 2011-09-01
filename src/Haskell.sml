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
        [(Name.conjConst, Name.mkGlobal "&&"),
         (Name.consConst, Name.mkGlobal ":"),
         (Name.nilConst, Name.mkGlobal "[]"),
         (Name.noneConst, Name.mkGlobal "Nothing"),
         (Name.someConst, Name.mkGlobal "Just"),
         (Name.sucConst, mkNaturalName "suc")];

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
      case NameMap.peek constMapping n of
        SOME n => n
      | NONE =>
        exportName n
        handle Error err =>
          let
            val err = "bad constant name: " ^ Name.toString n ^ "\n" ^ err
          in
            raise Error err
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

(* ------------------------------------------------------------------------- *)
(* A type of Haskell packages.                                               *)
(* ------------------------------------------------------------------------- *)

datatype data =
    Data of
      {name : TypeOp.typeOp,
       parameters : Name.name list,
       constructors : (Const.const * Type.ty list) list};

datatype newtype =
    Newtype of
      {name : TypeOp.typeOp,
       predicate : Term.term,
       abs : Const.const,
       rep : Const.const};

datatype value =
    Value of
      {name : Const.const,
       ty : Type.ty,
       equations : (Term.term list * Term.term) list};

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
(* Converting theorems into Haskell declarations.                            *)
(* ------------------------------------------------------------------------- *)

local
  fun destIndData tm =
      let
        val (p,t0) = Term.destForall tm

        fun destP t =
            let
              val (p',x) = Term.destApp t

              val () =
                  if Term.equalVar p p' then ()
                  else raise Error "bad p"
            in
              x
            end

        val (t1,t2) = Term.destImp t0

        val ty =
            let
              val (x,px) = Term.destForall t2

              val x' = destP px

              val () =
                  if Term.equalVar x x' then ()
                  else raise Error "bad x"
            in
              Var.typeOf x
            end

        fun destCon t =
            let
              val (vs,t3) = Term.stripForall t

              val t4 =
                  case total Term.destImp t3 of
                    NONE => t3
                  | SOME (t5,t6) =>
                    let
                      val vs = VarSet.fromList vs

                      fun check t =
                          if VarSet.member (Term.destVar (destP t)) vs then ()
                          else raise Error "bad step assumption"

                      val () = List.app check (Term.stripConj t5)
                    in
                      t6
                    end

              val t5 = destP t4

              val (t6,ts) = Term.stripApp t5

              val (c,_) = Term.destConst t6
            in
              (c, List.map Term.typeOf ts)
            end

        val (name,parms) =
            let
              val (name,parms) = Type.destOp ty

              val vs = List.map Type.destVar parms

              val () =
                  if NameSet.size (NameSet.fromList vs) = length vs then ()
                  else raise Error "duplicate type vars"
            in
              (name,vs)
            end

        val ts = Term.stripConj t1
      in
        Data
          {name = name,
           parameters = parms,
           constructors = List.map destCon ts}
      end
      handle Error err =>
        raise Error ("bad induction conjunct: " ^ err);

  fun destRecData tm =
      let
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad recursion conjunct: " ^ err);

  fun destCaseData tm =
      let
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad case conjunct: " ^ err);
in
  fun destData th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val (indData,recCaseData) = Term.destConj concl

        val (recData,caseData) = Term.destConj recCaseData

        val indData = destIndData indData
(***
        and recData = destRecData recData
        and caseData = destCaseData caseData
***)
      in
        indData
      end
      handle Error err =>
        raise Error ("bad data theorem: " ^ err);
end;

local
in
  fun destNewtype th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val (absRep,repAbs) = Term.destConj concl

        val (ty,abs,rep) =
            let
              val (a,t0) = Term.destForall absRep

              val ty = Var.typeOf a

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
              (ty,abs,rep)
            end

        val (ty',pred,abs',rep') =
            let
              val (r,t0) = Term.destForall repAbs

              val (pred,t1) = Term.destEq t0

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

              val (rep,t4) = Term.destApp t2

              val ty = Term.typeOf t4

              val (abs,t5) = Term.destApp t4

              val () =
                  if Term.equalVar r t5 then ()
                  else raise Error "bad var inside repAbs"
            in
              (ty,pred,abs,rep)
            end

        val () =
            if Type.equal ty ty' then ()
            else raise Error "different types in absRep and repAbs"

        val () =
            if Term.equal abs abs' then ()
            else raise Error "different abstractions in absRep and repAbs"

        val () =
            if Term.equal rep rep' then ()
            else raise Error "different representations in absRep and repAbs"

        val (name,_) = Type.destOp ty
        and (abs,_) = Term.destConst abs
        and (rep,_) = Term.destConst rep
      in
        Newtype
          {name = name,
           predicate = pred,
           abs = abs,
           rep = rep}
      end
      handle Error err =>
        raise Error ("bad newtype theorem: " ^ err);
end;

local
  fun destEqn tm =
      let
        val (_,tm) = Term.stripForall tm

        val (l,r) = Term.destEq tm

        val (f,a) = Term.stripApp l
      in
        ((f, length a), (a,r))
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
      | _ => raise Bug "Haskell.destSource: ambiguous"
    end;

(* ------------------------------------------------------------------------- *)
(* Sorting Haskell declarations into a module hierarchy.                     *)
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
  fun symbolTableData d =
      let
        val Data {name, parameters = _, constructors = cons} = d

        val sym = SymbolTable.empty

        val sym = SymbolTable.addTypeOp sym name

        val sym = List.foldl addConstructor sym cons
      in
        sym
      end;
end;

fun symbolNewtype (Newtype {name,...}) = Symbol.TypeOp name;

fun symbolTableNewtype n =
    let
      val Newtype {name, predicate = pred, abs, rep} = n

      val sym = SymbolTable.empty

      val sym = SymbolTable.addTypeOp sym name

      val sym = SymbolTable.addTerm sym pred

      val sym = SymbolTable.addConst sym abs

      val sym = SymbolTable.addConst sym rep
    in
      sym
    end;

fun symbolValue (Value {name,...}) = Symbol.Const name;

local
  fun addEquation ((args,tm),sym) =
      let
        val sym = SymbolTable.addTermList sym args

        val sym = SymbolTable.addTerm sym tm
      in
        sym
      end;
in
  fun symbolTableValue v =
      let
        val Value {name, ty, equations = eqns} = v

        val sym = SymbolTable.empty

        val sym = SymbolTable.addConst sym name

        val sym = SymbolTable.addType sym ty

        val sym = List.foldl addEquation sym eqns
      in
        sym
      end;
end;

fun symbolSource s =
    case s of
      DataSource x => symbolData x
    | NewtypeSource x => symbolNewtype x
    | ValueSource x => symbolValue x;

fun nameSource s = Symbol.name (symbolSource s);

fun namespaceSource s = Name.namespace (nameSource s);

fun symbolTableSource s =
    case s of
      DataSource x => symbolTableData x
    | NewtypeSource x => symbolTableNewtype x
    | ValueSource x => symbolTableValue x;

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
        val namespace = Namespace.append namespace (Namespace.fromString n)
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
    in
      sortSource (List.map destSource ths)
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
in
  fun ppFullNamespace ns = Namespace.pp (exportNamespace ns);

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
  fun ppBasic ns ty =
      case Type.dest ty of
        TypeTerm.VarTy' n => ppTypeVar n
      | TypeTerm.OpTy' (ot,tys) =>
        if List.null tys then ppTypeOp ns ot
        else Print.ppBracket "(" ")" (ppGen ns) ty

  and ppSpaceBasic ns ty =
      Print.sequence Print.break (ppBasic ns ty)

  and ppSpaceBasics ns tys =
      Print.program (List.map (ppSpaceBasic ns) tys)

  and ppGen ns ty =
      case Type.dest ty of
        TypeTerm.VarTy' _ => ppBasic ns ty
      | TypeTerm.OpTy' (ot,tys) =>
        if List.null tys then ppBasic ns ty
        else
          Print.inconsistentBlock 2
            [ppTypeOp ns ot,
             ppSpaceBasics ns tys];
in
  val ppType = ppGen;

  val ppTypeList = ppSpaceBasics;
end;

(* Terms *)

fun ppConst ns c = ppConstName ns (Const.name c);

fun ppVar v = ppVarName (Var.name v);

local
  val infixes =
      Print.Infixes
        [(* ML *)
         {token = "/", precedence = 7, assoc = Print.LeftAssoc},
         {token = "div", precedence = 7, assoc = Print.LeftAssoc},
         {token = "mod", precedence = 7, assoc = Print.LeftAssoc},
         {token = "*", precedence = 7, assoc = Print.LeftAssoc},
         {token = "+", precedence = 6, assoc = Print.LeftAssoc},
         {token = "-", precedence = 6, assoc = Print.LeftAssoc},
         {token = "^", precedence = 6, assoc = Print.LeftAssoc},
         {token = "@", precedence = 5, assoc = Print.RightAssoc},
         {token = ":", precedence = 5, assoc = Print.RightAssoc},
         {token = "==", precedence = 4, assoc = Print.NonAssoc},
         {token = "<>", precedence = 4, assoc = Print.NonAssoc},
         {token = "<=", precedence = 4, assoc = Print.NonAssoc},
         {token = "<", precedence = 4, assoc = Print.NonAssoc},
         {token = ">=", precedence = 4, assoc = Print.NonAssoc},
         {token = ">", precedence = 4, assoc = Print.NonAssoc},
         {token = ".", precedence = 3, assoc = Print.LeftAssoc},
         {token = "&&", precedence = ~1, assoc = Print.RightAssoc},
         {token = "||", precedence = ~2, assoc = Print.RightAssoc},
         {token = ",", precedence = ~1000, assoc = Print.RightAssoc}];

  val infixNames =
      NameMap.fromList
        [(Name.eqConst,"==")];

  fun destInfixTerm tm =
      let
        val (t,b) = Term.destApp tm

        val (t,a) = Term.destApp t

        val (c,_) = Term.destConst t

        val n = Const.name c
      in
        (n,a,b)
      end;

  fun destInfix tm =
      let
        val (c,a,b) = destInfixTerm tm
      in
        case NameMap.peek infixNames c of
          SOME s => (s,a,b)
        | NONE => raise Error "Haskell.ppTerm.destInfix"
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
              fun ppArg x = Print.sequence Print.break (ppBasicTerm x)

              val (tm,xs) = stripGenApp tm
            in
              if List.null xs then ppBasicTerm tm
              else
                Print.inconsistentBlock 0
                  [ppBasicTerm tm,
                   Print.inconsistentBlock 2
                     (Print.ppString "" :: List.map ppArg xs)]
            end

        and ppBoundVars (v,vs) =
            Print.sequence
              (Print.sequence
                 (ppBasicTerm v)
                 (Print.program
                   (List.map
                     (Print.sequence Print.break o ppBasicTerm) vs)))
              (Print.ppString " ->")

        and ppBindTerm (v,vs,body) =
            Print.inconsistentBlock 2
              [Print.ppString "\\",
               ppBoundVars (v,vs),
               Print.break,
               ppNormalTerm body]

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

        and ppCondTerm (f,c,a,b,r) =
            Print.inconsistentBlock 0
              [Print.consistentBlock 0
                 [Print.inconsistentBlock (if f then 3 else 8)
                    [Print.ppString (if f then "if " else "else if "),
                     ppInfixTerm (c,true)],
                  Print.break,
                  Print.ppString "then"],
               Print.inconsistentBlock 2
                 [Print.ppString "",
                  Print.break,
                  ppLetCondTerm (a,true)]] ::
            Print.break ::
            (case total Term.destCond b of
               SOME (c,a,b) => ppCondTerm (false,c,a,b,r)
             | NONE =>
                 [Print.inconsistentBlock 2
                    [Print.ppString "else",
                     Print.break,
                     ppLetCondTerm (b,r)]])

        and ppLetTerm (v,t,b,r) =
            Print.inconsistentBlock 4
              [Print.ppString "let ",
               ppApplicationTerm v,
               Print.ppString " =",
               Print.break,
               ppLetCondTerm (t,true),
               Print.ppString " in"] ::
            Print.break ::
            (case total Term.destLet b of
               NONE => [ppLetCondTerm (b,r)]
             | SOME (v,t,b) => ppLetTerm (v,t,b,r))

        and ppLetCondTerm (tm,r) =
            case total Term.destLet tm of
              SOME (v,t,b) =>
                Print.consistentBlock 0
                  (ppLetTerm (v,t,b,r))
            | NONE =>
              case total Term.destCond tm of
                SOME (c,a,b) =>
                Print.consistentBlock 0
                  (ppCondTerm (true,c,a,b,r))
              | NONE => ppInfixTerm (tm,r)

        and ppLetConditionalTerm (tm,r) =
            if not (Term.isLet tm orelse Term.isCond tm) then
              ppBinderTerm (tm,r)
            else if r then
              ppBracketTerm tm
            else
              ppLetCondTerm (tm,false)

        and ppInfixTerm tm_r = ppInfix ppLetConditionalTerm tm_r

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
        val Data {name, parameters = parms, constructors = cons} = data
      in
        Print.inconsistentBlock 2
          [ppDecl ns (name,parms),
           ppCons ns cons]
    end;
end;

fun ppNewtype ns newtype =
    let
      val Newtype {name, predicate = pred, abs, rep} = newtype
    in
      Print.inconsistentBlock 2
        []
    end;

local
  fun ppDecl ns (name,ty) =
      Print.inconsistentBlock 2
        [ppConst ns name,
         Print.ppString " ::",
         Print.break,
         ppType ns ty];

  fun ppEqn ns name ty (args,rtm) =
      let
        val ltm = Term.listMkApp (Term.mkConst (name,ty), args)
      in
        Print.inconsistentBlock 2
          [ppTerm ns ltm,
           Print.ppString " =",
           Print.break,
           ppTerm ns rtm]
      end;
in
  fun ppValue ns value =
      let
        val Value {name, ty, equations = eqns} = value
      in
        Print.inconsistentBlock 0
          (ppDecl ns (name,ty) ::
           List.map (Print.sequence Print.newline o ppEqn ns name ty) eqns)
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
           Print.newline,
           Print.newline,
           ppTags
             [("Maintainer", Print.ppString author),
              ("Stability", Print.ppString "provisional"),
              ("Portability", Print.ppString "portable")],
           Print.newline,
           Print.ppString "-}",
           Print.newline,
           ppModuleDeclaration namespace,
           Print.newline,
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
