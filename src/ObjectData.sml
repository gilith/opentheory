(* ========================================================================= *)
(* OPENTHEORY OBJECT DATA                                                    *)
(* Copyright (c) 2004 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure ObjectData :> ObjectData =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory object data.                                         *)
(* ------------------------------------------------------------------------- *)

datatype data =
    Num of int
  | Name of Name.name
  | TypeOp of TypeOp.typeOp
  | Type of Type.ty
  | Const of Const.const
  | Var of Var.var
  | Term of Term.term
  | Thm of Thm.thm
  | List of data list;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* List data *)

fun destList d =
    case d of
      List ds => ds
    | _ => raise Error "ObjectData.destList";

val isList = can destList;

val mkNil = List [];

fun isNil d =
    case d of
      List [] => true
    | _ => false;

fun mkCons (h,t) =
    let
      val ds = destList t
    in
      List (h :: ds)
    end;

fun destCons l =
    case destList l of
      [] => raise Error "ObjectData.destCons: nil"
    | h :: t => (h, List t);

val isCons = can destCons;

(* Num data *)

fun destNum d =
    case d of
      Num i => i
    | _ => raise Error "ObjectData.destNum";

val isNum = can destNum;

(* Name data *)

fun destName d =
    case d of
      Name n => n
    | _ => raise Error "ObjectData.destName";

val isName = can destName;

(* Name list data *)

fun mkNames tys = List (List.map Name tys);

fun destNames d = List.map destName (destList d);

val isNames = can destNames;

(* Unit data *)

val unit = List [];

fun mkUnit () = unit;

fun isUnit d =
    case d of
      List [] => true
    | _ => false;

(* Pair data *)

fun mkPair (x,y) = List [x,y];

fun destPair d =
    case d of
      List [x,y] => (x,y)
    | _ => raise Error "ObjectData.destPair";

val isPair = can destPair;

(* Triple data *)

fun mkTriple (x,y,z) = List [x,y,z];

fun destTriple d =
    case d of
      List [x,y,z] => (x,y,z)
    | _ => raise Error "ObjectData.destTriple";

val isTriple = can destTriple;

(* Type operator data *)

fun destTypeOp d =
    case d of
      TypeOp ot => ot
    | _ => raise Error "ObjectData.destTypeOp";

val isTypeOp = can destTypeOp;

fun equalTypeOp ot d =
    case total destTypeOp d of
      SOME ot' => TypeOp.equal ot' ot
    | NONE => false;

(* Type data *)

fun destType d =
    case d of
      Type ty => ty
    | _ => raise Error "ObjectData.destType";

val isType = can destType;

(* Type list data *)

fun mkTypes tys = List (List.map Type tys);

fun destTypes d = List.map destType (destList d);

val isTypes = can destTypes;

(* Type variable type data *)

fun mkVarType n = Type (Type.mkVar n);

fun destVarType d =
    case d of
      Type ty => Type.destVar ty
    | _ => raise Error "ObjectData.destVarType";

val isVarType = can destVarType;

(* Type operator type data *)

fun mkOpType (ot,tys) = Type (Type.mkOp (ot,tys));

fun destOpType d =
    case d of
      Type ty => Type.destOp ty
    | _ => raise Error "ObjectData.destOpType";

val isOpType = can destOpType;

(* Constant data *)

fun destConst d =
    case d of
      Const c => c
    | _ => raise Error "ObjectData.destConst";

val isConst = can destConst;

fun equalConst c d =
    case total destConst d of
      SOME c' => Const.equal c' c
    | NONE => false;

(* Term variable data *)

fun destVar d =
    case d of
      Var v => v
    | _ => raise Error "ObjectData.destVar";

val isVar = can destVar;

(* Term data *)

fun destTerm d =
    case d of
      Term tm => tm
    | _ => raise Error "ObjectData.destTerm";

val isTerm = can destTerm;

(* Term list data *)

fun mkTerms tms = List (List.map Term tms);

fun destTerms d = List.map destTerm (destList d);

val isTerms = can destTerms;

(* Term variable term data *)

fun mkVarTerm v = Term (Term.mkVar v);

fun destVarTerm d = Term.destVar (destTerm d);

val isVarTerm = can destVarTerm;

(* Constant term data *)

fun mkConstTerm c_ty = Term (Term.mkConst c_ty);

fun destConstTerm d = Term.destConst (destTerm d);

val isConstTerm = can destConstTerm;

(* Function application term data *)

fun mkAppTerm f_a = Term (Term.mkApp f_a);

fun destAppTerm d = Term.destApp (destTerm d);

val isAppTerm = can destAppTerm;

(* Lambda abstraction term data *)

fun mkAbsTerm v_b = Term (Term.mkAbs v_b);

fun destAbsTerm d = Term.destAbs (destTerm d);

val isAbsTerm = can destAbsTerm;

(* Sequent data *)

fun mkSequent seq =
    let
      val Sequent.Sequent {hyp = h, concl = c} = seq

      val h = TermAlphaSet.toList h
    in
      (mkTerms h, Term c)
    end;

fun destSequent (h,c) =
    Sequent.Sequent
      {hyp = TermAlphaSet.fromList (destTerms h),
       concl = destTerm c};

val isSequent = can destSequent;

(* Theorem data *)

fun destThm d =
    case d of
      Thm th => th
    | _ => raise Error "ObjectData.destThm";

val isThm = can destThm;

(* Type substitution data *)

local
  fun mkMaplet (n,ty) = mkPair (Name n, Type ty);
in
  fun mkTypeSubst m = List (List.map mkMaplet (NameMap.toList m));
end;

local
  fun destMaplet d =
      let
        val (dN,dT) = destPair d
      in
        (destName dN, destType dT)
      end;
in
  fun destTypeSubst d =
      let
        val ms = List.map destMaplet (destList d)
      in
        TypeSubst.fromListMap ms
      end;
end;

val isTypeSubst = can destTypeSubst;

(* Term substitution data *)

local
  fun mkMaplet (v,tm) = mkPair (Var v, Term tm);
in
  fun mkTermSubst m = List (List.map mkMaplet (VarMap.toList m));
end;

local
  fun destMaplet d =
      let
        val (dV,dT) = destPair d
      in
        (destVar dV, destTerm dT)
      end;
in
  fun destTermSubst d =
      let
        val ms = List.map destMaplet (destList d)
      in
        TermSubst.fromListTermMap ms
      end;
end;

val isTermSubst = can destTermSubst;

(* Substitution data *)

fun mkSubst (tyS,tmS) = mkPair (mkTypeSubst tyS, mkTermSubst tmS);

fun destSubst d =
    let
      val (dY,dM) = destPair d
    in
      (destTypeSubst dY, destTermSubst dM)
    end;

val isSubst = can destSubst;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

fun compare d1_d2 =
    if Portable.pointerEqual d1_d2 then EQUAL
    else
      case d1_d2 of
        (Num n1, Num n2) => Int.compare (n1,n2)
      | (Num _, _) => LESS
      | (_, Num _) => GREATER
      | (Name n1, Name n2) => Name.compare (n1,n2)
      | (Name _, _) => LESS
      | (_, Name _) => GREATER
      | (TypeOp ot1, TypeOp ot2) => TypeOp.compare (ot1,ot2)
      | (TypeOp _, _) => LESS
      | (_, TypeOp _) => GREATER
      | (Type ty1, Type ty2) => Type.compare (ty1,ty2)
      | (Type _, _) => LESS
      | (_, Type _) => GREATER
      | (Const c1, Const c2) => Const.compare (c1,c2)
      | (Const _, _) => LESS
      | (_, Const _) => GREATER
      | (Var v1, Var v2) => Var.compare (v1,v2)
      | (Var _, _) => LESS
      | (_, Var _) => GREATER
      | (Term tm1, Term tm2) => Term.compare (tm1,tm2)
      | (Term _, _) => LESS
      | (_, Term _) => GREATER
      | (Thm th1, Thm th2) => Thm.compare (th1,th2)
      | (Thm _, _) => LESS
      | (_, Thm _) => GREATER
      | (List ds1, List ds2) => lexCompare compare (ds1,ds2);

fun equal d1 d2 = compare (d1,d2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Classes of object data.                                                   *)
(* ------------------------------------------------------------------------- *)

fun inDictionary d =
    case d of
      Num _ => false
    | Name _ => false
    | TypeOp _ => true
    | Type _ => true
    | Const _ => true
    | Var _ => true
    | Term _ => true
    | Thm _ => true
    | List l => not (List.null l);

fun termBuilder d =
    case d of
      Num _ => false
    | Name _ => false
    | TypeOp _ => true
    | Type _ => true
    | Const _ => true
    | Var _ => true
    | Term _ => true
    | Thm _ => false
    | List l => List.exists termBuilder l;

(* ------------------------------------------------------------------------- *)
(* Extracting theorems from object data.                                     *)
(* ------------------------------------------------------------------------- *)

local
  fun addList acc ds =
      case ds of
        [] => acc
      | d :: ds => addCons acc ds d

  and addCons acc ds d =
      case d of
        Thm th => addList (th :: acc) ds
      | List l => addList acc (List.revAppend (l,ds))
      | _ => addList acc ds;
in
  val thms = addCons [] [];
end;

(* ------------------------------------------------------------------------- *)
(* Extracting symbols from object data.                                      *)
(* ------------------------------------------------------------------------- *)

local
  fun addList sym ds =
      case ds of
        [] => sym
      | d :: ds => addCons ds sym d

  and addCons ds sym d =
      case d of
        Num _ => addList sym ds
      | Name _ => addList sym ds
      | TypeOp ot =>
        let
          val sym = SymbolTable.addTypeOp sym ot
        in
          addList sym ds
        end
      | Type ty =>
        let
          val sym = SymbolTable.addType sym ty
        in
          addList sym ds
        end
      | Const c =>
        let
          val sym = SymbolTable.addConst sym c
        in
          addList sym ds
        end
      | Var v =>
        let
          val sym = SymbolTable.addVar sym v
        in
          addList sym ds
        end
      | Term tm =>
        let
          val sym = SymbolTable.addTerm sym tm
        in
          addList sym ds
        end
      | Thm th =>
        let
          val sym = SymbolTable.addSequent sym (Thm.sequent th)
        in
          addList sym ds
        end
      | List l =>
        let
          val ds = List.revAppend (l,ds)
        in
          addList sym ds
        end;
in
  val symbolAddList = addList;

  val symbolAdd = addCons [];
end;

val symbol = symbolAdd SymbolTable.empty;

(* ------------------------------------------------------------------------- *)
(* Breaking down object data into commands.                                  *)
(* ------------------------------------------------------------------------- *)

fun command d =
    case d of
      Num i => (Command.Num i, [])
    | Name n => (Command.Name n, [])
    | TypeOp ot => (Command.TypeOp, [Name (TypeOp.name ot)])
    | Type ty =>
      (case Type.dest ty of
         TypeTerm.VarTy' n => (Command.VarType, [Name n])
       | TypeTerm.OpTy' (ot,tys) => (Command.OpType, [TypeOp ot, mkTypes tys]))
    | Const c => (Command.Const, [Name (Const.name c)])
    | Var (TypeTerm.Var (n,ty)) => (Command.Var, [Name n, Type ty])
    | Term tm =>
      (case Term.dest tm of
         TypeTerm.Const' (c,ty) => (Command.ConstTerm, [Const c, Type ty])
       | TypeTerm.Var' v => (Command.VarTerm, [Var v])
       | TypeTerm.App' (f,a) => (Command.AppTerm, [Term f, Term a])
       | TypeTerm.Abs' (v,b) => (Command.AbsTerm, [Var v, Term b]))
    | Thm th =>
      let
        val (h,c) = mkSequent (Thm.sequent th)
      in
        (Command.Axiom,[h,c])
      end
    | List l =>
      (case l of
         [] => (Command.Nil,[])
       | h :: t => (Command.Cons, [h, List t]));

(* ------------------------------------------------------------------------- *)
(* Searching for subterms.                                                   *)
(* ------------------------------------------------------------------------- *)

local
  fun searchList ds srch =
      case ds of
        [] => (NONE,srch)
      | d :: ds => searchCons d ds srch

  and searchCons d ds srch =
      case d of
        Num _ => searchList ds srch
      | Name _ => searchList ds srch
      | TypeOp _ => searchList ds srch
      | Type _ => searchList ds srch
      | Const _ => searchList ds srch
      | Var _ => searchList ds srch
      | Term tm =>
        let
          val subtm_srch as (subtm,srch) =
              TermSearch.sharingSearchTerm tm srch
        in
          if Option.isSome subtm then subtm_srch else searchList ds srch
        end
      | Thm th =>
        let
          val subtm_srch as (subtm,srch) =
              Sequent.sharingSearch (Thm.sequent th) srch
        in
          if Option.isSome subtm then subtm_srch else searchList ds srch
        end
      | List l =>
        let
          val ds =
              if TermSearch.leftToRight srch then l @ ds
              else List.revAppend (l,ds)
        in
          searchList ds srch
        end;
in
  fun sharingSearch d srch = searchList [d] srch;
end;

fun search srch d =
    let
      val (subtm,_) = sharingSearch d srch
    in
      subtm
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp d =
    case d of
      Num n => Command.pp (Command.Num n)
    | Name s => Command.pp (Command.Name s)
    | TypeOp ot => TypeOp.pp ot
    | Type ty => Type.pp ty
    | Const c => Const.pp c
    | Var v => Var.pp v
    | Term tm => Term.pp tm
    | Thm th => Thm.pp th
    | List l => Print.ppList pp l;

end

structure ObjectDataOrdered =
struct type t = ObjectData.data val compare = ObjectData.compare end

structure ObjectDataMap = KeyMap (ObjectDataOrdered)

structure ObjectDataSet = ElementSet (ObjectDataMap)
