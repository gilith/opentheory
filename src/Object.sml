(* ========================================================================= *)
(* OPENTHEORY OBJECTS                                                        *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Object :> Object =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory objects.                                             *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Num of int
  | Name of Name.name
  | TypeOp of TypeOp.typeOp
  | Type of Type.ty
  | Const of Const.const
  | Var of Var.var
  | Term of Term.term
  | Thm of Thm.thm
  | List of object list;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* List objects *)

fun destList ob =
    case ob of
      List l => l
    | _ => raise Error "destList";

val isList = can destList;

(* Num objects *)

fun destNum ob =
    case ob of
      Num i => i
    | _ => raise Error "destNum";

val isNum = can destNum;

(* Name objects *)

fun destName ob =
    case ob of
      Name n => n
    | _ => raise Error "destName";

val isName = can destName;

(* Name list objects *)

fun mkNames tys = List (List.map Name tys);

fun destNames ob = List.map destName (destList ob);

val isNames = can destNames;

(* Unit objects *)

val unit = List [];

fun mkUnit () = unit;

fun isUnit ob =
    case ob of
      List [] => true
    | _ => false;

(* Pair objects *)

fun mkPair (x,y) = List [x,y];

fun destPair ob =
    case ob of
      List [x,y] => (x,y)
    | _ => raise Error "Object.destPair";

val isPair = can destPair;

(* Triple objects *)

fun mkTriple (x,y,z) = List [x,y,z];

fun destTriple ob =
    case ob of
      List [x,y,z] => (x,y,z)
    | _ => raise Error "Object.destTriple";

val isTriple = can destTriple;

(* Type operator objects *)

fun destTypeOp ob =
    case ob of
      TypeOp ot => ot
    | _ => raise Error "Object.destTypeOp";

val isTypeOp = can destTypeOp;

(* Type objects *)

fun destType ob =
    case ob of
      Type ty => ty
    | _ => raise Error "Object.destType";

val isType = can destType;

(* Type list objects *)

fun mkTypes tys = List (List.map Type tys);

fun destTypes ob = List.map destType (destList ob);

val isTypes = can destTypes;

(* Type variable type objects *)

fun mkVarType n = Type (Type.mkVar n);

fun destVarType ob =
    case ob of
      Type ty => Type.destVar ty
    | _ => raise Error "Object.destVarType";

val isVarType = can destVarType;

(* Type operator type objects *)

fun mkOpType (ot,tys) = Type (Type.mkOp (ot,tys));

fun destOpType ob =
    case ob of
      Type ty => Type.destOp ty
    | _ => raise Error "Object.destOpType";

val isOpType = can destOpType;

(* Constant objects *)

fun destConst ob =
    case ob of
      Const c => c
    | _ => raise Error "Object.destConst";

val isConst = can destConst;

(* Term variable objects *)

fun destVar ob =
    case ob of
      Var v => v
    | _ => raise Error "Object.destVar";

val isVar = can destVar;

(* Term objects *)

fun destTerm ob =
    case ob of
      Term tm => tm
    | _ => raise Error "Object.destTerm";

val isTerm = can destTerm;

(* Term list objects *)

fun mkTerms tms = List (List.map Term tms);

fun destTerms ob = List.map destTerm (destList ob);

val isTerms = can destTerms;

(* Term variable term objects *)

fun mkVarTerm v = Term (Term.mkVar v);

fun destVarTerm ob = Term.destVar (destTerm ob);

val isVarTerm = can destVarTerm;

(* Constant term objects *)

fun mkConstTerm c_ty = Term (Term.mkConst c_ty);

fun destConstTerm ob = Term.destConst (destTerm ob);

val isConstTerm = can destConstTerm;

(* Function application term objects *)

fun mkAppTerm f_a = Term (Term.mkApp f_a);

fun destAppTerm ob = Term.destApp (destTerm ob);

val isAppTerm = can destAppTerm;

(* Lambda abstraction term objects *)

fun mkAbsTerm v_b = Term (Term.mkAbs v_b);

fun destAbsTerm ob = Term.destAbs (destTerm ob);

val isAbsTerm = can destAbsTerm;

(* Sequent objects *)

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

(* Theorem objects *)

fun destThm ob =
    case ob of
      Thm th => th
    | _ => raise Error "Object.destThm";

val isThm = can destThm;

(* Type substitution objects *)

local
  fun mkMaplet (n,ty) = mkPair (Name n, Type ty);
in
  fun mkTypeSubst m = List (List.map mkMaplet (NameMap.toList m));
end;

local
  fun destMaplet ob =
      let
        val (obN,obT) = destPair ob
      in
        (destName obN, destType obT)
      end;
in
  fun destTypeSubst ob =
      let
        val ms = List.map destMaplet (destList ob)
      in
        TypeSubst.fromListMap ms
      end;
end;

val isTypeSubst = can destTypeSubst;

(* Term substitution objects *)

local
  fun mkMaplet (v,tm) = mkPair (Var v, Term tm);
in
  fun mkTermSubst m = List (List.map mkMaplet (VarMap.toList m));
end;

local
  fun destMaplet ob =
      let
        val (obV,obT) = destPair ob
      in
        (destVar obV, destTerm obT)
      end;
in
  fun destTermSubst ob =
      let
        val ms = List.map destMaplet (destList ob)
      in
        TermSubst.fromListTermMap ms
      end;
end;

val isTermSubst = can destTermSubst;

(* Substitution objects *)

fun mkSubst (tyS,tmS) = mkPair (mkTypeSubst tyS, mkTermSubst tmS);

fun destSubst ob =
    let
      val (obY,obM) = destPair ob
    in
      (destTypeSubst obY, destTermSubst obM)
    end;

val isSubst = can destSubst;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

fun compare ob1_ob2 =
    if Portable.pointerEqual ob1_ob2 then EQUAL
    else
      case ob1_ob2 of
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
      | (List l1, List l2) => lexCompare compare (l1,l2);

(* ------------------------------------------------------------------------- *)
(* Extracting the theorems stored in an object.                              *)
(* ------------------------------------------------------------------------- *)

val thms =
    let
      fun f acc obs =
          case obs of
            [] => acc
          | ob :: obs =>
            case ob of
              Thm th => f (th :: acc) obs
            | List l => f acc (l @ obs)
            | _ => f acc obs
    in
      fn ob => f [] [ob]
    end;

(* ------------------------------------------------------------------------- *)
(* Extracting the symbols in an object.                                      *)
(* ------------------------------------------------------------------------- *)

fun symbolAddList sym obs =
    case obs of
      [] => sym
    | ob :: obs =>
      case ob of
        Num _ => symbolAddList sym obs
      | Name _ => symbolAddList sym obs
      | TypeOp ot => Symbol.addTypeOp sym ot
      | Type ty =>
        let
          val sym = Symbol.addType sym ty
        in
          symbolAddList sym obs
        end
      | Const c => Symbol.addConst sym c
      | Var v => Symbol.addVar sym v
      | Term tm =>
        let
          val sym = Symbol.addTerm sym tm
        in
          symbolAddList sym obs
        end
      | Thm th =>
        let
          val sym = Symbol.addSequent sym (Thm.sequent th)
        in
          symbolAddList sym obs
        end
      | List l =>
        let
          val obs = l @ obs
        in
          symbolAddList sym obs
        end;

fun symbolAdd sym ob = symbolAddList sym [ob];

val symbol = symbolAdd Symbol.empty;

(* ------------------------------------------------------------------------- *)
(* Breaking down objects into commands.                                      *)
(* ------------------------------------------------------------------------- *)

fun command ob =
    case ob of
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
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp ob =
    case ob of
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

structure ObjectOrdered =
struct type t = Object.object val compare = Object.compare end

structure ObjectMap = KeyMap (ObjectOrdered)

structure ObjectSet = ElementSet (ObjectMap)
