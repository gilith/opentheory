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
    Error
  | Int of int
  | Name of Name.name
  | TypeOp of TypeOp.typeOp
  | Type of Type.ty
  | Const of Const.const
  | Var of Var.var
  | Term of Term.term
  | Thm of Thm.thm
  | List of object list
  | Call of Name.name;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* Error objects *)

fun destError ob =
    case ob of
      Error => ()
    | _ => raise Useful.Error "destError";

val isError = can destError;

(* Int objects *)

fun destInt ob =
    case ob of
      Int i => i
    | _ => raise Useful.Error "destInt";

val isInt = can destInt;

(* Name objects *)

fun destName ob =
    case ob of
      Name n => n
    | _ => raise Useful.Error "destName";

val isName = can destName;

(* List objects *)

fun destList ob =
    case ob of
      List l => l
    | _ => raise Useful.Error "destList";

val isList = can destList;

(* Nil list objects *)

val nilList = List [];

fun mkNilList () = nilList;

fun isNilList ob =
    case ob of
      List [] => true
    | _ => false;

(* Cons list objects *)

fun mkConsList (h,t) =
    case h of
      Call _ => raise Useful.Error "Object.mkConsList: cannot cons Call object"
    | _ =>
      case t of
        List l => List (h :: l)
      | _ => raise Useful.Error "Object.mkConsList";

fun destConsList ob =
    case ob of
      List (h :: t) => (h, List t)
    | _ => raise Useful.Error "Object.destConsList";

val isConsList = can destConsList;

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
    | _ => raise Useful.Error "Object.destPair";

val isPair = can destPair;

(* Triple objects *)

fun mkTriple (x,y,z) = List [x,y,z];

fun destTriple ob =
    case ob of
      List [x,y,z] => (x,y,z)
    | _ => raise Useful.Error "Object.destTriple";

val isTriple = can destTriple;

(* Type operator objects *)

fun mkOtypeOp (ot,tys) =
    let
      val tys = destOtypes tys
    in
      Otype (Type.mkOp (ot,tys))
    end;

fun destOtypeOp (Otype ty) =
    let
      val (ot,l) = Type.destOp ty
    in
      (ot, mkOtypes l)
    end
  | destOtypeOp _ = raise Useful.Error "Object.destOtypeOp";

val isOtypeOp = can destOtypeOp;

(* Type objects *)

fun destType ob =
    case ob of
      Type ty => ty
    | _ => raise Useful.Error "Object.destType";

val isType = can destType;

(* Type list objects *)

fun mkTypes tys = List (map Type tys);

fun destTypes ob = map destType (destList ob);

val isTypes = can destTypes;

(* Type variable type objects *)

fun mkVarType ob =
    case ob of
      Name n => Type (Type.mkVar n)
    | _ => raise Useful.Error "Object.mkVarType";

fun destVarType ob =
    case ob of
      Type ty => Name (Type.destVar ty)
    | _ => raise Useful.Error "Object.destVarType";

val isVarType = can destVarType;

(* Type operator type objects *)

fun mkOtypeOp (ot,tys) =
    let
      val tys = destOtypes tys
    in
      Otype (Type.mkOp (ot,tys))
    end;

fun destOtypeOp (Otype ty) =
    let
      val (ot,l) = Type.destOp ty
    in
      (ot, mkOtypes l)
    end
  | destOtypeOp _ = raise Useful.Error "Object.destOtypeOp";

val isOtypeOp = can destOtypeOp;

(* Term variable objects *)

fun mkOvar v =
    let
      val (n,ty) = Var.dest v
    in
      mkOpair (Oname n, Otype ty)
    end;

fun destOvar var =
    let
      val (n,ty) = destOpair var
    in
      Var.mk (destOname n, destOtype ty)
    end;

val isOvar = can destOvar;

(* Term objects *)

fun destOterm (Oterm tm) = tm
  | destOterm _ = raise Useful.Error "destOterm";

val isOterm = can destOterm;

(* Term list objects *)

fun mkOterms tys = Olist (map Oterm tys);

fun destOterms obj = map destOterm (destOlist obj);

val isOterms = can destOterms;

(* Term variable term objects *)

fun mkOtermVar (Oname n, Otype ty) = Oterm (Term.mkVar (Var.mk (n,ty)))
  | mkOtermVar _ = raise Useful.Error "Object.mkOtermVar";

fun destOtermVar (Oterm t) =
    let
      val v = Term.destVar t
      val (n,ty) = Var.dest v
    in
      (Oname n, Otype ty)
    end
  | destOtermVar _ = raise Useful.Error "Object.destOtermVar";

val isOtermVar = can destOtermVar;

(* Constant term objects *)

fun mkOtermConst (c,oty) =
    case oty of
      Otype ty => Oterm (Term.mkConst (c,ty))
    | _ => raise Useful.Error "Object.mkOtermConst";

fun destOtermConst (Oterm t) =
    let
      val (c,ty) = Term.destConst t
    in
      (c, Otype ty)
    end
  | destOtermConst _ = raise Useful.Error "Object.destOtermConst";

val isOtermConst = can destOtermConst;

(* Function application term objects *)

fun mkOtermApp (Oterm f, Oterm a) = Oterm (Term.mkApp (f,a))
  | mkOtermApp _ = raise Useful.Error "Object.mkOtermApp";

fun destOtermApp (Oterm t) =
    let
      val (f,a) = Term.destApp t
    in
      (Oterm f, Oterm a)
    end
  | destOtermApp _ = raise Useful.Error "Object.destOtermApp";

val isOtermApp = can destOtermApp;

(* Lambda abstraction term objects *)

fun mkOtermAbs (Oterm v, Oterm b) = Oterm (Term.mkAbs (Term.destVar v, b))
  | mkOtermAbs _ = raise Useful.Error "Object.mkOtermAbs";

fun destOtermAbs (Oterm t) =
    let
      val (v,b) = Term.destAbs t
    in
      (Oterm (Term.mkVar v), Oterm b)
    end
  | destOtermAbs _ = raise Useful.Error "Object.destOtermAbs";

val isOtermAbs = can destOtermAbs;

(* Sequent objects *)

fun mkOseq seq =
    let
      val Sequent.Sequent {hyp = h, concl = c} = seq
      val h = TermAlphaSet.toList h
    in
      (mkOterms h, Oterm c)
    end;

fun destOseq (h,c) =
    Sequent.Sequent
      {hyp = TermAlphaSet.fromList (destOterms h),
       concl = destOterm c};

val isOseq = can destOseq;

(* Theorem objects *)

fun destOthm (Othm th) = th
  | destOthm _ = raise Useful.Error "destOthm";

val isOthm = can destOthm;

(* Function call objects *)

fun destOcall (Ocall n) = n
  | destOcall _ = raise Useful.Error "destOcall";

val isOcall = can destOcall;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

fun compare ob1_ob2 =
    if Portable.pointerEqual ob1_ob2 then EQUAL
    else
      case ob1_ob2 of
        (Oerror,Oerror) => EQUAL
      | (Oerror,_) => LESS
      | (_,Oerror) => GREATER
      | (Oint n1, Oint n2) => Int.compare (n1,n2)
      | (Oint _, _) => LESS
      | (_, Oint _) => GREATER
      | (Oname n1, Oname n2) => Name.compare (n1,n2)
      | (Oname _, _) => LESS
      | (_, Oname _) => GREATER
      | (OtypeOp ot1, OtypeOp ot2) => TypeOp.compare (ot1,ot2)
      | (OtypeOp _, _) => LESS
      | (_, OtypeOp _) => GREATER
      | (Otype ty1, Otype ty2) => Type.compare (ty1,ty2)
      | (Otype _, _) => LESS
      | (_, Otype _) => GREATER
      | (Oconst c1, Oconst c2) => Const.compare (c1,c2)
      | (Oconst _, _) => LESS
      | (_, Oconst _) => GREATER
      | (Ovar v1, Ovar v2) => Var.compare (v1,v2)
      | (Ovar _, _) => LESS
      | (_, Ovar _) => GREATER
      | (Oterm tm1, Oterm tm2) => Term.compare (tm1,tm2)
      | (Oterm _, _) => LESS
      | (_, Oterm _) => GREATER
      | (Othm th1, Othm th2) => Thm.dealphaCompare (th1,th2)
      | (Othm _, _) => LESS
      | (_, Othm _) => GREATER
      | (Olist l1, Olist l2) => lexCompare compare (l1,l2)
      | (Olist _, _) => LESS
      | (_, Olist _) => GREATER
      | (Ocall n1, Ocall n2) => Name.compare (n1,n2);

(* ------------------------------------------------------------------------- *)
(* Extracting the theorems stored in an object.                              *)
(* ------------------------------------------------------------------------- *)

val thms =
    let
      fun f acc [] = acc
        | f acc (Othm th :: rest) = f (th :: acc) rest
        | f acc (Olist l :: rest) = f acc (l @ rest)
        | f acc (_ :: rest) = f acc rest
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
        Oerror => symbolAddList sym obs
      | Oint _ => symbolAddList sym obs
      | Oname _ => symbolAddList sym obs
      | OtypeOp ot => Symbol.addTypeOp sym ot
      | Otype ty =>
        let
          val sym = Symbol.addType sym ty
        in
          symbolAddList sym obs
        end
      | Oconst c => Symbol.addConst sym c
      | Ovar v => Symbol.addVar sym v
      | Oterm tm =>
        let
          val sym = Symbol.addTerm sym tm
        in
          symbolAddList sym obs
        end
      | Othm th =>
        let
          val sym = Symbol.addSequent sym (Thm.sequent th)
        in
          symbolAddList sym obs
        end
      | Olist l =>
        let
          val obs = l @ obs
        in
          symbolAddList sym obs
        end
      | Ocall _ => symbolAddList sym obs;

fun symbolAdd sym ob = symbolAddList sym [ob];

val symbol = symbolAdd Symbol.empty;

(* ------------------------------------------------------------------------- *)
(* Breaking down objects into commands.                                      *)
(* ------------------------------------------------------------------------- *)

fun toCommand ob =
    case ob of
      Oerror => (Command.Error,[])
    | Oint i => (Command.Num i, [])
    | Oname n => (Command.Name n, [])
    | OtypeOp ot => (Command.TypeOp, [Oname (TypeOp.name ot)])
    | Otype ty =>
      (case Type.dest ty of
         TypeTerm.VarTy' n => (Command.VarType, [Oname n])
       | TypeTerm.OpTy' (ot,tys) => (Command.OpType, [OtypeOp ot, mkOtypes tys]))
    | Oconst c => (Command.Const, [Oname (Const.name c)])
    | Ovar (TypeTerm.Var (n,ty)) => (Command.Var, [Oname n, Otype ty])
    | Oterm tm =>
      (case Term.dest tm of
         TypeTerm.Const' (c,ty) => (Command.ConstTerm, [Oconst c, Otype ty])
       | TypeTerm.Var' v => (Command.Var, [Ovar v])
       | TypeTerm.App' (f,a) => (Command.AppTerm, [Oterm f, Oterm a])
       | TypeTerm.Abs' (v,b) => (Command.AbsTerm, [Ovar v, Oterm b]))
    | Othm th =>
      let
        val (hyp,concl) = mkOseq (Thm.sequent th)
      in
        (Command.Thm,[hyp,concl])
      end
    | Olist l =>
      (case l of
         [] => (Command.Nil,[])
       | h :: t => (Command.Cons, [h, Olist t]))
    | Ocall _ => raise Bug "Object.toCommand: Ocall";

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp ob =
    case ob of
      Oerror => Print.ppString "ERROR"
    | Oint n => Print.ppInt n
    | Oname s => Name.ppQuoted s
    | OtypeOp ot => TypeOp.pp ot
    | Otype ty => Type.pp ty
    | Oconst c => Const.pp c
    | Ovar v => Var.pp v
    | Oterm tm => Term.pp tm
    | Othm th => Thm.pp th
    | Olist l => Print.ppList pp l
    | Ocall f => Print.ppBracket "<" ">" Name.pp f;

end

structure ObjectOrdered =
struct type t = Object.object val compare = Object.compare end

structure ObjectSet = ElementSet (ObjectOrdered)

structure ObjectMap = KeyMap (ObjectOrdered)
