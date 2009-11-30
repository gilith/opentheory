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
    Oerror
  | Oint of int
  | Oname of Name.name
  | Olist of object list
  | Otype of Type.ty
  | Oterm of Term.term
  | Othm of Thm.thm
  | Ocall of Name.name;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun destOerror Oerror = ()
  | destOerror _ = raise Error "destOerror";
val isOerror = can destOerror;

fun destOint (Oint n) = n
  | destOint _ = raise Error "destOint";
val isOint = can destOint;

fun destOname (Oname n) = n
  | destOname _ = raise Error "destOname";
val isOname = can destOname;

fun destOlist (Olist l) = l
  | destOlist _ = raise Error "destOlist";
val isOlist = can destOlist;

val onil = Olist [];
fun mkOnil () = onil;
fun isOnil (Olist []) = true
  | isOnil _ = false;

fun mkOcons (Ocall _, _) = raise Error "Object.mkOcons: cannot cons Ocall"
  | mkOcons (h, Olist t) = Olist (h :: t)
  | mkOcons (_, _) = raise Error "Object.mkOcons";
fun destOcons (Olist (h :: t)) = (h, Olist t)
  | destOcons _ = raise Error "Object.destOcons";
val isOcons = can destOcons;

val ounit = Olist [];
fun mkOunit () = ounit;
fun isOunit (Olist []) = true
  | isOunit _ = false;

fun mkOpair (x,y) = Olist [x,y];
fun destOpair (Olist [x,y]) = (x,y)
  | destOpair _ = raise Error "destOpair";
val isOpair = can destOpair;

fun mkOtriple (x,y,z) = Olist [x,y,z];
fun destOtriple (Olist [x,y,z]) = (x,y,z)
  | destOtriple _ = raise Error "destOtriple";
val isOtriple = can destOtriple;

fun destOtype (Otype ty) = ty
  | destOtype _ = raise Error "Object.destOtype";
val isOtype = can destOtype;

fun mkOtypes tys = Olist (map Otype tys);
fun destOtypes obj = map destOtype (destOlist obj);
val isOtypes = can destOtypes;

fun mkOtypeVar (Oname n) = Otype (Type.mkVar n)
  | mkOtypeVar _ = raise Error "Object.mkOtypeVar";
fun destOtypeVar (Otype ty) = Oname (Type.destVar ty)
  | destOtypeVar _ = raise Error "Object.destOtypeVar";
val isOtypeVar = can destOtypeVar;

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
  | destOtypeOp _ = raise Error "Object.destOtypeOp";

val isOtypeOp = can destOtypeOp;

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

fun destOterm (Oterm tm) = tm
  | destOterm _ = raise Error "destOterm";
val isOterm = can destOterm;

fun mkOterms tys = Olist (map Oterm tys);
fun destOterms obj = map destOterm (destOlist obj);
val isOterms = can destOterms;

fun mkOtermVar (Oname n, Otype ty) = Oterm (Term.mkVar (Var.mk (n,ty)))
  | mkOtermVar _ = raise Error "Object.mkOtermVar";
fun destOtermVar (Oterm t) =
    let
      val v = Term.destVar t
      val (n,ty) = Var.dest v
    in
      (Oname n, Otype ty)
    end
  | destOtermVar _ = raise Error "Object.destOtermVar";
val isOtermVar = can destOtermVar;

fun mkOtermConst (c,oty) =
    case oty of
      Otype ty => Oterm (Term.mkConst (c,ty))
    | _ => raise Error "Object.mkOtermConst";
fun destOtermConst (Oterm t) =
    let
      val (c,ty) = Term.destConst t
    in
      (c, Otype ty)
    end
  | destOtermConst _ = raise Error "Object.destOtermConst";
val isOtermConst = can destOtermConst;

fun mkOtermApp (Oterm f, Oterm a) = Oterm (Term.mkApp (f,a))
  | mkOtermApp _ = raise Error "Object.mkOtermApp";
fun destOtermApp (Oterm t) =
    let
      val (f,a) = Term.destApp t
    in
      (Oterm f, Oterm a)
    end
  | destOtermApp _ = raise Error "Object.destOtermApp";
val isOtermApp = can destOtermApp;

fun mkOtermAbs (Oterm v, Oterm b) = Oterm (Term.mkAbs (Term.destVar v, b))
  | mkOtermAbs _ = raise Error "Object.mkOtermAbs";
fun destOtermAbs (Oterm t) =
    let
      val (v,b) = Term.destAbs t
    in
      (Oterm (Term.mkVar v), Oterm b)
    end
  | destOtermAbs _ = raise Error "Object.destOtermAbs";
val isOtermAbs = can destOtermAbs;

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

fun destOthm (Othm th) = th
  | destOthm _ = raise Error "destOthm";
val isOthm = can destOthm;

fun destOcall (Ocall n) = n
  | destOcall _ = raise Error "destOcall";
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
      | (Otype ty1, Otype ty2) => Type.compare (ty1,ty2)
      | (Otype _, _) => LESS
      | (_, Otype _) => GREATER
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
      | Olist l =>
        let
          val obs = l @ obs
        in
          symbolAddList sym obs
        end
      | Otype ty =>
        let
          val sym = Symbol.addType sym ty
        in
          symbolAddList sym obs
        end
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
    | Olist l =>
      (case l of
         [] => (Command.Nil,[])
       | h :: t => (Command.Cons, [h, Olist t]))
    | Otype ty =>
      (case Type.dest ty of
         TypeTerm.VarTy' n => (Command.TypeVar, [Oname n])
       | TypeTerm.OpTy' (ot,tys) =>
         (Command.TypeOp, [Oname (TypeOp.name ot), mkOtypes tys]))
    | Oterm tm =>
      (case Term.dest tm of
         TypeTerm.Const' (c,ty) =>
         (Command.Const, [Oname (Const.name c), Otype ty])
       | TypeTerm.Var' (TypeTerm.Var (n,ty)) =>
         (Command.Var, [Oname n, Otype ty])
       | TypeTerm.App' (f,a) =>
         (Command.App, [Oterm f, Oterm a])
       | TypeTerm.Abs' (v,b) =>
         (Command.Abs, [Oterm (Term.mkVar v), Oterm b]))
    | Othm th =>
      let
        val (hyp,concl) = mkOseq (Thm.sequent th)
      in
        (Command.Thm,[hyp,concl])
      end
    | Ocall _ => raise Bug "Object.toCommand: Ocall";

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp ob =
    case ob of
      Oerror => Print.ppString "ERROR"
    | Oint n => Print.ppInt n
    | Oname s => Name.ppQuoted s
    | Otype ty => Type.pp ty
    | Oterm tm => Term.pp tm
    | Othm th => Thm.pp th
    | Olist l => Print.ppList pp l
    | Ocall f => Print.ppBracket "<" ">" Name.pp f;

end

structure ObjectOrdered =
struct type t = Object.object val compare = Object.compare end

structure ObjectSet = ElementSet (ObjectOrdered)

structure ObjectMap = KeyMap (ObjectOrdered)
