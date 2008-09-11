(* ========================================================================= *)
(* OPENTHEORY OBJECTS                                                        *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Object :> Object =
struct

open Useful Syntax;

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory objects.                                             *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Oerror
  | Onum of int
  | Oname of name
  | Olist of object list
  | Otype of ty
  | Oterm of term
  | Othm of thm
  | Ocall of name;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun destOerror Oerror = ()
  | destOerror _ = raise Error "destOerror";
val isOerror = can destOerror;

fun destOnum (Onum n) = n
  | destOnum _ = raise Error "destOnum";
val isOnum = can destOnum;

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

fun mkOtypeOp (Oname n, tys) = Otype (Type.mkOp (n, destOtypes tys))
  | mkOtypeOp _ = raise Error "Object.mkOtypeOp";
fun destOtypeOp (Otype ty) =
    let
      val (n,l) = Type.destOp ty
    in
      (Oname n, mkOtypes l)
    end
  | destOtypeOp _ = raise Error "Object.destOtypeOp";
val isOtypeOp = can destOtypeOp;

fun mkOvar (n,ty) = mkOpair (Oname n, Otype ty);
fun destOvar var =
    let
      val (n,ty) = destOpair var
    in
      (destOname n, destOtype ty)
    end;
val isOvar = can destOvar;

fun destOterm (Oterm tm) = tm
  | destOterm _ = raise Error "destOterm";
val isOterm = can destOterm;

fun mkOterms tys = Olist (map Oterm tys);
fun destOterms obj = map destOterm (destOlist obj);
val isOterms = can destOterms;

fun mkOtermVar (Oname n, Otype ty) = Oterm (Term.mkVar (n,ty))
  | mkOtermVar _ = raise Error "Object.mkOtermVar";
fun destOtermVar (Oterm t) =
    let
      val (n,ty) = Term.destVar t
    in
      (Oname n, Otype ty)
    end
  | destOtermVar _ = raise Error "Object.destOtermVar";
val isOtermVar = can destOtermVar;

fun mkOtermConst (Oname n, Otype ty) = Oterm (Term.mkConst (n,ty))
  | mkOtermConst _ = raise Error "Object.mkOtermConst";
fun destOtermConst (Oterm t) =
    let
      val (n,ty) = Term.destConst t
    in
      (Oname n, Otype ty)
    end
  | destOtermConst _ = raise Error "Object.destOtermConst";
val isOtermConst = can destOtermConst;

fun mkOtermComb (Oterm f, Oterm a) = Oterm (Term.mkComb (f,a))
  | mkOtermComb _ = raise Error "Object.mkOtermComb";
fun destOtermComb (Oterm t) =
    let
      val (f,a) = Term.destComb t
    in
      (Oterm f, Oterm a)
    end
  | destOtermComb _ = raise Error "Object.destOtermComb";
val isOtermComb = can destOtermComb;

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
      | (Onum n1, Onum n2) => Int.compare (n1,n2)
      | (Onum _, _) => LESS
      | (_, Onum _) => GREATER
      | (Oname n1, Oname n2) => Name.compare (n1,n2)
      | (Oname _, _) => LESS
      | (_, Oname _) => GREATER
      | (Otype ty1, Otype ty2) => Type.compare (ty1,ty2)
      | (Otype _, _) => LESS
      | (_, Otype _) => GREATER
      | (Oterm tm1, Oterm tm2) => Term.compare (tm1,tm2)
      | (Oterm _, _) => LESS
      | (_, Oterm _) => GREATER
      | (Othm th1, Othm th2) => Thm.compare (th1,th2)
      | (Othm _, _) => LESS
      | (_, Othm _) => GREATER
      | (Olist l1, Olist l2) => lexCompare compare (l1,l2)
      | (Olist _, _) => LESS
      | (_, Olist _) => GREATER
      | (Ocall n1, Ocall n2) => Name.compare (n1,n2);

(* ------------------------------------------------------------------------- *)
(* Lifting interpretations to Oname objects.                                 *)
(* ------------------------------------------------------------------------- *)

fun interpretType int obj =
    case obj of
      Oname n => Oname (Interpretation.interpretType int n)
    | _ => raise Error "Object.interpretType: not an Oname object";

fun interpretConst int obj =
    case obj of
      Oname n => Oname (Interpretation.interpretConst int n)
    | _ => raise Error "Object.interpretConst: not an Oname object";

fun interpretRule int obj =
    case obj of
      Oname n => Oname (Interpretation.interpretRule int n)
    | _ => raise Error "Object.interpretRule: not an Oname object";

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
      fn obj => f [] [obj]
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp p ob =
    case ob of
      Oerror => Parser.ppString p "ERROR"
    | Onum n => Parser.ppInt p n
    | Oname s => Name.ppQuoted p s
    | Otype ty => ppType p ty
    | Oterm tm => ppTerm p tm
    | Othm th => ppThm p th
    | Olist l => Parser.ppList pp p l
    | Ocall f => Parser.ppBracket "<" ">" Name.pp p f;

end

structure ObjectOrdered =
struct type t = Object.object val compare = Object.compare end

structure ObjectSet = ElementSet (ObjectOrdered)

structure ObjectMap = KeyMap (ObjectOrdered)
