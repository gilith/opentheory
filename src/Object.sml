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

fun destOthm (Othm th) = th
  | destOthm _ = raise Error "destOthm";
val isOthm = can destOthm;

fun destOcall (Ocall n) = n
  | destOcall _ = raise Error "destOcall";
val isOcall = can destOcall;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun iterCompare NONE NONE = EQUAL
    | iterCompare NONE (SOME _) = LESS
    | iterCompare (SOME _) NONE = GREATER
    | iterCompare (SOME i1) (SOME i2) =
      let
        val k1 = TermAlphaSet.readIterator i1
        and k2 = TermAlphaSet.readIterator i2
      in
        keyIterCompare k1 k2 i1 i2
      end

  and keyIterCompare k1 k2 i1 i2 =
      case Term.compare (k1,k2) of
        LESS => LESS
      | EQUAL =>
        let
          val i1 = TermAlphaSet.advanceIterator i1
          and i2 = TermAlphaSet.advanceIterator i2
        in
          iterCompare i1 i2
        end
      | GREATER => GREATER;
in
  fun compareUnalphaTermAlphaSet (s1,s2) =
      case Int.compare (TermAlphaSet.size s1, TermAlphaSet.size s2) of
        LESS => LESS
      | EQUAL =>
        let
          val i1 = TermAlphaSet.mkIterator s1
          and i2 = TermAlphaSet.mkIterator s2
        in
          iterCompare i1 i2
        end
      | GREATER => GREATER;
end;

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
      | (Othm th1, Othm th2) =>
        let
          val Sequent.Sequent {hyp = h1, concl = c1} = Thm.sequent th1
          and Sequent.Sequent {hyp = h2, concl = c2} = Thm.sequent th2
        in
          case Term.compare (c1,c2) of
            LESS => LESS
          | EQUAL => compareUnalphaTermAlphaSet (h1,h2)
          | GREATER => GREATER
        end
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
      fn obj => f [] [obj]
    end;

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
        val Sequent.Sequent {hyp,concl} = Thm.sequent th
        val hyp = TermAlphaSet.toList hyp
      in
        (Command.Thm, [mkOterms hyp, Oterm concl])
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
