(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Syntax :> Syntax =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Operators.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Nullary operators *)

fun mkNullaryOp c ty = Term.mkConst (c,ty);

fun destNullaryOp c tm =
    let
      val (c',ty) = Term.destConst tm
      val _ = Const.equal c c' orelse raise Error "Syntax.destNullaryOp"
    in
      ty
    end;

(* Unary operators *)

fun mkUnaryOp c (a,ty) =
    let
      val t = mkNullaryOp c (Type.mkFun (Term.typeOf a, ty))
    in
      Term.mkApp (t,a)
    end;

fun destUnaryOp c tm =
    let
      val (t,a) = Term.destApp tm
      val tty = destNullaryOp c t
      val ty = Type.rangeFun tty
    in
      (a,ty)
    end;

fun listMkUnaryOp c ty =
    let
      fun mk tm = mkUnaryOp c (tm,ty)
    in
      fn (k,tm) => funpow k mk tm
    end;

fun stripUnaryOp c =
    let
      fun strip k tm =
          case total (destUnaryOp c) tm of
            NONE => (k,tm)
          | SOME (tm,_) => strip (k + 1) tm
    in
      strip 0
    end;

(* Binary operators *)

fun mkBinaryOp c (a,b,ty) =
    let
      val t = mkUnaryOp c (a, Type.mkFun (Term.typeOf b, ty))
    in
      Term.mkApp (t,b)
    end;

fun destBinaryOp c tm =
    let
      val (t,b) = Term.destApp tm
      val (a,tty) = destUnaryOp c t
      val ty = Type.rangeFun tty
    in
      (a,b,ty)
    end;

fun listMkBinaryOp c ty =
    let
      fun mk (a,b) = mkBinaryOp c (a,b,ty)
    in
      fn (tm,tms) => List.foldl mk tm tms
    end;

fun stripBinaryOp c =
    let
      fun strip tms tm =
          case total (destBinaryOp c) tm of
            NONE => (tm,tms)
          | SOME (a,tm,_) => strip (a :: tms) tm
    in
      strip []
    end;

(* Quantifiers *)

fun mkQuant c (v,b,ty) =
    let
      val f = Term.mkAbs (v,b)
    in
      mkUnaryOp c (f,ty)
    end;

fun destQuant c tm =
    let
      val (f,ty) = destUnaryOp c tm
      val (v,b) = Term.destAbs f
    in
      (v,b,ty)
    end;

fun listMkQuant c ty =
    let
      fun mk (v,b) = mkQuant c (v,b,ty)
    in
      fn (vs,tm) => List.foldl mk tm vs
    end;

fun stripQuant c =
    let
      fun strip vs tm =
          case total (destQuant c) tm of
            NONE => (vs,tm)
          | SOME (v,b,_) => strip (v :: vs) b
    in
      strip []
    end;

(* ------------------------------------------------------------------------- *)
(* Booleans.                                                                 *)
(* ------------------------------------------------------------------------- *)

(* True *)

val stringTrue = "T";

val nameTrue = Name.mkGlobal stringTrue;

fun constTrue sym = Symbol.mkConst sym nameTrue;

fun termTrue sym = mkNullaryOp (constTrue sym) Type.bool;

fun isTrue sym = can (destNullaryOp (constTrue sym));

(* False *)

val stringFalse = "F";

val nameFalse = Name.mkGlobal stringFalse;

fun constFalse sym = Symbol.mkConst sym nameFalse;

fun termFalse sym = mkNullaryOp (constFalse sym) Type.bool;

fun isFalse sym = can (destNullaryOp (constFalse sym));

(* ------------------------------------------------------------------------- *)
(* Propositional connectives.                                                *)
(* ------------------------------------------------------------------------- *)

(* Negations *)

val stringNeg = "~";

val nameNeg = Name.mkGlobal stringNeg;

fun constNeg sym = Symbol.mkConst sym nameNeg;

val tyNeg = Type.mkFun (Type.bool,Type.bool);

fun termNeg sym = mkNullaryOp (constNeg sym) tyNeg;

fun mkNeg sym =
    let
      val c = constNeg sym
    in
      fn tm => mkUnaryOp c (tm,Type.bool)
    end;

fun destNeg sym =
    let
      val c = constNeg sym
    in
      fn tm =>
         let
           val (a,_) = destUnaryOp c tm
         in
           a
         end
    end;

fun isNeg sym = can (destNeg sym);

fun listMkNeg sym =
    let
      val c = constNeg sym
    in
      listMkUnaryOp c Type.bool
    end;

fun stripNeg sym =
    let
      val c = constNeg sym
    in
      stripUnaryOp c
    end;

(* Implications *)

val stringImp = "==>";

val nameImp = Name.mkGlobal stringImp;

fun constImp sym = Symbol.mkConst sym nameImp;

val tyImp = Type.mkFun (Type.bool, Type.mkFun (Type.bool,Type.bool));

fun termImp sym = mkNullaryOp (constImp sym) tyImp;

fun mkImp sym =
    let
      val c = constImp sym
    in
      fn (a,b) => mkBinaryOp c (a,b,Type.bool)
    end;

fun destImp sym =
    let
      val c = constImp sym
    in
      fn tm =>
         let
           val (a,b,_) = destBinaryOp c tm
         in
           (a,b)
         end
    end;

fun isImp sym = can (destImp sym);

fun listMkImp sym (tms,tm) =
    let
      val c = constImp sym
    in
      listMkBinaryOp c Type.bool (tm, rev tms)
    end;

fun stripImp sym tm =
    let
      val c = constImp sym
      val (tm,tms) = stripBinaryOp c tm
    in
      (rev tms, tm)
    end;

(* Conjunctions *)

val stringConj = "/\\";

val nameConj = Name.mkGlobal stringConj;

fun constConj sym = Symbol.mkConst sym nameConj;

val tyConj = Type.mkFun (Type.bool, Type.mkFun (Type.bool,Type.bool));

fun termConj sym = mkNullaryOp (constConj sym) tyConj;

fun mkConj sym =
    let
      val c = constConj sym
    in
      fn (a,b) => mkBinaryOp c (a,b,Type.bool)
    end;

fun destConj sym =
    let
      val c = constConj sym
    in
      fn tm =>
         let
           val (a,b,_) = destBinaryOp c tm
         in
           (a,b)
         end
    end;

fun isConj sym = can (destConj sym);

fun listMkConj sym tms =
    case rev tms of
      [] => termTrue sym
    | tm :: tms =>
      let
        val c = constConj sym
      in
        listMkBinaryOp c Type.bool (tm,tms)
      end;

fun stripConj sym tm =
    if isTrue sym tm then []
    else
      let
        val c = constConj sym
        val (tm,tms) = stripBinaryOp c tm
      in
        rev (tm :: tms)
      end;

(* Disjunctions *)

val stringDisj = "\\/";

val nameDisj = Name.mkGlobal stringDisj;

fun constDisj sym = Symbol.mkConst sym nameDisj;

val tyDisj = Type.mkFun (Type.bool, Type.mkFun (Type.bool,Type.bool));

fun termDisj sym = mkNullaryOp (constDisj sym) tyDisj;

fun mkDisj sym =
    let
      val c = constDisj sym
    in
      fn (a,b) => mkBinaryOp c (a,b,Type.bool)
    end;

fun destDisj sym =
    let
      val c = constDisj sym
    in
      fn tm =>
         let
           val (a,b,_) = destBinaryOp c tm
         in
           (a,b)
         end
    end;

fun isDisj sym = can (destDisj sym);

fun listMkDisj sym tms =
    case rev tms of
      [] => termTrue sym
    | tm :: tms =>
      let
        val c = constDisj sym
      in
        listMkBinaryOp c Type.bool (tm,tms)
      end;

fun stripDisj sym tm =
    if isTrue sym tm then []
    else
      let
        val c = constDisj sym
        val (tm,tms) = stripBinaryOp c tm
      in
        rev (tm :: tms)
      end;

(* ------------------------------------------------------------------------- *)
(* Quantifiers.                                                              *)
(* ------------------------------------------------------------------------- *)

(* Universal quantifiers *)

val stringForall = "!";

val nameForall = Name.mkGlobal stringForall;

fun constForall sym = Symbol.mkConst sym nameForall;

fun mkForall sym =
    let
      val c = constForall sym
    in
      fn (v,b) => mkQuant c (v,b,Type.bool)
    end;

fun destForall sym =
    let
      val c = constForall sym
    in
      fn tm =>
         let
           val (v,b,_) = destQuant c tm
         in
           (v,b)
         end
    end;

fun isForall sym = can (destForall sym);

fun listMkForall sym =
    let
      val c = constForall sym
    in
      fn (vs,tm) => listMkQuant c Type.bool (rev vs, tm)
    end;

fun stripForall sym =
    let
      val c = constForall sym
    in
      fn tm =>
         let
           val (vs,b) = stripQuant c tm
         in
           (rev vs, b)
         end
    end;

(* Existential quantifiers *)

val stringExists = "?";

val nameExists = Name.mkGlobal stringExists;

fun constExists sym = Symbol.mkConst sym nameExists;

fun mkExists sym =
    let
      val c = constExists sym
    in
      fn (v,b) => mkQuant c (v,b,Type.bool)
    end;

fun destExists sym =
    let
      val c = constExists sym
    in
      fn tm =>
         let
           val (v,b,_) = destQuant c tm
         in
           (v,b)
         end
    end;

fun isExists sym = can (destExists sym);

fun listMkExists sym =
    let
      val c = constExists sym
    in
      fn (vs,tm) => listMkQuant c Type.bool (rev vs, tm)
    end;

fun stripExists sym =
    let
      val c = constExists sym
    in
      fn tm =>
         let
           val (vs,b) = stripQuant c tm
         in
           (rev vs, b)
         end
    end;

(* Unique existential quantifiers *)

val stringExistsUnique = "?!";

val nameExistsUnique = Name.mkGlobal stringExistsUnique;

fun constExistsUnique sym = Symbol.mkConst sym nameExistsUnique;

fun mkExistsUnique sym =
    let
      val c = constExistsUnique sym
    in
      fn (v,b) => mkQuant c (v,b,Type.bool)
    end;

fun destExistsUnique sym =
    let
      val c = constExistsUnique sym
    in
      fn tm =>
         let
           val (v,b,_) = destQuant c tm
         in
           (v,b)
         end
    end;

fun isExistsUnique sym = can (destExistsUnique sym);

fun listMkExistsUnique sym =
    let
      val c = constExistsUnique sym
    in
      fn (vs,tm) => listMkQuant c Type.bool (rev vs, tm)
    end;

fun stripExistsUnique sym =
    let
      val c = constExistsUnique sym
    in
      fn tm =>
         let
           val (vs,b) = stripQuant c tm
         in
           (rev vs, b)
         end
    end;

(* Hilbert's indefinite choice operator (epsilon) *)

val stringSelect = "?!";

val nameSelect = Name.mkGlobal stringSelect;

fun constSelect sym = Symbol.mkConst sym nameSelect;

fun mkSelect sym =
    let
      val c = constSelect sym
    in
      fn (v,b) => mkQuant c (v, b, Var.typeOf v)
    end;

fun destSelect sym =
    let
      val c = constSelect sym
    in
      fn tm =>
         let
           val (v,b,_) = destQuant c tm
         in
           (v,b)
         end
    end;

fun isSelect sym = can (destSelect sym);

(* ------------------------------------------------------------------------- *)
(* The type of individuals.                                                  *)
(* ------------------------------------------------------------------------- *)

val stringInd = "ind";

val nameInd = Name.mkGlobal stringInd;

fun typeOpInd sym = Symbol.mkTypeOp sym nameInd;

fun tyInd sym = Type.mkOp (typeOpInd sym, []);

end
