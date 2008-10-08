(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Syntax :> Syntax =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Primitive.                                                                *)
(* ------------------------------------------------------------------------- *)

type name = Name.name;
type ty = Type.ty;
type var = Var.var;
type term = Term.term;
type sequent = Sequent.sequent;
type thm = Thm.thm;

(* Type variables *)

val mkTypeVar = Type.mkVar;
val destTypeVar = Type.destVar;
val isTypeVar = Type.isVar;
val equalTypeVar = Type.equalVar;

val alphaTy = Type.alphaTy;

(* Type operators *)

val mkTypeOp = Type.mkOp;
val destTypeOp = Type.destOp;
val isTypeOp = Type.isOp;

(* The type of booleans *)

val boolTy = Type.boolTy;

(* Function types *)

val mkFun = Type.mkFun;
val destFun = Type.destFun;
val isFun = Type.isFun;

fun listMkFun ([],ty) = ty
  | listMkFun (x :: xs, ty) = mkFun (x, listMkFun (xs,ty));

local
  fun strip acc ty =
    if not (isFun ty) then (rev acc, ty)
    else let val (x,ty) = destFun ty in strip (x :: acc) ty end;
in
  val stripFun = strip [];
end;

(* Constants *)

val mkConst = Term.mkConst;
val destConst = Term.destConst;
val isConst = Term.isConst;

(* Variables *)

val mkVar = Term.mkVar;
val destVar = Term.destVar;
val isVar = Term.isVar;
val equalVar = Term.equalVar;

(* Function applications *)

val mkComb = Term.mkComb;
val destComb = Term.destComb;
val isComb = Term.isComb;

val rator = fst o destComb;

val rand = snd o destComb;

val land = rand o rator;

fun listMkComb (tm,[]) = tm
  | listMkComb (tm, x :: xs) = listMkComb (mkComb (tm,x), xs);

local
  fun strip acc tm =
    if not (isComb tm) then (tm,acc)
    else let val (tm,x) = destComb tm in strip (x :: acc) tm end;
in
  val stripComb = strip [];
end;

(* Lambda abstractions *)

val mkAbs = Term.mkAbs;
val destAbs = Term.destAbs;
val isAbs = Term.isAbs;

fun listMkAbs ([],tm) = tm
  | listMkAbs (v :: vs, tm) = mkAbs (v, listMkAbs (vs,tm));

local
  fun strip acc tm =
    if not (isAbs tm) then (rev acc, tm)
    else let val (v,tm) = destAbs tm in strip (v :: acc) tm end;
in
  val stripAbs = strip [];
end;

(* Equality *)

val eqTy = Term.eqTy;
val eqTm = Term.eqTm;
val mkEq = Term.mkEq;
val destEq = Term.destEq;
val isEq = Term.isEq;
val lhs = fst o destEq;
val rhs = snd o destEq;

(* Theorems *)

fun axioms th = case Thm.dest th of Thm.Thm {axioms,...} => axioms;

fun sequent th = case Thm.dest th of Thm.Thm {sequent,...} => sequent;

fun hyp th = case Thm.dest th of Thm.Thm {sequent = {hyp, ...}, ...} => hyp;

fun concl th =
    case Thm.dest th of Thm.Thm {sequent = {concl, ...}, ...} => concl;

(* ------------------------------------------------------------------------- *)
(* Operators.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Unary operators *)

fun mkUnop n (ty,a) =
    let
      val c = mkConst (n,ty)
    in
      mkComb (c,a)
    end;

fun destUnop n tm =
    let
      val (c,a) = destComb tm
      val (n',ty) = destConst c
      val _ = Name.equal n n' orelse raise Error "Syntax.destUnop"
    in
      (ty,a)
    end;

fun isUnop n = can (destUnop n);

(* Binary operators *)

fun mkBinop n (ty,a,b) =
    let
      val c = mkConst (n,ty)
      val t = mkComb (c,a)
    in
      mkComb (t,b)
    end;

fun destBinop n tm =
    let
      val (t,b) = destComb tm
      val (c,a) = destComb t
      val (n',ty) = destConst c
      val _ = Name.equal n n' orelse raise Error "Syntax.destBinop"
    in
      (ty,a,b)
    end;

fun isBinop n = can (destBinop n);

(* ------------------------------------------------------------------------- *)
(* Boolean.                                                                  *)
(* ------------------------------------------------------------------------- *)

(* Negations *)

val negString = "~";

val negName = Name.mkGlobal negString;

fun mkNeg tm = mkUnop negName (boolTy,tm);

fun destNeg tm =
    let
      val (_,a) = destUnop negName tm
    in
      a
    end;

val isNeg = can destNeg;

(* Implications *)

val impName = Name.mkGlobal "==>";

val mkImp =
    let
      val impTy = mkFun (boolTy, mkFun (boolTy,boolTy))
    in
      fn (a,b) => mkBinop impName (impTy,a,b)
    end;

fun destImp tm =
    let
      val (_,a,b) = destBinop impName tm
    in
      (a,b)
    end;

val isImp = can destImp;

(* Universal quantifiers *)

val forallName = Name.mkGlobal "!";

fun forallType a = mkFun (mkFun (a, boolTy), boolTy);

fun mkForall (v,b) =
    mkComb (mkConst (forallName, forallType (snd v)), mkAbs (v,b));

fun destForall tm =
    let
      val (c,t) = destComb tm
      val _ = Name.equal (fst (destConst c)) forallName orelse
              raise Error "destForall"
    in
      destAbs t
    end;

val isForall = can destForall;

fun listMkForall ([],tm) = tm
  | listMkForall (v :: vs, tm) = mkForall (v, listMkForall (vs,tm));

local
  fun strip acc tm =
    if not (isForall tm) then (rev acc, tm)
    else let val (v,tm) = destForall tm in strip (v :: acc) tm end;
in
  val stripForall = strip [];
end;

(* Existential quantifiers *)

val existsName = Name.mkGlobal "?";

fun existsType a = mkFun (mkFun (a, boolTy), boolTy);

fun mkExists (v,b) =
    mkComb (mkConst (existsName, existsType (snd v)), mkAbs (v,b));

fun destExists tm =
    let
      val (c,t) = destComb tm
      val _ = Name.equal (fst (destConst c)) existsName orelse
              raise Error "destExists"
    in
      destAbs t
    end;

val isExists = can destExists;

fun listMkExists ([],tm) = tm
  | listMkExists (v :: vs, tm) = mkExists (v, listMkExists (vs,tm));

local
  fun strip acc tm =
    if not (isExists tm) then (rev acc, tm)
    else let val (v,tm) = destExists tm in strip (v :: acc) tm end;
in
  val stripExists = strip [];
end;

(* Unique existential quantifiers *)

val existsUniqueName = Name.mkGlobal "?!";

fun existsUniqueType a = mkFun (mkFun (a, boolTy), boolTy);

fun mkExistsUnique (v,b) =
    mkComb (mkConst (existsUniqueName, existsUniqueType (snd v)),
             mkAbs (v,b));

fun destExistsUnique tm =
    let
      val (c,t) = destComb tm
      val _ = Name.equal (fst (destConst c)) existsUniqueName orelse
              raise Error "destExistsUnique"
    in
      destAbs t
    end;

val isExistsUnique = can destExistsUnique;

fun listMkExistsUnique ([],tm) = tm
  | listMkExistsUnique (v :: vs, tm) =
    mkExistsUnique (v, listMkExistsUnique (vs,tm));

local
  fun strip acc tm =
      if not (isExistsUnique tm) then (rev acc, tm)
      else let val (v,tm) = destExistsUnique tm in strip (v :: acc) tm end;
in
  val stripExistsUnique = strip [];
end;

(* Hilbert's indefinite choice operator (epsilon) *)

fun selectTy a = Type.mkFun (Type.mkFun (a, Type.boolTy), a);

val selectN = Name.mkGlobal "@";

val selectTm =
    let
      val ty = selectTy Type.alphaTy
    in
      mkConst (selectN,ty)
    end;

fun mkSelect (v_b as ((_,ty),_)) =
    mkComb (mkConst (selectN, selectTy ty), mkAbs v_b);

fun destSelect tm =
    let
      val (_,t) = destUnop selectN tm
    in
      destAbs t
    end;

val isSelect = can destSelect;

fun listMkSelect ([],tm) = tm
  | listMkSelect (v :: vs, tm) = mkSelect (v, listMkSelect (vs,tm));

local
  fun strip acc tm =
    if not (isSelect tm) then (rev acc, tm)
    else let val (v,tm) = destSelect tm in strip (v :: acc) tm end;
in
  val stripSelect = strip [];
end;

(* ------------------------------------------------------------------------- *)
(* The type of individuals.                                                  *)
(* ------------------------------------------------------------------------- *)

val indName = Name.mkGlobal "ind";

val indTy = mkTypeOp (indName,[]);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

(* Types *)

val typeInfixTokens =
    Print.Infixes
      [{token = " * ", precedence = 3, leftAssoc = false},
       {token = " + ", precedence = 2, leftAssoc = false},
       {token = " -> ", precedence = 1, leftAssoc = false}];

local
  val typeInfixStrings = Print.tokensInfixes typeInfixTokens;

  fun abbreviateTypeOp n =
      case Name.toString n of
        "fun" => "->"
      | s => s;

  val ppTypeVar = Name.pp;

  val ppTypeOp = Print.ppMap abbreviateTypeOp Print.ppString;

  fun destTypeInfix ty =
      let
        val (f,xs) = destTypeOp ty
        val f = abbreviateTypeOp f
        val _ = StringSet.member f typeInfixStrings orelse
                raise Error "destTypeInfix"
      in
        case xs of
          [a,b] => (f,a,b)
        | _ => raise Bug ("destTypeInfix: bad arity of type operator " ^ f)
      end;

  val isTypeInfix = can destTypeInfix;

  val typeInfixPrinter = Print.ppInfixes typeInfixTokens (total destTypeInfix);

  fun basic ty =
      if isTypeVar ty then ppTypeVar (destTypeVar ty)
      else if isTypeInfix ty then ppBtype ty
      else
        let
          val (f,xs) = destTypeOp ty
        in
          Print.blockProgram Print.Inconsistent 0
            [(case xs of
                [] => Print.skip
              | [x] => Print.sequence (basic ty) (Print.addBreak 1)
              | _ =>
                Print.sequence
                  (Print.ppBracket "(" ")" (Print.ppOpList "," ppType) xs)
                  (Print.addBreak 1)),
             ppTypeOp f]
        end

  and basicr (ty,_) = basic ty

  and ppBtype ty = Print.ppBracket "(" ")" ppType ty

  and ppTyper tyr = typeInfixPrinter basicr tyr

  and ppType ty = ppTyper (ty,false);
in
  val ppType = ppType;
end;

val typeToString = Print.toString ppType;

(* Terms *)

val showTypes = ref false;

val infixTokens =
    Print.Infixes
      [(* ML style *)
       {token = " / ", precedence = 7, leftAssoc = true},
       {token = " div ", precedence = 7, leftAssoc = true},
       {token = " mod ", precedence = 7, leftAssoc = true},
       {token = " * ", precedence = 7, leftAssoc = true},
       {token = " + ", precedence = 6, leftAssoc = true},
       {token = " - ", precedence = 6, leftAssoc = true},
       {token = " ^ ", precedence = 6, leftAssoc = true},
       {token = " @ ", precedence = 5, leftAssoc = false},
       {token = " :: ", precedence = 5, leftAssoc = false},
       {token = " = ", precedence = 4, leftAssoc = true},
       {token = " <> ", precedence = 4, leftAssoc = true},
       {token = " <= ", precedence = 4, leftAssoc = true},
       {token = " < ", precedence = 4, leftAssoc = true},
       {token = " >= ", precedence = 4, leftAssoc = true},
       {token = " > ", precedence = 4, leftAssoc = true},
       {token = " o ", precedence = 3, leftAssoc = true},
       (* HOL style *)
       {token = " /\\ ", precedence = ~1, leftAssoc = false},
       {token = " \\/ ", precedence = ~2, leftAssoc = false},
       {token = " ==> ", precedence = ~3, leftAssoc = false},
       {token = " <=> ", precedence = ~4, leftAssoc = false}];

val ppVar =
    let
      val pp1 = Print.ppBracket "(" ")" (Print.ppOp2 " :" Name.pp ppType)
      val pp2 = Print.ppMap fst Name.pp
    in
      fn v => (if !showTypes then pp1 else pp2) v
    end;

local
  val binders =
      [("\\",stripAbs),
       ("!",stripForall),
       ("?",stripExists),
       ("?!",stripExistsUnique),
       ("@",stripSelect)];

  val infixStrings = Print.tokensInfixes infixTokens;

  val binderStrings = StringSet.fromList (map fst binders);

  val specialStrings =
      StringSet.add (StringSet.union infixStrings binderStrings) negString;

  fun abbreviateConst n =
      case Name.toString n of
        s => s;

  fun specialString n = StringSet.member n specialStrings;

  val ppConst =
      let
        fun f (n,_) =
            let
              val n = abbreviateConst n
            in
              if specialString n then "(" ^ n ^ ")" else n
            end
      in
        Print.ppMap f Print.ppString
      end;

  fun destInfix tm =
      let
        val (t,b) = destComb tm
        val (c,a) = destComb t
        val (n,_) = destConst c
        val n = abbreviateConst n
      in
        if StringSet.member n infixStrings then (n,a,b)
        else raise Error "Syntax.destInfix"
      end;

  val isInfix = can destInfix;

  fun countNegs tm =
      case total destNeg tm of
        NONE => (0,tm)
      | SOME t => let val (n,r) = countNegs t in (n + 1, r) end;

  fun destBinder tm =
      let
        fun f (s,d) = case d tm of ([],_) => NONE | (vs,b) => SOME (s,vs,b)
      in
        case first f binders of
          SOME x => x
        | NONE => raise Error "Syntax.destBinder"
      end;

  val isBinder = can destBinder;

  val infixPrinter = Print.ppInfixes infixTokens (total destInfix);

  fun basic tm =
      if isVar tm then ppVar (destVar tm)
      else if isConst tm then ppConst (destConst tm)
      else ppBtm tm

  and application tm =
      let
        val (f,xs) = stripComb tm
      in
        Print.program
          (basic f ::
           map (Print.sequence (Print.addBreak 1) o basic) xs)
      end

  and binder (tm,r) =
      let
        fun ppBind tm =
            let
              val (sym,vs,body) = destBinder tm
              val (v,vs) = hdTl vs
            in
              Print.program
                [Print.addString sym,
                 ppVar v,
                 Print.program
                   (map (Print.sequence (Print.addBreak 1) o ppVar) vs),
                 Print.addString ".",
                 Print.addBreak 1,
                 if isBinder body then ppBind body else ppTm (body,false)]
            end

        val ppBinder = Print.block Print.Inconsistent 2 o ppBind
      in
        (if not (isBinder tm) then application
         else (if r then Print.ppBracket "(" ")" else I) ppBinder) tm
      end

  and negs (tm,r) =
      let
        val (n,tm) = countNegs tm
      in
        Print.blockProgram Print.Inconsistent n
          [Print.duplicate n (Print.addString negString),
           if isInfix tm then ppBtm tm else binder (tm,r)]
      end

  and ppBtm tm = Print.ppBracket "(" ")" ppTm (tm,false)

  and ppTm tmr = infixPrinter negs tmr;
in
  val ppTerm = Print.ppMap (fn tm => (tm,false)) ppTm;
end;

val termToString = Print.toString ppTerm;

(* Substitutions *)

val ppSubst =
    Print.ppMap
      (fn sub => (TermSubst.toListType sub, TermSubst.toList sub))
      (Print.ppPair
         (Print.ppList (Print.ppPair Name.pp ppType))
         (Print.ppList (Print.ppPair ppVar ppTerm)));

val substToString = Print.toString ppSubst;

(* Sequents and theorems *)

val showHyp = ref false;

local
  fun dots n = if n <= 5 then nChars #"." n else ".." ^ Int.toString n ^ "..";

  fun ppSeq binop {hyp,concl} =
      Print.blockProgram Print.Inconsistent 2
        [(if TermAlphaSet.null hyp then Print.skip
          else
            Print.sequence
              (Print.ppBracket "{" "}"
                 (if !showHyp then
                    (Print.ppMap TermAlphaSet.toList
                       (Print.ppOpList "," ppTerm))
                  else
                    Print.ppMap (dots o TermAlphaSet.size) Print.ppString) hyp)
              (Print.addBreak 1)),
         Print.addString (binop ^ " "),
         ppTerm concl];
in
  val ppSequent = ppSeq "?-";

  val ppThm = Print.ppMap sequent (ppSeq "|-");
end;

val sequentToString = Print.toString ppSequent;

val thmToString = Print.toString ppThm;

end
