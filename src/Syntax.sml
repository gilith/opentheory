(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Syntax :> Syntax =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Primitive                                                                 *)
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

(* The type of individuals *)

val indTy = Type.indTy;

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

(* Hilbert's indefinite choice operator (epsilon) *)

val selectTy = Term.selectTy;
val selectTm = Term.selectTm;
val mkSelect = Term.mkSelect;
val destSelect = Term.destSelect;
val isSelect = Term.isSelect;

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

(* Theorems *)

fun axioms th = case Thm.dest th of Thm.Thm {axioms,...} => axioms;

fun sequent th = case Thm.dest th of Thm.Thm {sequent,...} => sequent;

fun hyp th = case Thm.dest th of Thm.Thm {sequent = {hyp, ...}, ...} => hyp;

fun concl th =
    case Thm.dest th of Thm.Thm {sequent = {concl, ...}, ...} => concl;

(* ------------------------------------------------------------------------- *)
(* Boolean                                                                   *)
(* ------------------------------------------------------------------------- *)

(* Negations *)

val negName = Name.mkGlobal "~";

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

(* ------------------------------------------------------------------------- *)
(* Pretty-printing                                                           *)
(* ------------------------------------------------------------------------- *)

(* Types *)

val typeInfixTokens : Parser.infixities =
    [{token = " * ", precedence = 3,  leftAssoc = false},
     {token = " + ", precedence = 2,  leftAssoc = false},
     {token = " -> ", precedence = 1,  leftAssoc = false}];

local
  val typeInfixStrings = Parser.infixTokens typeInfixTokens;

  fun abbreviateTypeOp n =
      case Name.toString n of
        "fun" => "->"
      | s => s;

  val ppTypeVar = Name.pp;

  val ppTypeOp = Parser.ppMap abbreviateTypeOp Parser.ppString;

  fun destTypeInfix ty =
      let
        val (f,xs) = destTypeOp ty
        val f = abbreviateTypeOp f
        val _ = mem f typeInfixStrings orelse raise Error "destTypeInfix"
      in
        case xs of
          [a,b] => (f,a,b)
        | _ => raise Bug ("destTypeInfix: bad arity of type operator " ^ f)
      end;

  val isTypeInfix = can destTypeInfix;

  val typeInfixPrinter =
      Parser.ppInfixes typeInfixTokens (total destTypeInfix);

  fun basic pp ty =
      if isTypeVar ty then ppTypeVar pp (destTypeVar ty)
      else if isTypeInfix ty then ppBtype pp ty
      else
        let
          val (f,xs) = destTypeOp ty
        in
          Parser.beginBlock pp Parser.Inconsistent 0;
          (case xs of
             [] => ()
           | [x] => (basic pp ty; Parser.addBreak pp (1,0))
           | _ =>
             (Parser.ppBracket "(" ")" (Parser.ppSequence "," ppType) pp xs;
              Parser.addBreak pp (1,0)));
          ppTypeOp pp f;
          Parser.endBlock pp
        end

  and basicr pp (ty,_) = basic pp ty

  and ppBtype pp ty = Parser.ppBracket "(" ")" ppType pp ty

  and ppTyper pp tyr = typeInfixPrinter basicr pp tyr

  and ppType pp ty = ppTyper pp (ty,false);
in
  val ppType = ppType;
end;

val typeToString = Parser.toString ppType;

(* Terms *)

val showTypes = ref false;

val infixTokens : Parser.infixities =
    [(* ML style *)
     {token = " / ",   precedence = 7,  leftAssoc = true},
     {token = " div ", precedence = 7,  leftAssoc = true},
     {token = " mod ", precedence = 7,  leftAssoc = true},
     {token = " * ",   precedence = 7,  leftAssoc = true},
     {token = " + ",   precedence = 6,  leftAssoc = true},
     {token = " - ",   precedence = 6,  leftAssoc = true},
     {token = " ^ ",   precedence = 6,  leftAssoc = true},
     {token = " @ ",   precedence = 5,  leftAssoc = false},
     {token = " :: ",  precedence = 5,  leftAssoc = false},
     {token = " = ",   precedence = 4,  leftAssoc = true},
     {token = " <> ",  precedence = 4,  leftAssoc = true},
     {token = " <= ",  precedence = 4,  leftAssoc = true},
     {token = " < ",   precedence = 4,  leftAssoc = true},
     {token = " >= ",  precedence = 4,  leftAssoc = true},
     {token = " > ",   precedence = 4,  leftAssoc = true},
     {token = " o ",   precedence = 3,  leftAssoc = true},
     (* HOL style *)
     {token = " /\\ ", precedence = ~1, leftAssoc = false},
     {token = " \\/ ", precedence = ~2, leftAssoc = false},
     {token = " ==> ", precedence = ~3, leftAssoc = false},
     {token = " <=> ", precedence = ~4, leftAssoc = false}];

val ppVar =
    let
      val pp1 = Parser.ppBracket "(" ")" (Parser.ppBinop " :" Name.pp ppType)
      val pp2 = Parser.ppMap fst Name.pp
    in
      fn pp => fn v => (if !showTypes then pp1 else pp2) pp v
    end;

local
  val binders =
      [("\\",stripAbs),
       ("!",stripForall),
       ("?",stripExists),
       ("?!",stripExistsUnique)];

  val infixStrings = Parser.infixTokens infixTokens;

  val binderStrings = map fst binders;

  fun abbreviateConst n =
      case Name.toString n of
        s => s;

  fun specialString n =
      n = "~" orelse mem n infixStrings orelse mem n binderStrings;

  val ppConst =
      let
        fun f (n,_) =
            let
              val n = abbreviateConst n
            in
              if specialString n then "(" ^ n ^ ")" else n
            end
      in
        Parser.ppMap f Parser.ppString
      end;

  fun destInfix tm =
      let
        val (t,b) = destComb tm
        val (c,a) = destComb t
        val (n,_) = destConst c
        val n = abbreviateConst n
      in
        if mem n infixStrings then (n,a,b) else raise Error "HOL.destInfix"
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

  val infixPrinter = Parser.ppInfixes infixTokens (total destInfix);

  fun basic pp tm =
      if isVar tm then ppVar pp (destVar tm)
      else if isConst tm then ppConst pp (destConst tm)
      else ppBtm pp tm

  and application pp tm =
      let
        val (f,xs) = stripComb tm
      in
        basic pp f;
        app (fn x => (Parser.addBreak pp (1,0); basic pp x)) xs
      end

  and binder pp (tm,r) =
      let
        fun ppBind pp tm =
            let
              val (sym,vs,body) = destBinder tm
              val (v,vs) = hdTl vs
            in
              Parser.addString pp sym;
              ppVar pp v;
              app (fn a => (Parser.addBreak pp (1,0); ppVar pp a)) vs;
              Parser.addString pp ".";
              Parser.addBreak pp (1,0);
              if isBinder body then ppBind pp body else ppTm pp (body,false)
            end

        fun ppBinder pp tm =
            (Parser.beginBlock pp Parser.Inconsistent 2;
             ppBind pp tm;
             Parser.endBlock pp)
      in
        (if not (isBinder tm) then application
         else (if r then Parser.ppBracket "(" ")" else I) ppBinder) pp tm
      end

  and negs pp (tm,r) =
      let
        val (n,tm) = countNegs tm
      in
        Parser.beginBlock pp Parser.Inconsistent n;
        funpow n (fn () => Parser.addString pp "~") ();
        if isInfix tm then ppBtm pp tm else binder pp (tm,r);
        Parser.endBlock pp
      end

  and ppBtm pp tm = Parser.ppBracket "(" ")" ppTm pp (tm,false)

  and ppTm pp tmr = infixPrinter negs pp tmr;
in
  val ppTerm = Parser.ppMap (fn tm => (tm,false)) ppTm;
end;

val termToString = Parser.toString ppTerm;

(* Substitutions *)

val ppSubst =
    Parser.ppMap
      (fn sub => (TermSubst.toListType sub, TermSubst.toList sub))
      (Parser.ppPair (Parser.ppList (Parser.ppPair Name.pp ppType))
                     (Parser.ppList (Parser.ppPair ppVar ppTerm)));

val substToString = Parser.toString ppSubst;

(* Sequents and theorems *)

local
  fun ppSeq pp binop hyp concl =
      (Parser.beginBlock pp Parser.Inconsistent 2;
       if TermAlphaSet.null hyp then ()
       else
         (Parser.ppBracket "{" "}"
            (Parser.ppSequence "," ppTerm) pp (TermAlphaSet.toList hyp);
          Parser.addBreak pp (1,0));
       Parser.addString pp (binop ^ " ");
       ppTerm pp concl;
       Parser.endBlock pp);
in
  fun ppSequent pp {hyp,concl} = ppSeq pp "?-" hyp concl;

  fun ppThm pp th =
      case Thm.dest th of
        Thm.Thm {sequent = {hyp,concl}, ...} => ppSeq pp "|-" hyp concl;
end;

val sequentToString = Parser.toString ppSequent;

val thmToString = Parser.toString ppThm;

end
