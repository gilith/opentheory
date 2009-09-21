(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Term :> Term =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic terms.                                       *)
(* ------------------------------------------------------------------------- *)

type term = TypeTerm.term;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

type term' = TypeTerm.term';

val mk = TypeTerm.mk;

val dest = TypeTerm.dest;

(* Constants *)

fun mkConst' c_ty = TypeTerm.Const' c_ty;

fun destConst' tm' =
    case tm' of
      TypeTerm.Const' c_ty => c_ty
    | _ => raise Error "Term.destConst'";

val isConst' = can destConst';

fun mkConst c_ty = mk (mkConst' c_ty);

fun destConst tm = destConst' (dest tm);

val isConst = can destConst;

(* Variables *)

fun mkVar' v = TypeTerm.Var' v;

fun destVar' tm' =
    case tm' of
      TypeTerm.Var' v => v
    | _ => raise Error "Term.destVar'";

val isVar' = can destVar';

fun equalVar' var tm' =
    case tm' of
      TypeTerm.Var' v => Var.equal var v
    | _ => false;

fun mkVar v = mk (mkVar' v);

fun destVar tm = destVar' (dest tm);

val isVar = can destVar;

fun equalVar v tm = equalVar' v (dest tm);

(* Function applications *)

fun mkApp' f_a = TypeTerm.App' f_a;

fun destApp' tm' =
    case tm' of
      TypeTerm.App' f_a => f_a
    | _ => raise Error "Term.destApp'";

val isApp' = can destApp';

fun mkApp f_a = mk (mkApp' f_a);

fun destApp tm = destApp' (dest tm);

val isApp = can destApp;

(* Lambda abstractions *)

fun mkAbs' v_b = TypeTerm.Abs' v_b;

fun destAbs' tm' =
    case tm' of
      TypeTerm.Abs' v_b => v_b
    | _ => raise Error "Term.destAbs'";

val isAbs' = can destAbs';

fun mkAbs v_b = mk (mkAbs' v_b);

fun destAbs tm = destAbs' (dest tm);

val isAbs = can destAbs;

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = TypeTerm.id;

val id = TypeTerm.id;

val equalId = TypeTerm.equalId;

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

val size = TypeTerm.size;

val sizeList = TypeTerm.sizeList;

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence.               *)
(* ------------------------------------------------------------------------- *)

val compare = TypeTerm.compare;

val equal = TypeTerm.equal;

local
  fun acmp n bv1 bv2 bvEq (tm1,tm2) =
      let
        val {id = id1, tm = tm1, sz = sz1, ty = _} = TypeTerm.info tm1
        and {id = id2, tm = tm2, sz = sz2, ty = _} = TypeTerm.info tm2
    in
      if bvEq andalso id1 = id2 then EQUAL
      else
        case Int.compare (sz1,sz2) of
          LESS => LESS
        | EQUAL => acmp' n bv1 bv2 bvEq (tm1,tm2)
        | GREATER => GREATER
    end

  and acmp' n bv1 bv2 bvEq tm1_tm2 =
      case tm1_tm2 of
        (TypeTerm.Const' c1_ty1, TypeTerm.Const' c2_ty2) =>
        prodCompare Const.compare Type.compare (c1_ty1,c2_ty2)
      | (TypeTerm.Const' _, _) => LESS
      | (_, TypeTerm.Const' _) => GREATER
      | (TypeTerm.Var' v1, TypeTerm.Var' v2) =>
        (case (VarMap.peek bv1 v1, VarMap.peek bv2 v2) of
           (NONE,NONE) => Var.compare (v1,v2)
         | (SOME _, NONE) => LESS
         | (NONE, SOME _) => GREATER
         | (SOME i1, SOME i2) => Int.compare (i1,i2))
      | (TypeTerm.Var' _, _) => LESS
      | (_, TypeTerm.Var' _) => GREATER
      | (TypeTerm.App' f1_a1, TypeTerm.App' f2_a2) =>
        let
          val cmp = acmp n bv1 bv2 bvEq
        in
          prodCompare cmp cmp (f1_a1,f2_a2)
        end
      | (TypeTerm.App' _, _) => LESS
      | (_, TypeTerm.App' _) => GREATER
      | (TypeTerm.Abs' (v1,b1), TypeTerm.Abs' (v2,b2)) =>
        let
          val bvEq = bvEq andalso Var.equal v1 v2
          val bv1 = VarMap.insert bv1 (v1,n)
          val bv2 = if bvEq then bv1 else VarMap.insert bv2 (v2,n)
          val n = n + 1
        in
          acmp n bv1 bv2 bvEq (b1,b2)
        end;
in
  val alphaCompare =
      let
        val n = 0
        val bv = VarMap.new ()
        val bvEq = true
      in
        acmp n bv bv bvEq
      end;
end;

fun alphaEqual tm1 tm2 = alphaCompare (tm1,tm2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* The type of a term.                                                       *)
(* ------------------------------------------------------------------------- *)

val typeOf = TypeTerm.typeOf;

(* ------------------------------------------------------------------------- *)
(* Free variables.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype sharingFreeVars =
    SharingFreeVars of
      {seen : VarSet.set IntMap.map};

val newSharingFreeVars =
    let
      val seen = IntMap.new ()
    in
      SharingFreeVars
        {seen = seen}
    end;

fun rawSharingFreeVars tm seen =
    let
      val {id,tm,...} = TypeTerm.info tm
    in
      case IntMap.peek seen id of
        SOME vs => (vs,seen)
      | NONE =>
        case tm of
          TypeTerm.Const' _ =>
          let
            val fv = VarSet.empty
            val seen = IntMap.insert seen (id,fv)
          in
            (fv,seen)
          end
        | TypeTerm.Var' v =>
          let
            val fv = VarSet.singleton v
            val seen = IntMap.insert seen (id,fv)
          in
            (fv,seen)
          end
        | TypeTerm.App' (f,a) =>
          let
            val (fv1,seen) = rawSharingFreeVars f seen
            val (fv2,seen) = rawSharingFreeVars a seen
            val fv = VarSet.union fv1 fv2
            val seen = IntMap.insert seen (id,fv)
          in
            (fv,seen)
          end
        | TypeTerm.Abs' (v,b) =>
          let
            val (fv,seen) = rawSharingFreeVars b seen
            val fv = if VarSet.member v fv then VarSet.delete fv v else fv
            val seen = IntMap.insert seen (id,fv)
          in
            (fv,seen)
          end
    end;

fun sharingFreeVars tm (SharingFreeVars {seen}) =
    let
      val (fv,seen) = rawSharingFreeVars tm seen
      val share = SharingFreeVars {seen = seen}
    in
      (fv,share)
    end;

local
  fun add (tm,(fvs,share)) =
      let
        val (fv,share) = sharingFreeVars tm share
        val fvs = VarSet.union fvs fv
      in
        (fvs,share)
      end;
in
  fun freeVarsList tms =
      let
        val fvs = VarSet.empty
        val share = newSharingFreeVars
        val (fvs,_) = List.foldl add (fvs,share) tms
      in
        fvs
      end;
end;

fun freeVars tm =
    let
      val share = newSharingFreeVars
      val (fv,_) = sharingFreeVars tm share
    in
      fv
    end;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

datatype sharingConsts =
    SharingConsts of
      {seen : IntSet.set,
       cons : ConstSet.set};

val emptySharingConsts =
    let
      val seen = IntSet.empty
      val cons = ConstSet.empty
    in
      SharingConsts
        {seen = seen,
         cons = cons}
    end;

fun sharingConsts seen acc tms =
    case tms of
      [] => (seen,acc)
    | tm :: tms =>
      let
        val {id,tm,...} = TypeTerm.info tm
      in
        if IntSet.member id seen then sharingConsts seen acc tms
        else
          let
            val seen = IntSet.add seen id
          in
            sharingConsts' seen acc tm tms
          end
      end

and sharingConsts' seen acc tm tms =
    case tm of
      TypeTerm.Const' (c,_) =>
      let
        val acc = ConstSet.add acc c
      in
        sharingConsts seen acc tms
      end
    | TypeTerm.Var' _ => sharingConsts seen acc tms
    | TypeTerm.App' (f,a) =>
      let
        val tms = f :: a :: tms
      in
        sharingConsts seen acc tms
      end
    | TypeTerm.Abs' (_,b) =>
      let
        val tms = b :: tms
      in
        sharingConsts seen acc tms
      end;

fun addSharingConsts (SharingConsts {seen,cons}) tms =
    let
      val (seen,cons) = sharingConsts seen cons tms
    in
      SharingConsts
        {seen = seen,
         cons = cons}
    end;

fun toSetSharingConsts (SharingConsts {cons,...}) = cons;

fun constsList tms =
    let
      val share = emptySharingConsts
      val share = addSharingConsts share tms
    in
      toSetSharingConsts share
    end;

fun consts tm = constsList [tm];

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype sharingTypeVars =
    SharingTypeVars of
      {tyShare : Type.sharingTypeVars,
       seen : IntSet.set};

val emptySharingTypeVars =
    let
      val tyShare = Type.emptySharingTypeVars
      val seen = IntSet.empty
    in
      SharingTypeVars
        {tyShare = tyShare,
         seen = seen}
    end;

fun sharingTypeVars tyShare seen tms =
    case tms of
      [] => (tyShare,seen)
    | tm :: tms =>
      let
        val {id,tm,...} = TypeTerm.info tm
      in
        if IntSet.member id seen then sharingTypeVars tyShare seen tms
        else
          let
            val seen = IntSet.add seen id
          in
            sharingTypeVars' tyShare seen tm tms
          end
      end

and sharingTypeVars' tyShare seen tm tms =
    case tm of
      TypeTerm.Const' (_,ty) =>
      let
        val tyShare = Type.addSharingTypeVars tyShare [ty]
      in
        sharingTypeVars tyShare seen tms
      end
    | TypeTerm.Var' v =>
      let
        val tyShare = Var.addSharingTypeVars tyShare v
      in
        sharingTypeVars tyShare seen tms
      end
    | TypeTerm.App' (f,a) =>
      let
        val tms = f :: a :: tms
      in
        sharingTypeVars tyShare seen tms
      end
    | TypeTerm.Abs' (v,b) =>
      let
        val tyShare = Var.addSharingTypeVars tyShare v
        val tms = b :: tms
      in
        sharingTypeVars tyShare seen tms
      end;

fun addSharingTypeVars (SharingTypeVars {tyShare,seen}) tms =
    let
      val (tyShare,seen) = sharingTypeVars tyShare seen tms
    in
      SharingTypeVars
        {tyShare = tyShare,
         seen = seen}
    end;

fun toSetSharingTypeVars (SharingTypeVars {tyShare,...}) =
    Type.toSetSharingTypeVars tyShare;

fun typeVarsList tms =
    let
      val share = emptySharingTypeVars
      val share = addSharingTypeVars share tms
    in
      toSetSharingTypeVars share
    end;

fun typeVars tm = typeVarsList [tm];

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype sharingTypeOps =
    SharingTypeOps of
      {tyShare : Type.sharingTypeOps,
       seen : IntSet.set};

val emptySharingTypeOps =
    let
      val tyShare = Type.emptySharingTypeOps
      val seen = IntSet.empty
    in
      SharingTypeOps
        {tyShare = tyShare,
         seen = seen}
    end;

fun sharingTypeOps tyShare seen tms =
    case tms of
      [] => (tyShare,seen)
    | tm :: tms =>
      let
        val {id,tm,...} = TypeTerm.info tm
      in
        if IntSet.member id seen then sharingTypeOps tyShare seen tms
        else
          let
            val seen = IntSet.add seen id
          in
            sharingTypeOps' tyShare seen tm tms
          end
      end

and sharingTypeOps' tyShare seen tm tms =
    case tm of
      TypeTerm.Const' (_,ty) =>
      let
        val tyShare = Type.addSharingTypeOps tyShare [ty]
      in
        sharingTypeOps tyShare seen tms
      end
    | TypeTerm.Var' v =>
      let
        val tyShare = Var.addSharingTypeOps tyShare v
      in
        sharingTypeOps tyShare seen tms
      end
    | TypeTerm.App' (f,a) =>
      let
        val tms = f :: a :: tms
      in
        sharingTypeOps tyShare seen tms
      end
    | TypeTerm.Abs' (v,b) =>
      let
        val tyShare = Var.addSharingTypeOps tyShare v
        val tms = b :: tms
      in
        sharingTypeOps tyShare seen tms
      end;

fun addSharingTypeOps (SharingTypeOps {tyShare,seen}) tms =
    let
      val (tyShare,seen) = sharingTypeOps tyShare seen tms
    in
      SharingTypeOps
        {tyShare = tyShare,
         seen = seen}
    end;

fun toSetSharingTypeOps (SharingTypeOps {tyShare,...}) =
    Type.toSetSharingTypeOps tyShare;

fun typeOpsList tms =
    let
      val share = emptySharingTypeOps
      val share = addSharingTypeOps share tms
    in
      toSetSharingTypeOps share
    end;

fun typeOps tm = typeOpsList [tm];

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

fun mkEqTy a = Type.mkFun (a, Type.mkFun (a, Type.bool));

fun destEqTy ty =
    let
      val (x,yb) = Type.destFun ty
      val (y,b) = Type.destFun yb
      val _ = Type.equal b Type.bool orelse
              raise Error "Term.destEqTy: not a relation"
      val _ = Type.equal x y orelse
              raise Error "Term.destEqTy: different argument types"
    in
      x
    end;

val isEqTy = can destEqTy;

val stringEq = "=";

val nameEq = Name.mkGlobal stringEq;

val constEq =
    let
      val name = nameEq
      val prov = TypeTerm.UndefProvConst
    in
      TypeTerm.Const
        {name = name,
         prov = prov}
    end;

fun mkEq (l,r) =
    let
      val ty = mkEqTy (typeOf l)
      val c = mkConst (constEq,ty)
      val t = mkApp (c,l)
    in
      mkApp (t,r)
    end;

fun destEq tm =
    let
      val (el,r) = destApp tm
      val (e,l) = destApp el
      val (c,_) = destConst e
    in
      if Const.equal c constEq then (l,r)
      else raise Error "Term.destEq"
    end;

val isEq = can destEq;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val maximumSize = ref 1000;

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

local
  val negString = "~";

  val binders = ["\\","!","?","?!","select"];

  val infixStrings = Print.tokensInfixes infixTokens;

  val binderStrings = StringSet.fromList binders;

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

  fun destNeg tm =
      let
        val (c,a) = destComb tm
        val (n,_) = destConst c
        val n = abbreviateConst n
      in
        if n = negString then a else raise Error "Syntax.destNeg"
      end;

  fun countNegs tm =
      case total destNeg tm of
        NONE => (0,tm)
      | SOME t => let val (n,r) = countNegs t in (n + 1, r) end;

  fun destBinder tm =
      let
        val (n,tm) =
            if isAbs tm then ("\\",tm)
            else
              let
                val (c,t) = destComb tm
                val (n,_) = destConst c
                val n = abbreviateConst n
              in
                if StringSet.member n binderStrings then (n,t)
                else raise Error "Syntax.destBinder"
              end

        val (v,b) = destAbs tm
      in
        (n,v,b)
      end;

  val isBinder = can destBinder;

  fun stripBinder tm =
      let
        val (n,v,b) = destBinder tm

        fun dest vs t =
            case total destBinder t of
              NONE => (vs,t)
            | SOME (n',v,b) =>
              if n' = n then dest (v :: vs) b else (vs,t)

        val (vs,b) = dest [] b
      in
        (n, v, rev vs, b)
      end;

  val infixPrinter = Print.ppInfixes infixTokens (total destInfix);

  fun basic tm =
      if isVar tm then Var.pp (destVar tm)
      else if isConst tm then ppConst (destConst tm)
      else ppBtm tm

  and application tm =
      case total destComb tm of
        NONE => basic tm
      | SOME (f,x) =>
        Print.program
          [function f,
           Print.addBreak 1,
           basic x]

  and function tm = if isInfix tm then ppBtm tm else binder (tm,true)

  and binder (tm,r) =
      let
        fun ppBind tm =
            let
              val (sym,v,vs,body) = stripBinder tm

              val printSym =
                  case String.size sym of
                    0 => Print.addString "EmptyBinder"
                  | n =>
                    let
                      val pp = Print.addString sym
                    in
                      if not (Char.isAlphaNum (String.sub (sym, n - 1))) then pp
                      else Print.sequence pp (Print.addString " ")
                    end
            in
              Print.program
                [printSym,
                 Var.pp v,
                 Print.program
                   (map (Print.sequence (Print.addBreak 1) o Var.pp) vs),
                 Print.addString ".",
                 Print.addBreak 1,
                 if isBinder body then ppBind body else ppTm (body,false)]
            end

        val ppBinder = Print.block Print.Inconsistent 2 o ppBind
      in
        if not (isBinder tm) then application
        else (if r then Print.ppBracket "(" ")" else I) ppBinder
      end tm

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
  fun pp tm =
      let
        val n = size tm
      in
        if n <= !maximumSize then ppTm (tm,false)
        else Print.addString ("term{" ^ Int.toString n ^ "}")
      end;
end;

val toString = Print.toString pp;

end

structure TermOrdered =
struct type t = Term.term val compare = Term.compare end

structure TermSet = ElementSet (TermOrdered)

structure TermMap = KeyMap (TermOrdered)

structure TermAlphaOrdered =
struct type t = Term.term val compare = Term.alphaCompare end

structure TermAlphaSet =
struct

  local
    structure S = ElementSet (TermAlphaOrdered);
  in
    open S;
  end;

  val typeOps =
      let
        fun addNames (tm,acc) = NameSet.union acc (Term.typeOps tm)
      in
        foldl addNames NameSet.empty
      end;

  val consts =
      let
        fun addNames (tm,acc) = NameSet.union acc (Term.consts tm)
      in
        foldl addNames NameSet.empty
      end;

end

structure TermAlphaMap = KeyMap (TermAlphaOrdered)
