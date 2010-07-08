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

fun rator tm = fst (destApp tm);

fun rand tm = snd (destApp tm);

fun land tm = rand (rator tm);

local
  fun mk (x,tm) = mkApp (tm,x);
in
  fun listMkApp (tm,xs) = List.foldl mk tm xs;
end;

local
  fun strip acc tm =
      case total destApp tm of
        NONE => (tm,acc)
      | SOME (tm,x) => strip (x :: acc) tm;
in
  val stripApp = strip [];
end;

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

fun listMkAbs (vs,tm) = List.foldl mkAbs tm (rev vs);

local
  fun strip acc tm =
      case total destAbs tm of
        NONE => (rev acc, tm)
      | SOME (v,tm) => strip (v :: acc) tm;
in
  val stripAbs = strip [];
end;

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

fun addConstSharingConsts c share =
    let
      val SharingConsts {seen,cons} = share

      val cons = ConstSet.add cons c
    in
      SharingConsts
        {seen = seen,
         cons = cons}
    end;

fun addConstSetSharingConsts cs share =
    let
      val SharingConsts {seen,cons} = share

      val cons = ConstSet.union cons cs
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

fun addListSharingConsts tms (SharingConsts {seen,cons}) =
    let
      val (seen,cons) = sharingConsts seen cons tms
    in
      SharingConsts
        {seen = seen,
         cons = cons}
    end;

fun addSharingConsts tm share = addListSharingConsts [tm] share;

fun unionSharingConsts share1 share2 =
    let
      val SharingConsts {seen = seen1, cons = cons1} = share1
      and SharingConsts {seen = seen2, cons = cons2} = share2

      val seen = IntSet.union seen1 seen2

      val cons = ConstSet.union cons1 cons2
    in
      SharingConsts
        {seen = seen,
         cons = cons}
    end;

fun toSetSharingConsts (SharingConsts {cons,...}) = cons;

fun constsList tms =
    let
      val share = emptySharingConsts

      val share = addListSharingConsts tms share
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
        val tyShare = Type.addSharingTypeVars ty tyShare
      in
        sharingTypeVars tyShare seen tms
      end
    | TypeTerm.Var' v =>
      let
        val tyShare = Var.addSharingTypeVars v tyShare
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
        val tyShare = Var.addSharingTypeVars v tyShare
        val tms = b :: tms
      in
        sharingTypeVars tyShare seen tms
      end;

fun addListSharingTypeVars tms (SharingTypeVars {tyShare,seen}) =
    let
      val (tyShare,seen) = sharingTypeVars tyShare seen tms
    in
      SharingTypeVars
        {tyShare = tyShare,
         seen = seen}
    end;

fun addSharingTypeVars tm share = addListSharingTypeVars [tm] share;

fun toSetSharingTypeVars (SharingTypeVars {tyShare,...}) =
    Type.toSetSharingTypeVars tyShare;

fun typeVarsList tms =
    let
      val share = emptySharingTypeVars

      val share = addListSharingTypeVars tms share
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

fun addTypeOpSharingTypeOps ot share =
    let
      val SharingTypeOps {tyShare,seen} = share

      val tyShare = Type.addTypeOpSharingTypeOps ot tyShare
    in
      SharingTypeOps
        {tyShare = tyShare,
         seen = seen}
    end;

fun addTypeOpSetSharingTypeOps ots share =
    let
      val SharingTypeOps {tyShare,seen} = share

      val tyShare = Type.addTypeOpSetSharingTypeOps ots tyShare
    in
      SharingTypeOps
        {tyShare = tyShare,
         seen = seen}
    end;

fun addTypeSharingTypeOps ty share =
    let
      val SharingTypeOps {tyShare,seen} = share

      val tyShare = Type.addSharingTypeOps ty tyShare
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
        val tyShare = Type.addSharingTypeOps ty tyShare
      in
        sharingTypeOps tyShare seen tms
      end
    | TypeTerm.Var' v =>
      let
        val tyShare = Var.addSharingTypeOps v tyShare
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
        val tyShare = Var.addSharingTypeOps v tyShare
        val tms = b :: tms
      in
        sharingTypeOps tyShare seen tms
      end;

fun addListSharingTypeOps tms (SharingTypeOps {tyShare,seen}) =
    let
      val (tyShare,seen) = sharingTypeOps tyShare seen tms
    in
      SharingTypeOps
        {tyShare = tyShare,
         seen = seen}
    end;

fun addSharingTypeOps tm share = addListSharingTypeOps [tm] share;

fun unionSharingTypeOps share1 share2 =
    let
      val SharingTypeOps {tyShare = tyShare1, seen = seen1} = share1
      and SharingTypeOps {tyShare = tyShare2, seen = seen2} = share2

      val tyShare = Type.unionSharingTypeOps tyShare1 tyShare2

      val seen = IntSet.union seen1 seen2
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

      val share = addListSharingTypeOps tms share
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

val boolEqTy = mkEqTy Type.bool;

val stringEq = "="
and stringBoolEq = "<=>";

val nameEq = Name.mkGlobal stringEq
and nameBoolEq = Name.mkGlobal stringBoolEq;

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

fun lhs tm = fst (destEq tm);

fun rhs tm = snd (destEq tm);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {abs : string,
       negations : string list,
       infixes : Print.infixes,
       binders : string list,
       maximumSize : int};

val defaultGrammar =
    let
      val abs = "\\"

      val negations = ["~"]

      val infixes =
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
             {token = " <=> ", precedence = ~4, leftAssoc = false},
             {token = ", ", precedence = ~1000, leftAssoc = false}]

      val binders = ["!","?","?!","select"]

      val maximumSize = 1000
    in
      Grammar
        {abs = abs,
         negations = negations,
         infixes = infixes,
         binders = binders,
         maximumSize = maximumSize}
    end;

val bit0Name = Name.mkGlobal "bit0"
and bit1Name = Name.mkGlobal "bit1"
and numeralName = Name.mkGlobal "numeral"
and zeroName = Name.mkGlobal "zero";

local
  val mkMap =
      let
        fun add (s,m) = NameMap.insert m (Name.mkGlobal s, s)
      in
        StringSet.foldl add (NameMap.new ())
      end;

  val unionMap =
      let
        fun merge ((_,x),(_,y)) =
            if x = y then SOME x
            else raise Error ("Term.pp: ambiguous name strings: \"" ^
                              x ^ "\" and \"" ^ y ^ "\"")
      in
        NameMap.union merge
      end;

  fun ppTerm abs negations infixes binders specials ppInfixes show =
      let
        fun showConst (c,ty) =
            if Const.equal c constEq then
              if Type.equal ty boolEqTy then nameBoolEq else nameEq
            else
              Show.showName show (Const.name c)

        fun ppConst c_ty =
            let
              val n = showConst c_ty
            in
              case NameMap.peek specials n of
                SOME s => Print.ppBracket "(" ")" Print.ppString s
              | NONE => Name.pp n
            end

        fun destNumber tm =
            case dest tm of
              TypeTerm.Const' c =>
              let
                val n = showConst c
              in
                if Name.equal n zeroName then 0
                else raise Error "Term.pp.destNumber: bad const"
              end
            | TypeTerm.App' (f,x) =>
              let
                val n = showConst (destConst f)
              in
                if Name.equal n bit0Name then 2 * destNumber x
                else if Name.equal n bit1Name then 2 * destNumber x + 1
                else raise Error "Term.pp.destNumber: bad app"
              end
            | _ => raise Error "Term.pp.destNumber: bad term"

        fun destNumeral (f,x) =
            let
              val n = showConst (destConst f)
            in
              if Name.equal n numeralName then destNumber x
              else raise Error "Term.pp.destNumeral: no marker"
            end

        fun destNegation tm =
            let
              val (c,a) = destApp tm

              val n = showConst (destConst c)
            in
              case NameMap.peek negations n of
                SOME s => (s,a)
              | NONE => raise Error "Term.pp.destNegation"
            end

        fun stripNegation tm =
            case total destNegation tm of
              SOME (s,a) =>
              let
                val (sl,t) = stripNegation a
              in
                (s :: sl, t)
              end
            | NONE => ([],tm)

        fun destInfix tm =
            let
              val (tm,b) = destApp tm

              val (c,a) = destApp tm

              val n = showConst (destConst c)
            in
              case NameMap.peek infixes n of
                SOME s => (s,a,b)
              | NONE => raise Error "Term.pp.destInfix"
            end

        val isInfix = can destInfix

        val ppInfix = ppInfixes (total destInfix)

        fun destBinder tm =
            case total destAbs tm of
              SOME (v,t) => (abs,v,t)
            | NONE =>
              let
                val (c,t) = destApp tm

                val (v,b) = destAbs t

                val n = showConst (destConst c)
              in
                case NameMap.peek binders n of
                  SOME s => (s,v,b)
                | NONE => raise Error "Term.pp.destBinder"
              end

        val isBinder = can destBinder

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
            end

        fun ppBasic tm =
            case dest tm of
              TypeTerm.Var' v => Var.pp v
            | TypeTerm.Const' c => ppConst c
            | TypeTerm.App' f_x =>
              (case total destNumeral f_x of
                 SOME i => Print.ppInt i
               | NONE => ppBracket tm)
            | TypeTerm.Abs' _ => ppBracket tm

        and ppApplication tm =
            case total destApp tm of
              NONE => ppBasic tm
            | SOME (f,x) =>
              case total destNumeral (f,x) of
                SOME i => Print.ppInt i
              | NONE =>
                Print.program
                  [ppFunction f,
                   Print.addBreak 1,
                   ppBasic x]

        and ppFunction tm =
            if isInfix tm then ppBracket tm else ppBinder (tm,true)

        and ppBind tm =
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
              Print.blockProgram Print.Inconsistent 2
                [printSym,
                 Var.pp v,
                 Print.program
                   (map (Print.sequence (Print.addBreak 1) o Var.pp) vs),
                 Print.addString ".",
                 Print.addBreak 1,
                 if isBinder body then ppBind body else ppNormal body]
            end

        and ppBinder (tm,r) =
            if not (isBinder tm) then ppApplication tm
            else if r then Print.ppBracket "(" ")" ppBind tm
            else ppBind tm

        and ppNegation (tm,r) =
            let
              val (syms,tm) = stripNegation tm
            in
              Print.blockProgram Print.Inconsistent (length syms)
                (map Print.addString syms @
                 [if isInfix tm then ppBracket tm else ppBinder (tm,r)])
            end

        and ppHanging tm_r = ppInfix ppNegation tm_r

        and ppNormal tm = ppHanging (tm,false)

        and ppBracket tm = Print.ppBracket "(" ")" ppNormal tm
      in
        ppNormal
      end;
in
  fun ppWithGrammar gram =
      let
        val Grammar {abs,negations,infixes,binders,maximumSize} = gram

        val negationNames = mkMap (StringSet.fromList negations)
        and infixNames = mkMap (Print.tokensInfixes infixes)
        and binderNames = mkMap (StringSet.fromList binders)

        val specialNames =
            unionMap negationNames (unionMap infixNames binderNames)

        val ppInfixes = Print.ppInfixes infixes
      in
        fn show => fn tm =>
           let
             val n = size tm
           in
             if n <= maximumSize then
               ppTerm abs negationNames infixNames binderNames
                 specialNames ppInfixes show tm
             else
               Print.ppBracket "term{" "}" Print.ppInt n
           end
      end
end;

val ppWithShow = ppWithGrammar defaultGrammar;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

end

structure TermOrdered =
struct type t = Term.term val compare = Term.compare end

structure TermMap = KeyMap (TermOrdered)

structure TermSet = ElementSet (TermMap)

structure TermAlphaOrdered =
struct type t = Term.term val compare = Term.alphaCompare end

structure TermAlphaMap = KeyMap (TermAlphaOrdered)

structure TermAlphaSet =
struct

  local
    structure S = ElementSet (TermAlphaMap);
  in
    open S;
  end;

  local
    fun addTm (tm,share) = Term.addSharingTypeOps tm share;
  in
    fun addSharingTypeOps set share = foldl addTm share set;
  end;

  fun typeOps set =
      let
        val share = Term.emptySharingTypeOps

        val share = addSharingTypeOps set share
      in
        Term.toSetSharingTypeOps share
      end;

  local
    fun addTm (tm,share) = Term.addSharingConsts tm share;
  in
    fun addSharingConsts set share = foldl addTm share set;
  end;

  fun consts set =
      let
        val share = Term.emptySharingConsts

        val share = addSharingConsts set share
      in
        Term.toSetSharingConsts share
      end;

  fun dealphaCompare (s1,s2) =
      if Portable.pointerEqual (s1,s2) then EQUAL
      else
        case Int.compare (size s1, size s2) of
          LESS => LESS
        | EQUAL =>
          let
            val l1 = Useful.sort Term.compare (toList s1)
            and l2 = Useful.sort Term.compare (toList s2)
          in
            Useful.lexCompare Term.compare (l1,l2)
          end
        | GREATER => GREATER;

end
