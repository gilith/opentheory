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

fun mkRefl tm = mkEq (tm,tm);

fun destRefl tm =
    let
      val (l,r) = destEq tm;

      val () = if equal l r then () else raise Error "Term.destRefl"
    in
      l
    end;

val isRefl = can destRefl;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {negations : Print.token list,
       infixes : Print.infixes,
       binders : Print.token list,
       showConst : Show.show -> Const.const * Type.ty -> Name.name,
       ppVar : Show.show -> Var.var Print.pp,
       ppConst : Show.show -> (Const.const * Type.ty) Print.pp,
       ppNegation : Show.show -> (Const.const * Type.ty) Print.pp,
       ppInfix : Show.show -> (Const.const * Type.ty) Print.pp,
       ppBinder : Show.show -> (Const.const * Type.ty) option Print.pp,
       maximumSize : int};

local
  val abs = "\\";

  val negations = ["~"];

  val infixes =
      Print.Infixes
        [(* ML style *)
         {token = "/", precedence = 7, assoc = Print.LeftAssoc},
         {token = "div", precedence = 7, assoc = Print.LeftAssoc},
         {token = "mod", precedence = 7, assoc = Print.LeftAssoc},
         {token = "*", precedence = 7, assoc = Print.LeftAssoc},
         {token = "+", precedence = 6, assoc = Print.LeftAssoc},
         {token = "-", precedence = 6, assoc = Print.LeftAssoc},
         {token = "^", precedence = 6, assoc = Print.LeftAssoc},
         {token = "@", precedence = 5, assoc = Print.RightAssoc},
         {token = "::", precedence = 5, assoc = Print.RightAssoc},
         {token = "=", precedence = 4, assoc = Print.NonAssoc},
         {token = "<>", precedence = 4, assoc = Print.NonAssoc},
         {token = "<=", precedence = 4, assoc = Print.NonAssoc},
         {token = "<", precedence = 4, assoc = Print.NonAssoc},
         {token = ">=", precedence = 4, assoc = Print.NonAssoc},
         {token = ">", precedence = 4, assoc = Print.NonAssoc},
         {token = "o", precedence = 3, assoc = Print.LeftAssoc},
         (* HOL style *)
         {token = "/\\", precedence = ~1, assoc = Print.RightAssoc},
         {token = "\\/", precedence = ~2, assoc = Print.RightAssoc},
         {token = "==>", precedence = ~3, assoc = Print.RightAssoc},
         {token = "<=>", precedence = ~4, assoc = Print.RightAssoc},
         {token = ",", precedence = ~1000, assoc = Print.RightAssoc}];

  val binders = ["!","?","?!","select","minimal"];

  fun showConst show (c,ty) =
      if Const.equal c constEq then
        if Type.equal ty boolEqTy then nameBoolEq else nameEq
      else
        Show.showName show (Const.name c);

  local
    val pairName = Name.mkGlobal ",";
  in
    fun ppInfixBuffer ppInf c_n =
        let
          val (_,n) = c_n

          val pps = [ppInf c_n, Print.addBreak 1]

          val pps =
              if Name.equal n pairName then pps
              else Print.ppString " " :: pps
        in
          Print.program pps
        end;
  end;

  local
    fun isAlpha c_n =
        case c_n of
          NONE => false
        | SOME (_,n) =>
          case Name.lastChar n of
            NONE => false
          | SOME c => Char.isAlpha c;
  in
    fun ppBinderBuffer ppBind c_n =
        let
          val pps = []

          val pps = if isAlpha c_n then Print.addBreak 1 :: pps else pps

          val pps = ppBind c_n :: pps
        in
          Print.program pps
        end;
  end;

  fun ppVar _ = Var.pp;

  fun ppConst show =
      let
        val toName = showConst show
      in
        Print.ppMap toName Name.pp
      end;

  fun ppNegation show =
      let
        val toName = showConst show
      in
        Print.ppMap toName Name.pp
      end;

  fun ppInfix show =
      let
        fun toName cty = (cty, showConst show cty)

        fun ppInf (_,n) = Name.pp n
      in
        Print.ppMap toName (ppInfixBuffer ppInf)
      end;

  fun ppBinder show =
      let
        fun toName cty =
            case cty of
              NONE => NONE
            | SOME cty => SOME (cty, showConst show cty)

        fun ppBind c =
            case c of
              NONE => Print.ppString abs
            | SOME (_,n) => Name.pp n
      in
        Print.ppMap toName (ppBinderBuffer ppBind)
      end;

  fun ppVarHtml show =
      let
        val toHtml = Var.toHtml show
      in
        Print.ppMap toHtml Html.ppFixed
      end;

  local
    fun mkSpan class inlines =
        let
          val attrs = Html.singletonAttrs ("class",class)
        in
          [Html.Span (attrs,inlines)]
        end;

    fun toHtmlConst show =
        let
          val toHtml = Const.toHtml show
        in
          fn (c,ty) =>
             let
               val n = showConst show (c,ty)
             in
               toHtml ((c, SOME ty), n)
             end
        end;

    fun toHtmlNegation show =
        let
          val toHtml = Const.toHtml show
        in
          fn (c,ty) =>
             let
               val n = showConst show (c,ty)
             in
               mkSpan "negation" (toHtml ((c, SOME ty), n))
             end
        end;

    fun toHtmlInfix show =
        let
          val toHtml = Const.toHtml show
        in
          fn ((c,ty),n) => mkSpan "infix" (toHtml ((c, SOME ty), n))
        end;

    fun toHtmlBinder show =
        let
          val toHtml = Const.toHtml show
        in
          fn ctyno =>
             let
               val h =
                   case ctyno of
                     SOME ((c,ty),n) => toHtml ((c, SOME ty), n)
                   | NONE => Name.toHtml (Name.mkGlobal abs)
             in
               mkSpan "binder" h
             end
        end;
  in
    fun ppConstHtml show = Print.ppMap (toHtmlConst show) Html.ppFixed;

    fun ppNegationHtml show = Print.ppMap (toHtmlNegation show) Html.ppFixed;

    fun ppInfixHtml show =
        let
          fun toName cty = (cty, showConst show cty)

          val ppInf = Print.ppMap (toHtmlInfix show) Html.ppFixed
        in
          Print.ppMap toName (ppInfixBuffer ppInf)
        end;

    fun ppBinderHtml show =
        let
          fun toName cty =
              case cty of
                NONE => NONE
              | SOME cty => SOME (cty, showConst show cty)

          val ppBind = Print.ppMap (toHtmlBinder show) Html.ppFixed
        in
          Print.ppMap toName (ppBinderBuffer ppBind)
        end;
  end;

  val maximumSize = 1000;
in
  val defaultGrammar =
      Grammar
        {negations = negations,
         infixes = infixes,
         binders = binders,
         showConst = showConst,
         ppVar = ppVar,
         ppConst = ppConst,
         ppNegation = ppNegation,
         ppInfix = ppInfix,
         ppBinder = ppBinder,
         maximumSize = maximumSize};

  val htmlGrammar =
      Grammar
        {negations = negations,
         infixes = infixes,
         binders = binders,
         showConst = showConst,
         ppVar = ppVarHtml,
         ppConst = ppConstHtml,
         ppNegation = ppNegationHtml,
         ppInfix = ppInfixHtml,
         ppBinder = ppBinderHtml,
         maximumSize = maximumSize};
end;

local
  val mkName = Name.mkGlobal;

  val bit0Name = mkName "bit0"
  and bit1Name = mkName "bit1"
  and condName = mkName "cond"
  and forallName = mkName "!"
  and letName = mkName "let"
  and selectName = mkName "select"
  and zeroName = mkName "zero";

  val mkMap =
      let
        fun add (s,m) =
            let
              val n = mkName s
            in
              case NameMap.peek m n of
                NONE => NameMap.insert m (n,s)
              | SOME s' => raise Error ("Term.pp.mkMap: name clash: \"" ^
                                        s ^ "\" and \"" ^ s' ^ "\"")
            end

        val emptyMap : Print.token NameMap.map = NameMap.new ();
      in
        StringSet.foldl add emptyMap
      end;

  fun mkSet s = NameSet.domain (mkMap (StringSet.fromList s));

  fun ppTerm negationNames infixNames binderNames specialNames showConst
             ppInfixes ppConstName ppNegationName ppInfixName ppBinderName
             ppVar show =
      let
        fun destCond show tm =
            let
              val (tm,b) = destApp tm

              val (tm,a) = destApp tm

              val (tm,c) = destApp tm

              val n = showConst show (destConst tm)

              val () = if Name.equal n condName then ()
                       else raise Error "Term.pp.destCond: not a cond"
            in
              (c,a,b)
            end

        fun isCond show = can (destCond show)

        fun destForall show tm =
            let
              val (c,t) = destApp tm

              val n = showConst show (destConst c)

              val () = if Name.equal n forallName then ()
                       else raise Error "Term.pp.destForall: not a forall"
            in
              destAbs t
            end

        fun stripForall show =
            let
              fun dst acc tm =
                  case total (destForall show) tm of
                    NONE => (rev acc, tm)
                  | SOME (v,tm) => dst (v :: acc) tm
            in
              dst []
            end

        fun destSelect show tm =
            let
              val (c,t) = destApp tm

              val n = showConst show (destConst c)

              val () = if Name.equal n selectName then ()
                       else raise Error "Term.pp.destSelect: not a select"
            in
              destAbs t
            end

        fun destGenAbs show tm =
            case total destAbs tm of
              SOME (v,t) => (mkVar v, t)
            | NONE =>
              let
                val (f,tm) = destSelect show tm

                val (vl,tm) = stripForall show tm

                val vs = VarSet.fromList vl

                val () = if length vl = VarSet.size vs then ()
                         else raise Error "Term.pp.destGenAbs: duplicate vars"

                val () = if not (VarSet.member f vs) then ()
                         else raise Error "Term.pp.destGenAbs: function is var"

                val (pat,body) = destEq tm

                val (ft,pat) = destApp pat

                val () = if equalVar f ft then ()
                         else raise Error "Term.pp.destGenAbs: no function"

                val () = if VarSet.equal (freeVars pat) vs then ()
                         else raise Error "Term.pp.destGenAbs: weird pat vars"

                val () = if not (VarSet.member f (freeVars body)) then ()
                         else raise Error "Term.pp.destGenAbs: function in body"
              in
                (pat,body)
              end

        fun destLet show tm =
            let
              val (tm,t) = destApp tm

              val (tm,vb) = destApp tm

              val n = showConst show (destConst tm)

              val () = if Name.equal n letName then ()
                       else raise Error "Term.pp.destLet: not a let"

              val (v,b) = destGenAbs show vb
            in
              (v,t,b)
            end

        fun isLet show = can (destLet show)

        fun destNumeral show tm =
            case dest tm of
              TypeTerm.Const' c =>
              let
                val n = showConst show c
              in
                if Name.equal n zeroName then 0
                else raise Error "Term.pp.destNumeral: bad const"
              end
            | TypeTerm.App' (f,x) =>
              let
                val n = showConst show (destConst f)
              in
                if Name.equal n bit0Name then
                  let
                    val i = destNumeral show x
                  in
                    if i > 0 then 2 * i
                    else raise Error "Term.pp.destNumeral: bit0 zero"
                  end
                else if Name.equal n bit1Name then
                  let
                    val i = destNumeral show x
                  in
                    2 * i + 1
                  end
                else raise Error "Term.pp.destNumeral: bad app"
              end
            | _ => raise Error "Term.pp.destNumeral: bad term"

        fun isNumeral show = can (destNumeral show);

        val ppNumeral = Print.ppInt

        fun ppConst c_ty =
            let
              val n = showConst show c_ty
            in
              if NameSet.member n specialNames then
                Print.ppBracket "(" ")" ppConstName c_ty
              else
                ppConstName c_ty
            end

        fun destNegation tm =
            let
              val (t,a) = destApp tm

              val c = destConst t

              val n = showConst show c
            in
              if NameSet.member n negationNames then (c,a)
              else raise Error "Term.pp.destNegation"
            end

        val isNegation = can destNegation

        fun stripNegation tm =
            case total destNegation tm of
              SOME (s,a) =>
              let
                val (sl,t) = stripNegation a
              in
                (s :: sl, t)
              end
            | NONE => ([],tm)

        fun destInfixTerm tm =
            let
              val (t,b) = destApp tm

              val (t,a) = destApp t

              val c = destConst t

              val n = showConst show c
            in
              ((c,n),a,b)
            end

        fun destInfix tm =
            let
              val ((_,n),a,b) = destInfixTerm tm
            in
              case NameMap.peek infixNames n of
                SOME s => (s,a,b)
              | NONE => raise Error "Term.pp.destInfix"
            end

        val isInfix = can destInfix

        fun ppInfixToken (tm,_) =
            let
              val ((c,_),_,_) = destInfixTerm tm
            in
              ppInfixName c
            end

        val ppInfix = ppInfixes (total destInfix) ppInfixToken

        fun destBinder tm =
            case total (destGenAbs show) tm of
              SOME (v,t) => (NONE,v,t)
            | NONE =>
              let
                val (t,a) = destApp tm

                val (v,b) = destGenAbs show a

                val c = destConst t

                val n = showConst show c
              in
                if NameSet.member n binderNames then (SOME c, v, b)
                else raise Error "Term.pp.destBinder"
              end

        val isBinder = can destBinder

        fun equalBinder ct1 ct2 =
            case (ct1,ct2) of
              (NONE,NONE) => true
            | (SOME (c1,_), SOME (c2,_)) => Const.equal c1 c2
            | _ => false

        fun stripBinder tm =
            let
              val (c,v,b) = destBinder tm

              fun dest vs t =
                  case total destBinder t of
                    NONE => (vs,t)
                  | SOME (c',v,b) =>
                    if equalBinder c c' then dest (v :: vs) b else (vs,t)

              val (vs,b) = dest [] b
            in
              (c, v, rev vs, b)
            end

        fun destGenApp tm =
            if isNumeral show tm then raise Error "Term.pp.destGenApp: numeral"
            else if isCond show tm then raise Error "Term.pp.destGenApp: cond"
            else if isLet show tm then raise Error "Term.pp.destGenApp: let"
            else if isInfix tm then raise Error "Term.pp.destGenApp: infix"
            else if isNegation tm then raise Error "Term.pp.destGenApp: negation"
            else if isBinder tm then raise Error "Term.pp.destGenApp: binder"
            else destApp tm

        val stripGenApp =
            let
              fun strip acc tm =
                  case total destGenApp tm of
                    NONE => (tm,acc)
                  | SOME (f,x) => strip (x :: acc) f
            in
              strip []
            end

        fun ppBasicTerm tm =
            case total (destNumeral show) tm of
              SOME i => ppNumeral i
            | NONE =>
              case dest tm of
                TypeTerm.Var' v => ppVar v
              | TypeTerm.Const' c_ty => ppConst c_ty
              | TypeTerm.App' _ => ppBracketTerm tm
              | TypeTerm.Abs' _ => ppBracketTerm tm

        and ppApplicationTerm tm =
            let
              val (tm,xs) = stripGenApp tm
            in
              if null xs then ppBasicTerm tm
              else
                Print.blockProgram Print.Inconsistent 2
                  (ppBasicTerm tm ::
                   List.map (Print.sequence (Print.addBreak 1) o ppBasicTerm) xs)
            end

        and ppBindTerm tm =
            let
              val (c,v,vs,body) = stripBinder tm
            in
              Print.blockProgram Print.Inconsistent 2
                [ppBinderName c,
                 ppBasicTerm v,
                 Print.program
                   (List.map
                      (Print.sequence (Print.addBreak 1) o ppBasicTerm) vs),
                 Print.ppString ".",
                 Print.addBreak 1,
                 if isBinder body then ppBindTerm body
                 else ppNormalTerm body]
            end

        and ppBinderTerm (tm,r) =
            if not (isBinder tm) then ppApplicationTerm tm
            else if r then Print.ppBracket "(" ")" ppBindTerm tm
            else ppBindTerm tm

        and ppNegationTerm (tm,r) =
            let
              val (cs,tm) = stripNegation tm
            in
              if List.null cs then ppBinderTerm (tm,r)
              else
                Print.blockProgram Print.Inconsistent 2
                  (List.map ppNegationName cs @
                   [if isInfix tm orelse isCond show tm then ppBracketTerm tm
                    else ppBinderTerm (tm,r)])
            end

        and ppCondTerm (f,c,a,b,r) =
            Print.blockProgram Print.Inconsistent 0
              [Print.blockProgram Print.Consistent 0
                 [Print.blockProgram Print.Inconsistent (if f then 3 else 8)
                    [Print.ppString (if f then "if " else "else if "),
                     ppInfixTerm (c,true)],
                  Print.addBreak 1,
                  Print.ppString "then"],
               Print.blockProgram Print.Inconsistent 2
                 [Print.ppString "",
                  Print.addBreak 1,
                  ppLetCondTerm (a,true)]] ::
            Print.addBreak 1 ::
            (case total (destCond show) b of
               SOME (c,a,b) => ppCondTerm (false,c,a,b,r)
             | NONE =>
                 [Print.blockProgram Print.Inconsistent 2
                    [Print.ppString "else",
                     Print.addBreak 1,
                     ppLetCondTerm (b,r)]])

        and ppLetTerm (v,t,b,r) =
            Print.blockProgram Print.Inconsistent 4
              [Print.ppString "let ",
               ppBasicTerm v,
               Print.ppString " =",
               Print.addBreak 1,
               ppLetCondTerm (t,true),
               Print.ppString " in"] ::
            Print.addBreak 1 ::
            (case total (destLet show) b of
               NONE => [ppLetCondTerm (b,r)]
             | SOME (v,t,b) => ppLetTerm (v,t,b,r))

        and ppLetCondTerm (tm,r) =
            case total (destLet show) tm of
              SOME (v,t,b) =>
                Print.blockProgram Print.Consistent 0
                  (ppLetTerm (v,t,b,r))
            | NONE =>
              case total (destCond show) tm of
                SOME (c,a,b) =>
                Print.blockProgram Print.Consistent 0
                  (ppCondTerm (true,c,a,b,r))
              | NONE => ppInfixTerm (tm,r)

        and ppLetConditionalTerm (tm,r) =
            if not (isLet show tm orelse isCond show tm) then
              ppNegationTerm (tm,r)
            else if r then
              ppBracketTerm tm
            else
              ppLetCondTerm (tm,false)

        and ppInfixTerm tm_r = ppInfix ppLetConditionalTerm tm_r

        and ppNormalTerm tm = ppInfixTerm (tm,false)

        and ppBracketTerm tm = Print.ppBracket "(" ")" ppNormalTerm tm
      in
        ppNormalTerm
      end;
in
  fun ppWithGrammar gram =
      let
        val Grammar
              {negations,infixes,binders,
               showConst,
               ppVar,ppConst,ppNegation,ppInfix,ppBinder,
               maximumSize} = gram

        val ppInfixes = Print.ppInfixes infixes

        val negationNames = mkSet negations
        and infixNames = mkMap (Print.tokensInfixes infixes)
        and binderNames = mkSet binders

        val specialNames =
            NameSet.unionList
              [negationNames, NameSet.domain infixNames, binderNames]
      in
        fn show =>
           let
             val ppConst = ppConst show
             and ppNegation = ppNegation show
             and ppInfix = ppInfix show
             and ppBinder = ppBinder show
             and ppVar = ppVar show
           in
             fn tm =>
                let
                  val n = size tm
                in
                  if n <= maximumSize then
                    ppTerm
                      negationNames infixNames binderNames specialNames
                      showConst
                      ppInfixes ppConst ppNegation ppInfix ppBinder
                      ppVar show tm
                  else
                    Print.ppBracket "term{" "}" Print.ppInt n
                end
           end
      end;
end;

val ppWithShow = ppWithGrammar defaultGrammar;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val ppHtml = ppWithGrammar htmlGrammar;

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

  fun dealphaEqual s1 s2 = dealphaCompare (s1,s2) = EQUAL;

end
