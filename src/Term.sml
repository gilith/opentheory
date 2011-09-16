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

fun destConst tm =
    destConst' (dest tm)
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destConst: " ^ err);
*)

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

fun destVar tm =
    destVar' (dest tm)
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destVar: " ^ err);
*)

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

fun destApp tm =
    destApp' (dest tm)
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destApp: " ^ err);
*)

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

fun destAbs tm =
    destAbs' (dest tm)
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destAbs: " ^ err);
*)

val isAbs = can destAbs;

fun listMkAbs (vs,tm) = List.foldl mkAbs tm (List.rev vs);

local
  fun strip acc tm =
      case total destAbs tm of
        NONE => (List.rev acc, tm)
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

fun checkEqual tm1 tm2 =
    if equal tm1 tm2 then ()
    else raise Error "terms not equal";

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
          val (n1,ty1) = Var.dest v1
          and (n2,ty2) = Var.dest v2
        in
          case Type.compare (ty1,ty2) of
            LESS => LESS
          | EQUAL =>
            let
              val bvEq = bvEq andalso Name.equal n1 n2

              val bv1 = VarMap.insert bv1 (v1,n)

              val bv2 = if bvEq then bv1 else VarMap.insert bv2 (v2,n)

              val n = n + 1
            in
              acmp n bv1 bv2 bvEq (b1,b2)
            end
          | GREATER => GREATER
        end;
in
  val alphaCompare =
      let
        val n = 0
        and bv = VarMap.new ()
        and bvEq = true
      in
        acmp n bv bv bvEq
      end;
end;

fun alphaEqual tm1 tm2 = alphaCompare (tm1,tm2) = EQUAL;

fun checkAlphaEqual tm1 tm2 =
    if alphaEqual tm1 tm2 then ()
    else raise Error "terms not alpha-equivalent";

(* ------------------------------------------------------------------------- *)
(* The type of a term.                                                       *)
(* ------------------------------------------------------------------------- *)

val typeOf = TypeTerm.typeOf;

(* ------------------------------------------------------------------------- *)
(* Checking that a term has type bool.                                       *)
(* ------------------------------------------------------------------------- *)

fun isBool tm = Type.isBool (typeOf tm);

fun checkBool tm =
    if isBool tm then () else raise Error "term is not boolean";

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

fun mkEqConst a =
    let
      val c = Const.eq
      and ty = Type.mkEq a
    in
      mkConst (c,ty)
    end;

fun destEqConst tm =
    let
      val (c,ty) = destConst tm
    in
      if Const.isEq c then Type.destEq ty
      else raise Error "wrong constant"
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destEqConst: " ^ err);
*)

val isEqConst = can destEqConst;

fun mkEq (l,r) =
    let
      val c = mkEqConst (typeOf l)

      val t = mkApp (c,l)
    in
      mkApp (t,r)
    end;

fun destEq tm =
    let
      val (el,r) = destApp tm

      val (e,l) = destApp el
    in
      if isEqConst e then (l,r)
      else raise Error "not equality constant"
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destEq: " ^ err);
*)

val isEq = can destEq;

fun lhs tm = fst (destEq tm);

fun rhs tm = snd (destEq tm);

fun mkRefl tm = mkEq (tm,tm);

fun destRefl tm =
    let
      val (l,r) = destEq tm;

      val () = if equal l r then () else raise Error "lhs <> rhs"
    in
      l
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destRefl: " ^ err);
*)

val isRefl = can destRefl;

(* Hilbert's choice operator *)

fun mkSelectConst a = mkConst (Const.select, Type.mkSelect a);

fun destSelectConst tm =
    let
      val (c,ty) = destConst tm
    in
      if Const.isSelect c then Type.destSelect ty
      else raise Error "wrong constant"
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destSelectConst: " ^ err);
*)

val isSelectConst = can destSelectConst;

fun mkSelect (v_b as (v,_)) =
    let
      val vb = mkAbs v_b

      val c = mkSelectConst (Var.typeOf v)
    in
      mkApp (c,vb)
    end;

fun destSelect tm =
    let
      val (c,vb) = destApp tm
    in
      if isSelectConst c then destAbs vb
      else raise Error "not select constant"
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destSelect: " ^ err);
*)

val isSelect = can destSelect;

(* ------------------------------------------------------------------------- *)
(* Axioms.                                                                   *)
(* ------------------------------------------------------------------------- *)

val axiomOfExtensionality =
    let
      val ty0 = Type.mkVar (Name.mkGlobal "A")
      val ty1 = Type.mkVar (Name.mkGlobal "B")
      val ty2 = Type.mkFun (ty0,ty1)
      val ty3 = Type.bool
      val ty4 = Type.mkFun (ty2,ty3)
      val v0 = Var.mk (Name.mkGlobal "a", ty4)
      val v1 = Var.mk (Name.mkGlobal "b", ty2)
      val v2 = Var.mk (Name.mkGlobal "c", ty3)
      val v3 = Var.mk (Name.mkGlobal "d", ty2)
      val v4 = Var.mk (Name.mkGlobal "e", ty0)
      val tm0 = mkVar v0
      val tm1 = mkVar v2
      val tm2 = mkAbs (v2,tm1)
      val tm3 = mkEq (tm2,tm2)
      val tm4 = mkAbs (v1,tm3)
      val tm5 = mkEq (tm0,tm4)
      val tm6 = mkAbs (v0,tm5)
      val tm7 = mkVar v3
      val tm8 = mkVar v4
      val tm9 = mkApp (tm7,tm8)
      val tm10 = mkAbs (v4,tm9)
      val tm11 = mkEq (tm10,tm7)
      val tm12 = mkAbs (v3,tm11)
      val tm13 = mkApp (tm6,tm12)
    in
      tm13
    end;

val axiomOfChoice =
    let
      val ty0 = Type.mkVar (Name.mkGlobal "A")
      val ty1 = Type.bool
      val ty2 = Type.mkFun (ty0,ty1)
      val ty3 = Type.mkFun (ty2,ty1)
      val ty4 = Type.mkFun (ty1,ty1)
      val ty5 = Type.mkFun (ty1,ty4)
      val v0 = Var.mk (Name.mkGlobal "a", ty3)
      val v1 = Var.mk (Name.mkGlobal "b", ty2)
      val v2 = Var.mk (Name.mkGlobal "c", ty1)
      val v3 = Var.mk (Name.mkGlobal "d", ty2)
      val v4 = Var.mk (Name.mkGlobal "e", ty2)
      val v5 = Var.mk (Name.mkGlobal "f", ty0)
      val v6 = Var.mk (Name.mkGlobal "g", ty0)
      val v7 = Var.mk (Name.mkGlobal "h", ty1)
      val v8 = Var.mk (Name.mkGlobal "i", ty1)
      val v9 = Var.mk (Name.mkGlobal "j", ty1)
      val v10 = Var.mk (Name.mkGlobal "k", ty1)
      val v11 = Var.mk (Name.mkGlobal "l", ty5)
      val v12 = Var.mk (Name.mkGlobal "m", ty5)
      val tm0 = mkVar v0
      val tm1 = mkVar v2
      val tm2 = mkAbs (v2,tm1)
      val tm3 = mkEq (tm2,tm2)
      val tm4 = mkAbs (v1,tm3)
      val tm5 = mkEq (tm0,tm4)
      val tm6 = mkAbs (v0,tm5)
      val tm7 = mkVar v4
      val tm8 = mkAbs (v5,tm3)
      val tm9 = mkEq (tm7,tm8)
      val tm10 = mkAbs (v4,tm9)
      val tm11 = mkVar v11
      val tm12 = mkVar v9
      val tm13 = mkApp (tm11,tm12)
      val tm14 = mkVar v10
      val tm15 = mkApp (tm13,tm14)
      val tm16 = mkAbs (v11,tm15)
      val tm17 = mkVar v12
      val tm18 = mkApp (tm17,tm3)
      val tm19 = mkApp (tm18,tm3)
      val tm20 = mkAbs (v12,tm19)
      val tm21 = mkEq (tm16,tm20)
      val tm22 = mkAbs (v10,tm21)
      val tm23 = mkAbs (v9,tm22)
      val tm24 = mkVar v7
      val tm25 = mkApp (tm23,tm24)
      val tm26 = mkVar v8
      val tm27 = mkApp (tm25,tm26)
      val tm28 = mkEq (tm27,tm24)
      val tm29 = mkAbs (v8,tm28)
      val tm30 = mkAbs (v7,tm29)
      val tm31 = mkVar v3
      val tm32 = mkVar v6
      val tm33 = mkApp (tm31,tm32)
      val tm34 = mkApp (tm30,tm33)
      val tm35 = mkSelectConst ty0
      val tm36 = mkApp (tm35,tm31)
      val tm37 = mkApp (tm31,tm36)
      val tm38 = mkApp (tm34,tm37)
      val tm39 = mkAbs (v6,tm38)
      val tm40 = mkApp (tm10,tm39)
      val tm41 = mkAbs (v3,tm40)
      val tm42 = mkApp (tm6,tm41)
    in
      tm42
    end;

val axiomOfInfinity =
    let
      val ty0 = Type.ind
      val ty1 = Type.mkFun (ty0,ty0)
      val ty2 = Type.bool
      val ty3 = Type.mkFun (ty1,ty2)
      val ty4 = Type.mkFun (ty2,ty2)
      val ty5 = Type.mkFun (ty2,ty4)
      val ty6 = Type.mkFun (ty0,ty2)
      val v0 = Var.mk (Name.mkGlobal "a", ty3)
      val v1 = Var.mk (Name.mkGlobal "b", ty4)
      val v2 = Var.mk (Name.mkGlobal "c", ty2)
      val v3 = Var.mk (Name.mkGlobal "d", ty2)
      val v4 = Var.mk (Name.mkGlobal "e", ty2)
      val v5 = Var.mk (Name.mkGlobal "f", ty2)
      val v6 = Var.mk (Name.mkGlobal "g", ty2)
      val v7 = Var.mk (Name.mkGlobal "h", ty2)
      val v8 = Var.mk (Name.mkGlobal "i", ty2)
      val v9 = Var.mk (Name.mkGlobal "j", ty5)
      val v10 = Var.mk (Name.mkGlobal "k", ty5)
      val v11 = Var.mk (Name.mkGlobal "l", ty3)
      val v12 = Var.mk (Name.mkGlobal "m", ty1)
      val v13 = Var.mk (Name.mkGlobal "n", ty1)
      val v14 = Var.mk (Name.mkGlobal "o", ty1)
      val v15 = Var.mk (Name.mkGlobal "p", ty6)
      val v16 = Var.mk (Name.mkGlobal "q", ty0)
      val v17 = Var.mk (Name.mkGlobal "r", ty0)
      val v18 = Var.mk (Name.mkGlobal "s", ty0)
      val v19 = Var.mk (Name.mkGlobal "t", ty2)
      val v20 = Var.mk (Name.mkGlobal "u", ty0)
      val v21 = Var.mk (Name.mkGlobal "v", ty6)
      val v22 = Var.mk (Name.mkGlobal "w", ty2)
      val v23 = Var.mk (Name.mkGlobal "x", ty0)
      val v24 = Var.mk (Name.mkGlobal "y", ty0)
      val tm0 = mkVar v1
      val tm1 = mkVar v3
      val tm2 = mkAbs (v3,tm1)
      val tm3 = mkEq (tm2,tm2)
      val tm4 = mkAbs (v2,tm3)
      val tm5 = mkEq (tm0,tm4)
      val tm6 = mkAbs (v1,tm5)
      val tm7 = mkVar v9
      val tm8 = mkVar v7
      val tm9 = mkApp (tm7,tm8)
      val tm10 = mkVar v8
      val tm11 = mkApp (tm9,tm10)
      val tm12 = mkAbs (v9,tm11)
      val tm13 = mkVar v10
      val tm14 = mkApp (tm13,tm3)
      val tm15 = mkApp (tm14,tm3)
      val tm16 = mkAbs (v10,tm15)
      val tm17 = mkEq (tm12,tm16)
      val tm18 = mkAbs (v8,tm17)
      val tm19 = mkAbs (v7,tm18)
      val tm20 = mkVar v5
      val tm21 = mkApp (tm19,tm20)
      val tm22 = mkVar v6
      val tm23 = mkApp (tm21,tm22)
      val tm24 = mkEq (tm23,tm20)
      val tm25 = mkAbs (v6,tm24)
      val tm26 = mkAbs (v5,tm25)
      val tm27 = mkVar v11
      val tm28 = mkAbs (v12,tm3)
      val tm29 = mkEq (tm27,tm28)
      val tm30 = mkAbs (v11,tm29)
      val tm31 = mkVar v0
      val tm32 = mkVar v13
      val tm33 = mkApp (tm31,tm32)
      val tm34 = mkApp (tm26,tm33)
      val tm35 = mkVar v4
      val tm36 = mkApp (tm34,tm35)
      val tm37 = mkAbs (v13,tm36)
      val tm38 = mkApp (tm30,tm37)
      val tm39 = mkApp (tm26,tm38)
      val tm40 = mkApp (tm39,tm35)
      val tm41 = mkAbs (v4,tm40)
      val tm42 = mkApp (tm6,tm41)
      val tm43 = mkAbs (v0,tm42)
      val tm44 = mkVar v15
      val tm45 = mkAbs (v16,tm3)
      val tm46 = mkEq (tm44,tm45)
      val tm47 = mkAbs (v15,tm46)
      val tm48 = mkVar v14
      val tm49 = mkVar v17
      val tm50 = mkApp (tm48,tm49)
      val tm51 = mkVar v18
      val tm52 = mkApp (tm48,tm51)
      val tm53 = mkEq (tm50,tm52)
      val tm54 = mkApp (tm26,tm53)
      val tm55 = mkEq (tm49,tm51)
      val tm56 = mkApp (tm54,tm55)
      val tm57 = mkAbs (v18,tm56)
      val tm58 = mkApp (tm47,tm57)
      val tm59 = mkAbs (v17,tm58)
      val tm60 = mkApp (tm47,tm59)
      val tm61 = mkApp (tm19,tm60)
      val tm62 = mkVar v19
      val tm63 = mkApp (tm26,tm62)
      val tm64 = mkApp (tm6,tm2)
      val tm65 = mkApp (tm63,tm64)
      val tm66 = mkAbs (v19,tm65)
      val tm67 = mkVar v21
      val tm68 = mkVar v23
      val tm69 = mkApp (tm67,tm68)
      val tm70 = mkApp (tm26,tm69)
      val tm71 = mkVar v22
      val tm72 = mkApp (tm70,tm71)
      val tm73 = mkAbs (v23,tm72)
      val tm74 = mkApp (tm47,tm73)
      val tm75 = mkApp (tm26,tm74)
      val tm76 = mkApp (tm75,tm71)
      val tm77 = mkAbs (v22,tm76)
      val tm78 = mkApp (tm6,tm77)
      val tm79 = mkAbs (v21,tm78)
      val tm80 = mkVar v20
      val tm81 = mkVar v24
      val tm82 = mkApp (tm48,tm81)
      val tm83 = mkEq (tm80,tm82)
      val tm84 = mkAbs (v24,tm83)
      val tm85 = mkApp (tm79,tm84)
      val tm86 = mkAbs (v20,tm85)
      val tm87 = mkApp (tm47,tm86)
      val tm88 = mkApp (tm66,tm87)
      val tm89 = mkApp (tm61,tm88)
      val tm90 = mkAbs (v14,tm89)
      val tm91 = mkApp (tm43,tm90)
    in
      tm91
    end;

(* ------------------------------------------------------------------------- *)
(* General syntax operations.                                                *)
(* ------------------------------------------------------------------------- *)

(* Nullary operators *)

fun destNullaryOp p tm =
    let
      val (c,_) = destConst tm
    in
      if p c then ()
      else raise Error "wrong constant"
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destNullaryOp: " ^ err);
*)

fun isNullaryOp p = can (destNullaryOp p);

(* Unary operators *)

fun destUnaryOp p tm =
    let
      val (c,a) = destApp tm

      val () = destNullaryOp p c
    in
      a
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destUnaryOp: " ^ err);
*)

(* Binary operators *)

fun destBinaryOp p tm =
    let
      val (ca,b) = destApp tm

      val a = destUnaryOp p ca
    in
      (a,b)
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destBinaryOp: " ^ err);
*)

fun stripBinaryOp p =
    let
      fun strip tms tm =
          case total (destBinaryOp p) tm of
            NONE => (tms, tm)
          | SOME (a,tm) => strip (a :: tms) tm
    in
      strip []
    end;

(* Ternary operators *)

fun destTernaryOp p tm =
    let
      val (cab,c) = destApp tm

      val (a,b) = destBinaryOp p cab
    in
      (a,b,c)
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destTernaryOp: " ^ err);
*)

(* Quantifiers *)

fun destQuant p tm =
    let
      val f = destUnaryOp p tm
    in
      destAbs f
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destQuant: " ^ err);
*)

fun stripQuant p =
    let
      fun strip vs tm =
          case total (destQuant p) tm of
            NONE => (vs,tm)
          | SOME (v,b) => strip (v :: vs) b
    in
      strip []
    end;

(* ------------------------------------------------------------------------- *)
(* Boolean syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Truth *)

val isTrue = isNullaryOp Const.isTrue;

(* Falsity *)

val isFalse = isNullaryOp Const.isFalse;

(* Negation *)

val isNegConst = isNullaryOp Const.isNeg;

fun destNeg tm =
    destUnaryOp Const.isNeg tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destNeg: " ^ err);
*)

val isNeg = can destNeg;

(* Conjunction *)

val isConjConst = isNullaryOp Const.isConj;

fun destConj tm =
    destBinaryOp Const.isConj tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destConj: " ^ err);
*)

val isConj = can destConj;

fun stripConj tm =
    if isTrue tm then []
    else
      let
        val (tms,tm) = stripBinaryOp Const.isConj tm
      in
        List.revAppend (tms,[tm])
      end;

(* Disjunction *)

val isDisjConst = isNullaryOp Const.isDisj;

fun destDisj tm =
    destBinaryOp Const.isDisj tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destDisj: " ^ err);
*)

val isDisj = can destDisj;

fun stripDisj tm =
    if isFalse tm then []
    else
      let
        val (tms,tm) = stripBinaryOp Const.isDisj tm
      in
        List.revAppend (tms,[tm])
      end;

(* Implication *)

val isImpConst = isNullaryOp Const.isImp;

fun destImp tm =
    destBinaryOp Const.isImp tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destImp: " ^ err);
*)

val isImp = can destImp;

fun stripImp tm =
    let
      val (tms,tm) = stripBinaryOp Const.isImp tm
    in
      (List.rev tms, tm)
    end;

(* Universal *)

val isForallConst = isNullaryOp Const.isForall;

fun destForall tm =
    destQuant Const.isForall tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destForall: " ^ err);
*)

val isForall = can destForall;

fun stripForall tm =
    let
      val (vs,tm) = stripQuant Const.isForall tm
    in
      (List.rev vs, tm)
    end;

(* Existence *)

val isExistsConst = isNullaryOp Const.isExists;

fun destExists tm =
    destQuant Const.isExists tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destExists: " ^ err);
*)

val isExists = can destExists;

fun stripExists tm =
    let
      val (vs,tm) = stripQuant Const.isExists tm
    in
      (List.rev vs, tm)
    end;

(* Unique existence *)

val isExistsUniqueConst = isNullaryOp Const.isExistsUnique;

fun destExistsUnique tm =
    destQuant Const.isExistsUnique tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destExistsUnique: " ^ err);
*)

val isExistsUnique = can destExistsUnique;

fun stripExistsUnique tm =
    let
      val (vs,tm) = stripQuant Const.isExistsUnique tm
    in
      (List.rev vs, tm)
    end;

(* Conditional *)

val isCondConst = isNullaryOp Const.isCond;

fun destCond tm =
    destTernaryOp Const.isCond tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destCond: " ^ err);
*)

val isCond = can destCond;

(* Generalized abstractions *)

fun destGenAbs tm =
    case total destAbs tm of
      SOME (v,t) => (mkVar v, t)
    | NONE =>
      let
        val (f,tm) = destSelect tm

        val (vl,tm) = stripForall tm

        val () =
            if not (List.exists (Var.equal f) vl) then ()
            else raise Error "function is var"

        val (pat,body) = destEq tm

        val (ft,pat) = destApp pat

        val () =
            if equalVar f ft then ()
            else raise Error "no function"

        val () =
            if Var.listEqual (VarSet.toList (freeVars pat)) vl then ()
            else raise Error "bad pattern var list"

        val () =
            if not (VarSet.member f (freeVars body)) then ()
            else raise Error "function in body"
      in
        (pat,body)
      end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destGenAbs: " ^ err);
*)

val isGenAbs = can destGenAbs;

local
  fun strip acc tm =
      case total destGenAbs tm of
        NONE => (List.rev acc, tm)
      | SOME (v,tm) => strip (v :: acc) tm;
in
  val stripGenAbs = strip [];
end;

(* Let bindings *)

local
  fun transfer v t =
      case total destGenAbs t of
        NONE => (v,t)
      | SOME (w,t) => transfer (mkApp (v,w)) t;
in
  fun destLet tm =
      let
        val (vb,t) = destApp tm

        val (v,b) = destGenAbs vb

        val (v,t) = transfer v t
      in
        (v,t,b)
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("Term.destLet: " ^ err);
*)
end;

val isLet = can destLet;

(* Numerals *)

fun destFromNatural tm =
    let
      val (f,t) = destApp tm

      val (c,_) = destConst f
    in
      if Const.isFromNatural c then t
      else raise Error "Term.destFromNatural"
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destFromNatural: " ^ err);
*)

local
  fun destNum tm =
      case dest tm of
        TypeTerm.Const' (c,_) =>
        if Const.isZero c then 0
        else raise Error "Term.destNumeral: bad const"
      | TypeTerm.App' (f,x) =>
        let
          val (c,_) = destConst f
        in
          if Const.isBit0 c then
            let
              val i = destNum x
            in
              if i > 0 then 2 * i
              else raise Error "Term.destNumeral: bit0 zero"
            end
          else if Const.isBit1 c then
            let
              val i = destNum x
            in
              2 * i + 1
            end
          else
            raise Error "Term.destNumeral: bad app"
        end
      | _ => raise Error "Term.destNumeral: bad term";
in
  fun destNumeral tm =
      let
        val tm =
            case total destFromNatural tm of
              SOME t => t
            | NONE => tm
      in
        destNum tm
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("Term.destNumeral: " ^ err);
*)
end

val isNumeral = can destNumeral;

(* Set comprehensions *)

fun destFromPredicate tm =
    destUnaryOp Const.isFromPredicate tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destFromPredicate: " ^ err);
*)

fun destComprehension tm =
    let
      val tm = destFromPredicate tm

      val (v,tm) = destAbs tm

      val (vl,tm) = stripExists tm

      val vs = VarSet.fromList vl

      val () =
          if length vl = VarSet.size vs then ()
          else raise Error "duplicate vars"

      val () =
          if not (VarSet.member v vs) then ()
          else raise Error "var capture"

      val (tm,pred) = destConj tm

      val (v',pat) = destEq tm

      val () =
          if equalVar v v' then ()
          else raise Error "missing var"

      val fvs = freeVars pat

      val () =
          if not (VarSet.member v fvs) then ()
          else raise Error "var in pat"

      val () =
          if not (VarSet.member v (freeVars pred)) then ()
          else raise Error "var in pred"

      val () =
          if VarSet.subset vs fvs then ()
          else raise Error "unused pat var"
    in
      case vl of
        [] => raise Error "no pat vars"
      | v :: vl => (v,vl,pat,pred)
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Term.destComprehension: " ^ err);
*)

val isComprehension = can destComprehension;

(* Case expressions *)

local
  fun mkBranch ty n =
      let
        fun strip vs tm =
            if Type.equal (typeOf tm) ty then (n, List.rev vs, tm)
            else
              let
                val (v,tm) = destGenAbs tm
              in
                strip (v :: vs) tm
              end
      in
        strip []
      end;

  fun mkBranches ty =
      let
        fun zipf bs ns args =
            case args of
              [] => raise Bug "Term.destCase.mkBranches: args too short"
            | arg :: args =>
              case ns of
                [] =>
                if List.null args then (arg, List.rev bs)
                else raise Bug "Term.destCase.mkBranches: args too long"
              | n :: ns =>
                let
                  val b = mkBranch ty n arg
                in
                  zipf (b :: bs) ns args
                end
      in
        zipf []
      end;
in
  fun destCase tm =
      let
        val (c,args) = stripApp tm

        val (c,_) = destConst c

        val (_,ns) = Name.destCase (Const.name c)

        val () =
            if length args = length ns + 1 then ()
            else raise Error "Term.destCase: wrong number of args"
      in
        mkBranches (typeOf tm) ns args
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("Term.destCase: " ^ err);
*)
end;

val isCase = can destCase;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {negations : Print.token list,
       infixes : Print.infixes,
       binders : Print.token list,
       showConst : Show.show -> Const.const * Type.ty -> Name.name,
       ppSyntax : string Print.pp,
       ppVar : Show.show -> Var.var Print.pp,
       ppConst : Show.show -> (Const.const * Type.ty) Print.pp,
       ppNegation : Show.show -> (Const.const * Type.ty) Print.pp,
       ppInfix : Show.show -> (Const.const * Type.ty) Print.pp,
       ppBinder : Show.show -> (Const.const * Type.ty) option Print.pp,
       ppNumeral : Show.show -> (int * Type.ty) Print.pp,
       maximumSize : int};

local
  fun mkName s = Name.mkGlobal s
  and destName n = snd (Name.dest n);

  val stringAbs = "\\"
  and stringBoolEq = "<=>"
  and stringBoolNeg = "\\lnot"
  and stringConj = destName Name.conjConst
  and stringDisj = destName Name.disjConst
  and stringEq = destName Name.eqConst
  and stringExists = destName Name.existsConst
  and stringExistsUnique = destName Name.existsUniqueConst
  and stringForall = destName Name.forallConst
  and stringImp = destName Name.impConst
  and stringNeg = destName Name.negConst
  and stringPair = ","
  and stringSelect = destName Name.selectConst;

  val negations = [stringNeg];

  val infixes =
      Print.Infixes
        [(* ML *)
         {token = "/", precedence = 7, assoc = Print.LeftAssoc},
         {token = "div", precedence = 7, assoc = Print.LeftAssoc},
         {token = "mod", precedence = 7, assoc = Print.LeftAssoc},
         {token = "*", precedence = 7, assoc = Print.LeftAssoc},
         {token = "+", precedence = 6, assoc = Print.LeftAssoc},
         {token = "-", precedence = 6, assoc = Print.LeftAssoc},
         {token = "^", precedence = 6, assoc = Print.LeftAssoc},
         {token = "@", precedence = 5, assoc = Print.RightAssoc},
         {token = "::", precedence = 5, assoc = Print.RightAssoc},
         {token = stringEq, precedence = 4, assoc = Print.NonAssoc},
         {token = "<>", precedence = 4, assoc = Print.NonAssoc},
         {token = "<=", precedence = 4, assoc = Print.NonAssoc},
         {token = "<", precedence = 4, assoc = Print.NonAssoc},
         {token = ">=", precedence = 4, assoc = Print.NonAssoc},
         {token = ">", precedence = 4, assoc = Print.NonAssoc},
         {token = "o", precedence = 3, assoc = Print.LeftAssoc},
         (* Set theory *)
         {token = "intersect", precedence = 7, assoc = Print.LeftAssoc},
         {token = "difference", precedence = 6, assoc = Print.LeftAssoc},
         {token = "union", precedence = 6, assoc = Print.LeftAssoc},
         {token = "subset", precedence = 4, assoc = Print.NonAssoc},
         {token = "properSubset", precedence = 4, assoc = Print.NonAssoc},
         {token = "member", precedence = 4, assoc = Print.NonAssoc},
         (* HOL *)
         {token = stringConj, precedence = ~1, assoc = Print.RightAssoc},
         {token = stringDisj, precedence = ~2, assoc = Print.RightAssoc},
         {token = stringImp, precedence = ~3, assoc = Print.RightAssoc},
         {token = stringBoolEq, precedence = ~4, assoc = Print.RightAssoc},
         {token = stringPair, precedence = ~1000, assoc = Print.RightAssoc}];

  val binders =
      [stringForall,stringExists,stringExistsUnique,stringSelect,"minimal"];

  local
    val nameBoolEq = mkName stringBoolEq;
  in
    fun showConst show (c,ty) =
        if Const.isEq c then
          if Type.isBoolEq ty then nameBoolEq else Name.eqConst
        else
          Show.showName show (Const.name c);
  end

  local
    val pairName = mkName stringPair;
  in
    fun ppInfixBuffer ppInf c_n =
        let
          val (_,n) = c_n

          val pps = [ppInf c_n, Print.break]

          val pps = if Name.equal n pairName then pps else Print.space :: pps
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

          val pps = if isAlpha c_n then Print.break :: pps else pps

          val pps = ppBind c_n :: pps
        in
          Print.program pps
        end;
  end;

  val ppSyntax = Print.ppMap mkName Name.pp;

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
             NONE => Print.ppString stringAbs
            | SOME (_,n) => Name.pp n
      in
        Print.ppMap toName (ppBinderBuffer ppBind)
      end;

  fun ppNumeral _ (i,_) = Print.ppInt i;

  val ppSyntaxHtml =
      let
        fun toHtml s = Name.toHtml (mkName s)
      in
        Print.ppMap toHtml Html.ppFixed
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

    local
      val nameNeg = mkName stringNeg
      and nameBoolNeg = mkName stringBoolNeg
      and tyBoolNeg = Type.mkFun (Type.bool,Type.bool);

      fun isBoolNeg n ty =
          Name.equal n nameNeg andalso Type.equal ty tyBoolNeg;
    in
      fun toHtmlNegation show =
          let
            val toHtml = Const.toHtml show
          in
            fn (c,ty) =>
               let
                 val n = showConst show (c,ty)

                 val n = if isBoolNeg n ty then nameBoolNeg else n
             in
               mkSpan "negation" (toHtml ((c, SOME ty), n))
             end
          end;
    end;

    fun toHtmlInfix show =
        let
          val toHtml = Const.toHtml show
        in
          fn ((c,ty),n) => mkSpan "infix" (toHtml ((c, SOME ty), n))
        end;

    local
      val nameAbs = mkName stringAbs;

      val htmlAbs = Name.toHtml nameAbs;
    in
      fun toHtmlBinder show =
          let
            val toHtml = Const.toHtml show
          in
            fn ctyno =>
               let
                 val h =
                     case ctyno of
                       SOME ((c,ty),n) => toHtml ((c, SOME ty), n)
                     | NONE => htmlAbs
               in
                 mkSpan "binder" h
               end
          end;
    end;

    fun toHtmlNumeral show =
        let
          val ppTy = Type.ppHtml show
        in
          fn (i,ty) =>
             let
               val s = Print.toLine Print.ppInt i

               val class = "numeral"

               val title = s ^ " : " ^ Print.toLine ppTy ty

               val attrs = Html.fromListAttrs [("class",class),("title",title)]

               val inlines = [Html.Text s]
             in
               [Html.Span (attrs,inlines)]
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

    fun ppNumeralHtml show = Print.ppMap (toHtmlNumeral show) Html.ppFixed;
  end;

  val maximumSize = 10000;
in
  val defaultGrammar =
      Grammar
        {negations = negations,
         infixes = infixes,
         binders = binders,
         showConst = showConst,
         ppSyntax = ppSyntax,
         ppVar = ppVar,
         ppConst = ppConst,
         ppNegation = ppNegation,
         ppInfix = ppInfix,
         ppBinder = ppBinder,
         ppNumeral = ppNumeral,
         maximumSize = maximumSize};

  val htmlGrammar =
      Grammar
        {negations = negations,
         infixes = infixes,
         binders = binders,
         showConst = showConst,
         ppSyntax = ppSyntaxHtml,
         ppVar = ppVarHtml,
         ppConst = ppConstHtml,
         ppNegation = ppNegationHtml,
         ppInfix = ppInfixHtml,
         ppBinder = ppBinderHtml,
         ppNumeral = ppNumeralHtml,
         maximumSize = maximumSize};
end;

local
  val mkName = Name.mkGlobal;

  val mkMap =
      let
        fun add (s,m) =
            let
              val n = mkName s
            in
              case NameMap.peek m n of
                NONE => NameMap.insert m (n,s)
              | SOME s' =>
                let
                  val err =
                      "Term.pp.mkMap: name clash: \"" ^
                      s ^ "\" and \"" ^ s' ^ "\""
                in
                  raise Error err
                end
            end

        val emptyMap : Print.token NameMap.map = NameMap.new ();
      in
        StringSet.foldl add emptyMap
      end;

  fun mkSet s = NameSet.domain (mkMap (StringSet.fromList s));

  fun isLetCondCase tm = isLet tm orelse isCond tm orelse isCase tm;

  fun ppTerm negationNames infixNames binderNames specialNames showConst
             ppInfixes ppConstName ppNegationName ppInfixName ppBinderName
             ppNumeral ppVar ppSyntax show =
      let
        fun ppBracket ppA a =
            Print.inconsistentBlock 1
              [ppSyntax "(",
               ppA a,
               ppSyntax ")"]

        fun ppConst c_ty =
            let
              val n = showConst show c_ty
            in
              if NameSet.member n specialNames then ppBracket ppConstName c_ty
              else ppConstName c_ty
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
            case total destGenAbs tm of
              SOME (v,t) => (NONE,v,t)
            | NONE =>
              let
                val (t,a) = destApp tm

                val (v,b) = destGenAbs a

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
              (c, v, List.rev vs, b)
            end

        fun destGenApp tm =
            if isNumeral tm then
              raise Error "Term.pp.destGenApp: numeral"
            else if isCond tm then
              raise Error "Term.pp.destGenApp: cond"
            else if isLet tm then
              raise Error "Term.pp.destGenApp: let"
            else if isComprehension tm then
              raise Error "Term.pp.destGenApp: comprehension"
            else if isInfix tm then
              raise Error "Term.pp.destGenApp: infix"
            else if isNegation tm then
              raise Error "Term.pp.destGenApp: negation"
            else if isBinder tm then
              raise Error "Term.pp.destGenApp: binder"
            else if isCase tm then
              raise Error "Term.pp.destGenApp: case"
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
            case total destNumeral tm of
              SOME i => ppNumeral (i, typeOf tm)
            | NONE =>
              case total destComprehension tm of
                SOME v_vs_pat_pred => ppComprehension v_vs_pat_pred
              | NONE =>
                case dest tm of
                  TypeTerm.Var' v => ppVar v
                | TypeTerm.Const' c_ty => ppConst c_ty
                | TypeTerm.App' _ => ppBracketTerm tm
                | TypeTerm.Abs' _ => ppBracketTerm tm

        and ppApplicationTerm tm =
            let
              val b = Print.Break {size = 1, extraIndent = 2}

              fun ppArg x = Print.sequence (Print.ppBreak b) (ppBasicTerm x)

              val (tm,xs) = stripGenApp tm
            in
              if List.null xs then ppBasicTerm tm
              else
                Print.inconsistentBlock 0
                  (ppBasicTerm tm :: List.map ppArg xs)
            end

        and ppBoundVars (v,vs) =
            Print.program
              (ppBasicTerm v ::
               List.map (Print.sequence Print.break o ppBasicTerm) vs @
               [ppSyntax "."])

        and ppComprehension (v,vs,pat,pred) =
            Print.inconsistentBlock 2
              [ppSyntax "{",
               Print.space,
               ppBoundVars (mkVar v, List.map mkVar vs),
               Print.break,
               ppInfixTerm (pat,true),
               Print.space,
               ppSyntax "|",
               Print.break,
               ppNormalTerm pred,
               Print.space,
               ppSyntax "}"]

        and ppBindTerm tm =
            let
              val (c,v,vs,body) = stripBinder tm
            in
              Print.inconsistentBlock 2
                [ppBinderName c,
                 ppBoundVars (v,vs),
                 Print.break,
                 if isBinder body then ppBindTerm body
                 else ppNormalTerm body]
            end

        and ppBinderTerm (tm,r) =
            if not (isBinder tm) then ppApplicationTerm tm
            else if r then ppBracket ppBindTerm tm
            else ppBindTerm tm

        and ppNegationTerm (tm,r) =
            let
              val (cs,tm) = stripNegation tm
            in
              if List.null cs then ppBinderTerm (tm,r)
              else
                Print.inconsistentBlock 2
                  (List.map ppNegationName cs @
                   [if isInfix tm orelse isCond tm then ppBracketTerm tm
                    else ppBinderTerm (tm,r)])
            end

        and ppLetTerm (v,t,b,r) =
            let
              val ppLetBind =
                  Print.inconsistentBlock 4
                    [ppSyntax "let",
                     Print.space,
                     ppApplicationTerm v,
                     Print.space,
                     ppSyntax "<-",
                     Print.break,
                     ppLetCondCaseNestedTerm (t,true),
                     Print.space,
                     ppSyntax "in"]

              val ppLetBody =
                  case total destLet b of
                    NONE => [ppLetCondCaseNestedTerm (b,r)]
                  | SOME (v,t,b) => ppLetTerm (v,t,b,r)
            in
              ppLetBind ::
              Print.break ::
              ppLetBody
            end

        and ppCondTerm (f,c,a,b,r) =
            let
              val ppCond = ppInfixTerm (c,true)

              val ppIfCond =
                  if f then
                    Print.inconsistentBlock 3
                      [ppSyntax "if",
                       Print.space,
                       ppCond]
                  else
                    Print.inconsistentBlock 8
                      [ppSyntax "else",
                       Print.space,
                       ppSyntax "if",
                       Print.space,
                       ppCond]

              val ppIfCondThen =
                  Print.consistentBlock 0
                    [ppIfCond,
                     Print.break,
                     ppSyntax "then"]

              val ppIfCondThenBranch =
                  Print.inconsistentBlock 0
                    [ppIfCondThen,
                     Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
                     ppLetCondCaseNestedTerm (a,true)]

              val ppElseBranch =
                  case total destCond b of
                    SOME (c,a,b) => ppCondTerm (false,c,a,b,r)
                  | NONE =>
                    [Print.inconsistentBlock 2
                       [ppSyntax "else",
                        Print.break,
                        ppLetCondCaseNestedTerm (b,r)]]
            in
              ppIfCondThenBranch ::
              Print.break ::
              ppElseBranch
            end

        and ppCaseTerm (a,bs,r) =
            let
              fun ppBranch (pat,t_r) =
                  Print.consistentBlock 2
                    [ppApplicationTerm pat,
                     Print.space,
                     ppSyntax "->",
                     Print.break,
                     ppLetCondCaseNestedTerm t_r]

              val ppDecl =
                  Print.consistentBlock 5
                    [ppSyntax "case",
                     Print.space,
                     ppInfixTerm (a,true),
                     Print.space,
                     ppSyntax "of"]

              fun ppDeclAlternative br =
                  Print.program
                    [ppDecl,
                     Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
                     ppBranch br]

              fun ppAlternative br =
                  Print.sequence
                    Print.break
                    (Print.inconsistentBlock 2
                       [ppSyntax "|",
                        Print.space,
                        ppBranch br])

              val (br,brs) =
                  let
                    val ty = typeOf a

                    fun mkBranch (n,xs,t) =
                        let
                          val c = Const.mkUndef n

                          val ty = Type.listMkFun (List.map typeOf xs, ty)

                          val pat = listMkApp (mkConst (c,ty), xs)
                        in
                          (pat,(t,true))
                        end
                  in
                    case List.rev (List.map mkBranch bs) of
                      [] => raise Bug "Term.pp.ppCaseTerm: no branches"
                    | (pat,(t,_)) :: rest =>
                      case List.rev ((pat,(t,r)) :: rest) of
                        [] => raise Bug "Term.pp.ppCaseTerm: no branches II"
                      | br :: brs => (br,brs)
                  end
            in
              Print.consistentBlock 0
                (ppDeclAlternative br :: List.map ppAlternative brs)
            end

        and ppLetCondCaseNestedTerm (tm,r) =
            case total destLet tm of
              SOME (v,t,b) =>
                Print.consistentBlock 0 (ppLetTerm (v,t,b,r))
            | NONE =>
              case total destCond tm of
                SOME (c,a,b) =>
                Print.consistentBlock 0 (ppCondTerm (true,c,a,b,r))
              | NONE =>
                case total destCase tm of
                  SOME (a,bs) => ppCaseTerm (a,bs,r)
                | NONE => ppInfixTerm (tm,r)

        and ppLetCondCaseTerm (tm,r) =
            if not (isLetCondCase tm) then ppNegationTerm (tm,r)
            else if r then ppBracketTerm tm
            else ppLetCondCaseNestedTerm (tm,false)

        and ppInfixTerm tm_r = ppInfix ppLetCondCaseTerm tm_r

        and ppNormalTerm tm = ppInfixTerm (tm,false)

        and ppBracketTerm tm = ppBracket ppNormalTerm tm
      in
        ppNormalTerm
      end;
in
  fun ppWithGrammar gram =
      let
        val Grammar
              {negations,infixes,binders,
               showConst,
               ppSyntax,ppVar,ppConst,ppNegation,ppInfix,ppBinder,ppNumeral,
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
             and ppNumeral = ppNumeral show
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
                      ppNumeral ppVar ppSyntax show tm
                  else
                    let
                      val () = warn "term too large to print"
                    in
                      Print.inconsistentBlock 0
                        [ppSyntax "term{",
                         Print.ppInt n,
                         ppSyntax "}"]
                    end
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

(* ------------------------------------------------------------------------- *)
(* Debugging.                                                                *)
(* ------------------------------------------------------------------------- *)

(*OpenTheoryDebug
local
  fun ppPath path = Print.program (List.map Print.ppInt (List.rev path));

  fun ppTerms ppIntro (tm1,tm2) =
      Print.consistentBlock 2
        [ppIntro,
         Print.ppBreak (Print.Break {size = 1, extraIndent = 3}),
         pp tm1,
         Print.break,
         Print.consistentBlock 3
           [Print.ppString "vs",
            Print.space,
            pp tm2]];

  fun ppComplaint (err,path,tm1,tm2) =
      if List.null path then
        Print.consistentBlock 0
          [Print.ppString err,
           Print.ppString " at term root"]
      else
        let
          val ppIntro =
              Print.consistentBlock 0
                [Print.ppString err,
                 Print.ppString " at path ",
                 ppPath path,
                 Print.ppString " subterms:"]
        in
          ppTerms ppIntro (tm1,tm2)
        end;

  fun complain err path tm1 tm2 =
      Print.toString ppComplaint (err,path,tm1,tm2);

  fun checkRoot tm1 tm2 =
      let
        val path = []
      in
        check path tm1 tm2
      end
      handle Error err =>
        let
          val ppTms = ppTerms (Print.ppString "different terms:")
        in
          raise Error (Print.toString ppTms (tm1,tm2) ^ "\n" ^ err)
        end

  and check path tm1 tm2 =
      case (dest tm1, dest tm2) of
        (TypeTerm.Const' (c1,ty1), TypeTerm.Const' (c2,ty2)) =>
        let
          val () =
              Const.checkEqual checkRoot c1 c2
              handle Error err =>
                let
                  val err =
                      complain "different constants" path tm1 tm2 ^
                      "\n" ^ err
                in
                  raise Error err
                end

          val () =
              Type.checkEqual checkRoot ty1 ty2
              handle Error err =>
                let
                  val err =
                      complain "different constant types" path tm1 tm2 ^
                      "\n" ^ err
                in
                  raise Error err
                end
        in
          ()
        end
      | (TypeTerm.Const' _, _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (_, TypeTerm.Const' _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (TypeTerm.Var' v1, TypeTerm.Var' v2) =>
        let
          val () =
              Var.checkEqual checkRoot v1 v2
              handle Error err =>
                let
                  val err =
                      complain "different variables" path tm1 tm2 ^
                      "\n" ^ err
                in
                  raise Error err
                end
        in
          ()
        end
      | (TypeTerm.Var' _, _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (_, TypeTerm.Var' _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (TypeTerm.App' (f1,a1), TypeTerm.App' (f2,a2)) =>
        let
          val () = check (0 :: path) f1 f2

          val () = check (1 :: path) a1 a2
        in
          ()
        end
      | (TypeTerm.App' _, _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (_, TypeTerm.App' _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (TypeTerm.Abs' (v1,b1), TypeTerm.Abs' (v2,b2)) =>
        let
          val () =
              Var.checkEqual checkRoot v1 v2
              handle Error err =>
                let
                  val err =
                      complain "different bound variables" path tm1 tm2 ^
                      "\n" ^ err
                in
                  raise Error err
                end

          val path = 1 :: path
        in
          check path b1 b2
        end;

  fun checkAlpha path bv bv1 bv2 tm1 tm2 =
      case (dest tm1, dest tm2) of
        (TypeTerm.Const' (c1,ty1), TypeTerm.Const' (c2,ty2)) =>
        let
          val () =
              Const.checkEqual checkRoot c1 c2
              handle Error err =>
                let
                  val err =
                      complain "different constants" path tm1 tm2 ^
                      "\n" ^ err
                in
                  raise Error err
                end

          val () =
              Type.checkEqual checkRoot ty1 ty2
              handle Error err =>
                let
                  val err =
                      complain "different constant types" path tm1 tm2 ^
                      "\n" ^ err
                in
                  raise Error err
                end
        in
          ()
        end
      | (TypeTerm.Const' _, _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (_, TypeTerm.Const' _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (TypeTerm.Var' v1, TypeTerm.Var' v2) =>
        (case (VarMap.peek bv1 v1, VarMap.peek bv2 v2) of
           (NONE,NONE) =>
           let
             val () =
                 Var.checkEqual checkRoot v1 v2
                 handle Error err =>
                   let
                     val err =
                         complain "different variables" path tm1 tm2 ^
                         "\n" ^ err
                   in
                     raise Error err
                   end
           in
             ()
           end
         | (SOME _, NONE) =>
           raise Error (complain "bound vs free variable" path tm1 tm2)
         | (NONE, SOME _) =>
           raise Error (complain "free vs bound variable" path tm1 tm2)
         | (SOME i1, SOME i2) =>
           if i1 = i2 then ()
           else raise Error (complain "different lambda binding" path tm1 tm2))
      | (TypeTerm.Var' _, _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (_, TypeTerm.Var' _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (TypeTerm.App' (f1,a1), TypeTerm.App' (f2,a2)) =>
        let
          val () = checkAlpha (0 :: path) bv bv1 bv2 f1 f2

          val () = checkAlpha (1 :: path) bv bv1 bv2 a1 a2
        in
          ()
        end
      | (TypeTerm.App' _, _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (_, TypeTerm.App' _) =>
        raise Error (complain "different term structure" path tm1 tm2)
      | (TypeTerm.Abs' (v1,b1), TypeTerm.Abs' (v2,b2)) =>
        let
          val ty1 = Var.typeOf v1
          and ty2 = Var.typeOf v2

          val () =
              Type.checkEqual checkRoot ty1 ty2
              handle Error err =>
                let
                  val err =
                      complain "different bound variable types" path tm1 tm2 ^
                      "\n" ^ err
                in
                  raise Error err
                end

          val bv1 = VarMap.insert bv1 (v1,bv)

          val bv2 = VarMap.insert bv2 (v2,bv)

          val bv = bv + 1

          val path = 1 :: path
        in
          checkAlpha path bv bv1 bv2 b1 b2
        end;

  fun checkAlphaRoot tm1 tm2 =
      let
        val path = []
        and n = 0
        and bv = VarMap.new ()
      in
        checkAlpha path n bv bv tm1 tm2
      end
      handle Error err =>
        let
          val ppTms = ppTerms (Print.ppString "terms not alpha-equivalent:")
        in
          raise Error (Print.toString ppTms (tm1,tm2) ^ "\n" ^ err)
        end;
in
  val checkEqual = fn tm1 => fn tm2 =>
      let
        val () =
            checkEqual tm1 tm2
            handle Error _ =>
              let
                val () = checkRoot tm1 tm2
              in
                raise Bug "Term.checkEqual failed but debug version succeeded"
              end
      in
        ()
      end;

  val checkAlphaEqual = fn tm1 => fn tm2 =>
      let
        val () =
            checkAlphaEqual tm1 tm2
            handle Error _ =>
              let
                val () = checkAlphaRoot tm1 tm2
              in
                raise Bug
                  "Term.checkAlphaEqual failed but debug version succeeded"
              end
      in
        ()
      end;
end;
*)

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

  val isBool = all Term.isBool;

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
