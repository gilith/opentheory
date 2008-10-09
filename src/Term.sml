(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Term :> Term =
struct

open Useful;

infixr ==

val op== = Portable.pointerEqual;

(* ------------------------------------------------------------------------- *)
(* Terms                                                                     *)
(* ------------------------------------------------------------------------- *)

datatype term' =
    Const of Name.name * Type.ty
  | Var of Var.var
  | Comb of term' * term'
  | Abs of Var.var * term';

type term = term';

(* ------------------------------------------------------------------------- *)
(* The constant registry (initially contains the primitive constants)        *)
(* ------------------------------------------------------------------------- *)

datatype registry =
    Registry of
      {all : Name.name list,
       types : Type.ty NameMap.map};

val registry = ref (Registry {all = [], types = NameMap.new ()});

fun constType name =
    let
      val Registry {types,...} = !registry
    in
      NameMap.peek types name
    end;

fun allConsts name =
    let
      val Registry {all,...} = !registry
    in
      all
    end;

fun declareConst name ty =
    let
      val _ = not (Option.isSome (constType name)) orelse
              raise Error ("already a constant with name " ^
                           Name.toString name)
      val Registry {all,types} = !registry
      val all = name :: all
      and types = NameMap.insert types (name,ty)
    in
      registry := Registry {all = all, types = types}
    end
    handle Error err => raise Error ("Term.declareConst: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Type checking                                                             *)
(* ------------------------------------------------------------------------- *)

fun typeOf (Const (_,ty)) = ty
  | typeOf (Var (_,ty)) = ty
  | typeOf (Comb (t,u)) = snd (Type.destFun (typeOf t))
  | typeOf (Abs ((_,vty),t)) = Type.mkFun (vty, typeOf t);

(* ------------------------------------------------------------------------- *)
(* Term constructors and destructors                                         *)
(* ------------------------------------------------------------------------- *)

fun mk (tm : term) = tm;

fun dest (tm : term) = tm;

fun mkConst (n,ty) =
    let
      val () =
          case constType n of
            NONE => ()
          | SOME ty' =>
            if can (TypeSubst.match ty') ty then ()
            else raise Error ("bad type for constant " ^ Name.toString n)
    in
      Const (n,ty)
    end
    handle Error err => raise Error ("Term.mkConst: " ^ err);

fun destConst (Const n_ty) = n_ty
  | destConst _ = raise Error "destConst";

val isConst = can destConst;

val mkVar = Var;

fun destVar (Var v) = v
  | destVar _ = raise Error "destVar";

val isVar = can destVar;

fun equalVar var (Var v) = Var.equal var v
  | equalVar _ _ = false;

fun mkComb (f,x) =
    let
      val (ty,_) = Type.destFun (typeOf f)
      val ty' = typeOf x
      val _ = Type.equal ty ty' orelse raise Error "incompatible types"
    in
      Comb (f,x)
    end
    handle Error err => raise Error ("Term.mkComb: " ^ err);

fun destComb (Comb f_x) = f_x
  | destComb _ = raise Error "destComb";

val isComb = can destComb;

val mkAbs = Abs;

fun destAbs (Abs v_t) = v_t
  | destAbs _ = raise Error "destAbs";

val isAbs = can destAbs;

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence                *)
(* ------------------------------------------------------------------------- *)

val constCompare = prodCompare Name.compare Type.compare;

local
  fun cmp tm1_tm2 =
      if op== tm1_tm2 then EQUAL
      else
        case tm1_tm2 of
          (Const c1, Const c2) => constCompare (c1,c2)
        | (Const _, _) => LESS
        | (_, Const _) => GREATER
        | (Var v1, Var v2) => Var.compare (v1,v2)
        | (Var _, _) => LESS
        | (_, Var _) => GREATER
        | (Comb a1, Comb a2) => prodCompare cmp cmp (a1,a2)
        | (Comb _, _) => LESS
        | (_, Comb _) => GREATER
        | (Abs l1, Abs l2) => prodCompare Var.compare cmp (l1,l2);
in
  val compare = cmp;
end;

fun equal tm1 tm2 = compare (tm1,tm2) = EQUAL;

local
  fun acmp n bv1 bv2 bvEq tm1_tm2 =
      if bvEq andalso op== tm1_tm2 then EQUAL
      else
        case tm1_tm2 of
          (Const c1, Const c2) => constCompare (c1,c2)
        | (Const _, _) => LESS
        | (_, Const _) => GREATER
        | (Var v1, Var v2) =>
          (case (VarMap.peek bv1 v1, VarMap.peek bv2 v2) of
             (NONE,NONE) => Var.compare (v1,v2)
           | (SOME _, NONE) => LESS
           | (NONE, SOME _) => GREATER
           | (SOME n1, SOME n2) => Int.compare (n1,n2))
        | (Var _, _) => LESS
        | (_, Var _) => GREATER
        | (Comb a1, Comb a2) =>
          let
            val cmp = acmp n bv1 bv2 bvEq
          in
            prodCompare cmp cmp (a1,a2)
          end
        | (Comb _, _) => LESS
        | (_, Comb _) => GREATER
        | (Abs (v1,t1), Abs (v2,t2)) =>
          let
            val bvEq = bvEq andalso Var.equal v1 v2
            val bv1 = VarMap.insert bv1 (v1,n)
            val bv2 = if bvEq then bv1 else VarMap.insert bv2 (v2,n)
            val n = n + 1
          in
            acmp n bv1 bv2 bvEq (t1,t2)
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
(* Free term and type variables                                              *)
(* ------------------------------------------------------------------------- *)

fun typeVars (Const (_,ty)) = Type.typeVars ty
  | typeVars (Var (_,ty)) = Type.typeVars ty
  | typeVars (Comb (a,b)) = NameSet.union (typeVars a) (typeVars b)
  | typeVars (Abs ((_,ty),b)) = NameSet.union (Type.typeVars ty) (typeVars b);

val freeVars =
    let
      fun fv _ (Const _) = VarSet.empty
        | fv bv (Var v) =
          if VarSet.member v bv then VarSet.empty else VarSet.singleton v
        | fv bv (Comb (a,b)) = VarSet.union (fv bv a) (fv bv b)
        | fv bv (Abs (v,b)) = fv (VarSet.add bv v) b
    in
      fv VarSet.empty
    end;

(* ------------------------------------------------------------------------- *)
(* Type operators and constants.                                             *)
(* ------------------------------------------------------------------------- *)

fun typeOps tm =
    case tm of
      Const (_,ty) => Type.typeOps ty
    | Var v => Var.typeOps v
    | Comb (f,a) => NameSet.union (typeOps f) (typeOps a)
    | Abs (v,b) => NameSet.union (Var.typeOps v) (typeOps b);

fun consts tm =
    case tm of
      Const (n,_) => NameSet.singleton n
    | Var _ => NameSet.empty
    | Comb (f,a) => NameSet.union (consts f) (consts a)
    | Abs (_,b) => consts b;

(* ------------------------------------------------------------------------- *)
(* Primitive constants                                                       *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

fun eqTy a = Type.mkFun (a, Type.mkFun (a, Type.boolTy));

val eqN = Name.mkGlobal "="

val eqTm =
    let
      val ty = eqTy Type.alphaTy
      val () = declareConst eqN ty
    in
      Const (eqN,ty)
    end;

fun mkEq (l,r) = mkComb (mkComb (mkConst (eqN, eqTy (typeOf l)), l), r);

fun destEq (Comb (Comb (Const (n,_), l), r)) =
    if Name.equal n eqN then (l,r) else raise Error "Term.destEq"
  | destEq _ = raise Error "Term.destEq";

val isEq = can destEq;

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
