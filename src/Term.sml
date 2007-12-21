(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Term :> Term =
struct

open Useful;

structure N = Name;
structure NS = NameSet;
structure NM = NameMap;
structure Ty = Type;
structure TyU = TypeSubst;
structure V = Var;
structure VS = VarSet;
structure VM = VarMap;

infixr ==

val op== = Portable.pointerEqual;

(* ------------------------------------------------------------------------- *)
(* Terms                                                                     *)
(* ------------------------------------------------------------------------- *)

datatype term' =
    Const of N.name * Ty.ty
  | Var of V.var
  | App of term' * term'
  | Lam of V.var * term';

type term = term';

(* ------------------------------------------------------------------------- *)
(* The constant registry (initially contains the primitive constants)        *)
(* ------------------------------------------------------------------------- *)

datatype registry = Registry of {all : N.name list, types : Ty.ty NM.map};

val registry = ref (Registry {all = [], types = NM.new ()});

fun constType name =
    let
      val Registry {types,...} = !registry
    in
      case NM.peek types name of
        NONE => raise Error ("constType: no constant with name "^name)
      | SOME ty => ty
    end;

fun allConsts name =
    let
      val Registry {all,...} = !registry
    in
      all
    end;

fun declareConst name ty =
    let
      val _ = not (can constType name) orelse
              raise Error ("already a constant with name " ^ name)
      val Registry {all,types} = !registry
      val all = name :: all
      and types = NM.insert types (name,ty)
    in
      registry := Registry {all = all, types = types}
    end
    handle Error err => raise Error ("Term.declareConst: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Type checking                                                             *)
(* ------------------------------------------------------------------------- *)

fun typeOf (Const (_,ty)) = ty
  | typeOf (Var (_,ty)) = ty
  | typeOf (App (t,u)) = snd (Ty.destFun (typeOf t))
  | typeOf (Lam ((_,vty),t)) = Ty.mkFun (vty, typeOf t);

(* ------------------------------------------------------------------------- *)
(* Term constructors and destructors                                         *)
(* ------------------------------------------------------------------------- *)

fun mk (tm : term) = tm;

fun dest (tm : term) = tm;

fun mkConst (n,ty) =
    let
      val ty' = constType n
      val _ = can (TyU.match ty') ty orelse
              raise Error ("bad type for constant " ^ n)
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
      val (ty,_) = Ty.destFun (typeOf f)
      val ty' = typeOf x
      val _ = Ty.equal ty ty' orelse raise Error "incompatible types"
    in
      App (f,x)
    end
    handle Error err => raise Error ("Term.mkComb: " ^ err);

fun destComb (App f_x) = f_x
  | destComb _ = raise Error "destComb";

val isComb = can destComb;

val mkAbs = Lam;

fun destAbs (Lam v_t) = v_t
  | destAbs _ = raise Error "destAbs";

val isAbs = can destAbs;

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence                *)
(* ------------------------------------------------------------------------- *)

val constCompare = prodCompare N.compare Ty.compare;

local
  fun cmp tm1_tm2 =
      if op== tm1_tm2 then EQUAL
      else
        case tm1_tm2 of
          (Const c1, Const c2) => constCompare (c1,c2)
        | (Const _, _) => LESS
        | (_, Const _) => GREATER
        | (Var v1, Var v2) => V.compare (v1,v2)
        | (Var _, _) => LESS
        | (_, Var _) => GREATER
        | (App a1, App a2) => prodCompare cmp cmp (a1,a2)
        | (App _, _) => LESS
        | (_, App _) => GREATER
        | (Lam l1, Lam l2) => prodCompare V.compare cmp (l1,l2);
in
  val compare = cmp;
end;

fun equal tm1 tm2 = compare (tm1,tm2) = EQUAL;

local
  fun acmp n bv1 bv2 tm1_tm2 =
      if n = 0 andalso op== tm1_tm2 then EQUAL
      else
        case tm1_tm2 of
          (Const c1, Const c2) => constCompare (c1,c2)
        | (Const _, _) => LESS
        | (_, Const _) => GREATER
        | (Var v1, Var v2) =>
          (case (VM.peek bv1 v1, VM.peek bv2 v2) of
             (NONE,NONE) => V.compare (v1,v2)
           | (SOME _, NONE) => LESS
           | (NONE, SOME _) => GREATER
           | (SOME n1, SOME n2) => Int.compare (n1,n2))
        | (Var _, _) => LESS
        | (_, Var _) => GREATER
        | (App a1, App a2) =>
          let
            val cmp = acmp n bv1 bv2
          in
            prodCompare cmp cmp (a1,a2)
          end
        | (App _, _) => LESS
        | (_, App _) => GREATER
        | (Lam (v1,t1), Lam (v2,t2)) =>
          if n = 0 andalso V.equal v1 v2 then acmp n bv1 bv2 (t1,t2)
          else acmp (n+1) (VM.insert bv1 (v1,n)) (VM.insert bv2 (v2,n)) (t1,t2);
in
  val alphaCompare = acmp 0 (VM.new ()) (VM.new ());
end;

fun alphaEqual tm1 tm2 = alphaCompare (tm1,tm2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Free term and type variables                                              *)
(* ------------------------------------------------------------------------- *)

fun typeVars (Const (_,ty)) = Ty.typeVars ty
  | typeVars (Var (_,ty)) = Ty.typeVars ty
  | typeVars (App (a,b)) = NS.union (typeVars a) (typeVars b)
  | typeVars (Lam ((_,ty),b)) = NS.union (Ty.typeVars ty) (typeVars b);

val freeVars =
    let
      fun fv _ (Const _) = VS.empty
        | fv bv (Var v) = if VS.member v bv then VS.empty else VS.singleton v
        | fv bv (App (a,b)) = VS.union (fv bv a) (fv bv b)
        | fv bv (Lam (v,b)) = fv (VS.add bv v) b
    in
      fv VS.empty
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive constants                                                       *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

fun eqTy a = Ty.mkFun (a, Ty.mkFun (a, Ty.boolTy));

val eqTm =
    let
      val n = "="
      and ty = eqTy Ty.alphaTy
      val () = declareConst n ty
    in
      Const (n,ty)
    end;

fun mkEq (l,r) = mkComb (mkComb (mkConst ("=", eqTy (typeOf l)), l), r);

fun destEq (App (App (Const ("=",_), l), r)) = (l,r)
  | destEq _ = raise Error "Term.destEq";

val isEq = can destEq;

(* Hilbert's Epsilon operator *)

fun selectTy a = Ty.mkFun (Ty.mkFun (a, Ty.boolTy), a);

val selectTm =
    let
      val n = "@"
      and ty = selectTy Ty.alphaTy
      val () = declareConst n ty
    in
      Const (n,ty)
    end;

fun mkSelect (v_b as ((_,ty),_)) = App (Const ("@", selectTy ty), Lam v_b);

fun destSelect (App (Const ("@",_), Lam v_b)) = v_b
  | destSelect _ = raise Error "Term.destSelect";

val isSelect = can destSelect;

end

structure TermOrdered =
struct type t = Term.term val compare = Term.compare end

structure TermSet = ElementSet (TermOrdered)

structure TermMap = KeyMap (TermOrdered)

structure TermAlphaOrdered =
struct type t = Term.term val compare = Term.alphaCompare end

structure TermAlphaSet = ElementSet (TermAlphaOrdered)

structure TermAlphaMap = KeyMap (TermAlphaOrdered)
