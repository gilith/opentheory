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

fun const_type name =
    let
      val Registry {types,...} = !registry
    in
      case NM.peek types name of
        NONE => raise Error ("const_type: no constant with name "^name)
      | SOME ty => ty
    end;

fun all_consts name =
    let
      val Registry {all,...} = !registry
    in
      all
    end;

fun declare_const name ty =
    let
      val _ = not (can const_type name) orelse
              raise Error ("already a constant with name " ^ name)
      val Registry {all,types} = !registry
      val all = name :: all
      and types = NM.insert types (name,ty)
    in
      registry := Registry {all = all, types = types}
    end
    handle Error err => raise Error ("Term.declare_const: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Type checking                                                             *)
(* ------------------------------------------------------------------------- *)

fun type_of (Const (_,ty)) = ty
  | type_of (Var (_,ty)) = ty
  | type_of (App (t,u)) = snd (Ty.dest_fun (type_of t))
  | type_of (Lam ((_,vty),t)) = Ty.mk_fun (vty, type_of t);

(* ------------------------------------------------------------------------- *)
(* Term constructors and destructors                                         *)
(* ------------------------------------------------------------------------- *)

fun mk (tm : term) = tm;

fun dest (tm : term) = tm;

fun mk_const (n,ty) =
    let
      val ty' = const_type n
      val _ = can (TyU.match ty') ty orelse
              raise Error ("bad type for constant " ^ n)
    in
      Const (n,ty)
    end
    handle Error err => raise Error ("Term.mk_const: " ^ err);

fun dest_const (Const n_ty) = n_ty | dest_const _ = raise Error "dest_const";

val is_const = can dest_const;

val mk_var = Var;

fun dest_var (Var v) = v | dest_var _ = raise Error "dest_var";

val is_var = can dest_var;

fun equal_var var (Var v) = Var.equal var v
  | equal_var _ _ = false;

fun mk_comb (f,x) =
    let
      val (ty,_) = Ty.dest_fun (type_of f)
      val ty' = type_of x
      val _ = Ty.equal ty ty' orelse raise Error "incompatible types"
    in
      App (f,x)
    end
    handle Error err => raise Error ("mk_comb: " ^ err);

fun dest_comb (App f_x) = f_x | dest_comb _ = raise Error "dest_comb";

val is_comb = can dest_comb;

val mk_abs = Lam;

fun dest_abs (Lam v_t) = v_t | dest_abs _ = raise Error "dest_abs";

val is_abs = can dest_abs;

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence                *)
(* ------------------------------------------------------------------------- *)

val const_compare = prodCompare N.compare Ty.compare;

local
  fun cmp tm1_tm2 =
      if op== tm1_tm2 then EQUAL
      else
        case tm1_tm2 of
          (Const c1, Const c2) => const_compare (c1,c2)
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
          (Const c1, Const c2) => const_compare (c1,c2)
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
  val alpha_compare = acmp 0 (VM.new ()) (VM.new ());
end;

fun alpha_equal tm1 tm2 = alpha_compare (tm1,tm2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Free term and type variables                                              *)
(* ------------------------------------------------------------------------- *)

fun type_vars (Const (_,ty)) = Ty.type_vars ty
  | type_vars (Var (_,ty)) = Ty.type_vars ty
  | type_vars (App (a,b)) = NS.union (type_vars a) (type_vars b)
  | type_vars (Lam ((_,ty),b)) = NS.union (Ty.type_vars ty) (type_vars b);

val free_vars =
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

fun eq_type a = Ty.mk_fun (a, Ty.mk_fun (a, Ty.bool));

val eq =
    let
      val n = "="
      and ty = eq_type Ty.alpha
      val () = declare_const n ty
    in
      Const (n,ty)
    end;

fun mk_eq (l,r) = mk_comb (mk_comb (mk_const ("=", eq_type (type_of l)), l), r);

fun dest_eq (App (App (Const ("=",_), l), r)) = (l,r)
  | dest_eq _ = raise Error "dest_eq";

val is_eq = can dest_eq;

(* Hilbert's Epsilon operator *)

fun select_type a = Ty.mk_fun (Ty.mk_fun (a, Ty.bool), a);

val select =
    let
      val n = "@"
      and ty = select_type Ty.alpha
      val () = declare_const n ty
    in
      Const (n,ty)
    end;

fun mk_select (v_b as ((_,ty),_)) = App (Const ("@", select_type ty), Lam v_b);

fun dest_select (App (Const ("@",_), Lam v_b)) = v_b
  | dest_select _ = raise Error "dest_select";

val is_select = can dest_select;

end

structure TermSet =
ElementSet (struct type t = Term.term val compare = Term.compare end);

structure TermMap =
KeyMap (struct type t = Term.term val compare = Term.compare end);

structure TermAlphaSet =
ElementSet (struct type t = Term.term val compare = Term.alpha_compare end);

structure TermAlphaMap =
KeyMap (struct type t = Term.term val compare = Term.alpha_compare end);
