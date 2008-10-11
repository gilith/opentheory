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
(* A type of higher order logic terms.                                       *)
(* ------------------------------------------------------------------------- *)

datatype term =
    Term of
      {tm : term',
       sz : int,
       ty : Type.ty,
       fvs : VarSet.set,
       cs : NameSet.set,
       tyVs : NameSet.set,
       tyOs : NameSet.set}

and term' =
    Const of Name.name * Type.ty
  | Var of Var.var
  | Comb of term * term
  | Abs of Var.var * term;

(* ------------------------------------------------------------------------- *)
(* The constant registry (initially contains the primitive constants).       *)
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
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

fun size (Term {sz,...}) = sz;

(* ------------------------------------------------------------------------- *)
(* Type checking.                                                            *)
(* ------------------------------------------------------------------------- *)

fun typeOf (Term {ty,...}) = ty;

(* ------------------------------------------------------------------------- *)
(* Free term and type variables.                                             *)
(* ------------------------------------------------------------------------- *)

fun freeVars (Term {fvs,...}) = fvs;

fun typeVars (Term {tyVs,...}) = tyVs;

(* ------------------------------------------------------------------------- *)
(* Constants and type operators.                                             *)
(* ------------------------------------------------------------------------- *)

fun consts (Term {cs,...}) = cs;

fun typeOps (Term {tyOs,...}) = tyOs;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk tm =
    case tm of
      Const (n,ty) =>
      let
        val () =
            case constType n of
              NONE => ()
            | SOME ty' =>
              if can (TypeSubst.match ty') ty then ()
              else raise Error ("Term.mk: bad type for constant " ^
                                Name.toString n)

        val sz = 1
        val fvs = VarSet.empty
        val cs = NameSet.singleton n
        val tyVs = Type.typeVars ty
        val tyOs = Type.typeOps ty
      in
        Term
          {tm = tm,
           sz = sz,
           ty = ty,
           fvs = fvs,
           cs = cs,
           tyVs = tyVs,
           tyOs = tyOs}
      end
    | Var v =>
      let
        val (_,ty) = v
        val sz = 1
        val fvs = VarSet.singleton v
        val cs = NameSet.empty
        val tyVs = Type.typeVars ty
        val tyOs = Type.typeOps ty
      in
        Term
          {tm = tm,
           sz = sz,
           ty = ty,
           fvs = fvs,
           cs = cs,
           tyVs = tyVs,
           tyOs = tyOs}
      end
    | Comb (f,a) =>
      let
        val (tyA,ty) = Type.destFun (typeOf f)
        val tyA' = typeOf a
        val _ = Type.equal tyA tyA' orelse
                raise Error "Term.mk: incompatible types in comb"
        val sz = size f + size a + 1
        val fvs = VarSet.union (freeVars f) (freeVars a)
        val cs = NameSet.union (consts f) (consts a)
        val tyVs = NameSet.union (typeVars f) (typeVars a)
        val tyOs = NameSet.union (typeOps f) (typeOps a)
      in
        Term
          {tm = tm,
           sz = sz,
           ty = ty,
           fvs = fvs,
           cs = cs,
           tyVs = tyVs,
           tyOs = tyOs}
      end
    | Abs (v,b) =>
      let
        val (_,tyV) = v
        val sz = size b + 1
        val ty = Type.mkFun (tyV, typeOf b);
        val fvs = freeVars b
        val fvs = if VarSet.member v fvs then VarSet.delete fvs v else fvs
        val cs = consts b
        val tyVs = NameSet.union (Type.typeVars tyV) (typeVars b)
        val tyOs = NameSet.union (Type.typeOps tyV) (typeOps b)
      in
        Term
          {tm = tm,
           sz = sz,
           ty = ty,
           fvs = fvs,
           cs = cs,
           tyVs = tyVs,
           tyOs = tyOs}
      end;

fun dest (Term {tm,...}) = tm;

(* Constants *)

fun mkConst' n_ty = Const n_ty;

fun destConst' (Const n_ty) = n_ty
  | destConst' _ = raise Error "Term.destConst'";

val isConst' = can destConst';

fun mkConst n_ty = mk (mkConst' n_ty);

fun destConst tm = destConst' (dest tm);

val isConst = can destConst;

(* Variables *)

fun mkVar' v = Var v;

fun destVar' (Var v) = v
  | destVar' _ = raise Error "Term.destVar'";

val isVar' = can destVar';

fun equalVar' var (Var v) = Var.equal var v
  | equalVar' _ _ = false;

fun mkVar v = mk (mkVar' v);

fun destVar tm = destVar' (dest tm);

val isVar = can destVar;

fun equalVar var tm = equalVar' var (dest tm);

(* Function applications *)

fun mkComb' f_a = Comb f_a;

fun destComb' (Comb f_a) = f_a
  | destComb' _ = raise Error "Term.destComb'";

val isComb' = can destComb';

fun mkComb f_a = mk (mkComb' f_a);

fun destComb tm = destComb' (dest tm);

val isComb = can destComb;

(* Function abstractions *)

fun mkAbs' v_b = Abs v_b;

fun destAbs' (Abs v_b) = v_b
  | destAbs' _ = raise Error "Term.destAbs'";

val isAbs' = can destAbs';

fun mkAbs v_b = mk (mkAbs' v_b);

fun destAbs tm = destAbs' (dest tm);

val isAbs = can destAbs;

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence.               *)
(* ------------------------------------------------------------------------- *)

val constCompare = prodCompare Name.compare Type.compare;

fun compare tm1_tm2 =
    if op== tm1_tm2 then EQUAL
    else
      let
        val (Term {tm = tm1, ...}, Term {tm = tm2, ...}) = tm1_tm2
      in
        compare' (tm1,tm2)
      end

and compare' tm1_tm2 =
    if op== tm1_tm2 then EQUAL
    else
      case tm1_tm2 of
        (Const c1, Const c2) => constCompare (c1,c2)
      | (Const _, _) => LESS
      | (_, Const _) => GREATER
      | (Var v1, Var v2) => Var.compare (v1,v2)
      | (Var _, _) => LESS
      | (_, Var _) => GREATER
      | (Comb a1, Comb a2) => prodCompare compare compare (a1,a2)
      | (Comb _, _) => LESS
      | (_, Comb _) => GREATER
      | (Abs l1, Abs l2) => prodCompare Var.compare compare (l1,l2);

fun equal tm1 tm2 = compare (tm1,tm2) = EQUAL;

local
  fun acmp n bv1 bv2 bvEq tm1_tm2 =
      if bvEq andalso op== tm1_tm2 then EQUAL
      else
        let
          val (Term {tm = tm1, ...}, Term {tm = tm2, ...}) = tm1_tm2
        in
          acmp' n bv1 bv2 bvEq (tm1,tm2)
        end

  and acmp' n bv1 bv2 bvEq tm1_tm2 =
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
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

fun eqTy a = Type.mkFun (a, Type.mkFun (a, Type.bool));

val eqN = Name.mkGlobal "="

val eqTm =
    let
      val ty = eqTy Type.alpha
      val () = declareConst eqN ty
    in
      mkConst (eqN,ty)
    end;

fun mkEq (l,r) = mkComb (mkComb (mkConst (eqN, eqTy (typeOf l)), l), r);

fun destEq tm =
    let
      val (eq_l,r) = destComb tm
      val (eq,l) = destComb eq_l
      val (n,_) = destConst eq
    in
      if Name.equal n eqN then (l,r) else raise Error "Term.destEq"
    end;

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
