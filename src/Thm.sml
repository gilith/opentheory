(* ========================================================================= *)
(* A MINIMAL HIGHER ORDER LOGIC KERNEL                                       *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Thm :> Thm =
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
structure T = Term;
structure TS = TermSet;
structure TAS = TermAlphaSet;
structure TU = TermSubst;
structure S = Sequent;
structure SS = SequentSet;

infixr ==

val op== = Portable.pointerEqual;

(* ------------------------------------------------------------------------- *)
(* Theorems                                                                  *)
(* ------------------------------------------------------------------------- *)

datatype thm' = Thm of {id : int, axioms : SS.set, sequent : S.sequent};

type thm = thm';

(* ------------------------------------------------------------------------- *)
(* Destructors                                                               *)
(* ------------------------------------------------------------------------- *)

fun dest (th : thm) = th;

(* ------------------------------------------------------------------------- *)
(* The theorem id counter                                                    *)
(* ------------------------------------------------------------------------- *)

val next_thm_id =
    let
      val counter = ref 0
    in
      fn () => let val c = !counter val () = counter := c + 1 in c end
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference                                              *)
(* ------------------------------------------------------------------------- *)

val empty_axioms = SS.empty;
val single_axiom = SS.singleton;

val empty_hyp = TAS.empty;
val single_hyp = TAS.singleton;

fun Axiom sequent =
    let
      val _ = S.boolean sequent orelse
              raise Error "Axiom: sequent is not boolean"
      val axioms = single_axiom sequent
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end;

fun Abs v th =
    let
      val Thm {axioms,sequent,...} = th
      val {hyp,concl} = sequent
      val (a,b) = T.dest_eq concl
      val fv = TAS.foldl (fn (t,z) => VS.union z (T.free_vars t)) VS.empty hyp
      val _ = not (VS.member v fv) orelse raise Error "Abs: free in hypothesis"
      val concl = T.mk_eq (T.mk_abs (v,a), T.mk_abs (v,b))
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end;

fun Assume t =
    let
      val _ = Ty.equal (T.type_of t) Ty.bool orelse
              raise Error "Assume: not a proposition"
      val axioms = empty_axioms
      and hyp = single_hyp t
      and concl = t
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end;

fun Beta_conv t =
    let
      val (v,t1,t2) =
          case T.dest t of
            T.App (t',t2) =>
            (case T.dest t' of
               T.Lam (v,t1) => (v,t1,t2)
             | _ => raise Error "Beta_conv: term function not a lambda")
          | _ => raise Error "Beta_conv: term not a function application"
      val u = if T.equal_var v t2 then t1 else TU.subst (TU.singleton (v,t2)) t1
      val axioms = empty_axioms
      and hyp = empty_hyp
      and concl = T.mk_eq (t,u)
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end;

fun Deduct_antisym th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2
      val {hyp = h1, concl = c1} = s1
      and {hyp = h2, concl = c2} = s2
      val h1 = if TAS.member c2 h1 then TAS.delete h1 c2 else h1
      and h2 = if TAS.member c1 h2 then TAS.delete h2 c1 else h2
      val axioms = SS.union a1 a2
      and hyp = TAS.union h1 h2
      and concl = T.mk_eq (c1,c2)
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end;

fun Eq_mp th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2
      val {hyp = h1, concl = c1} = s1
      and {hyp = h2, concl = c2} = s2
      val axioms = SS.union a1 a2
      and hyp = TAS.union h1 h2
      val (c2',concl) = T.dest_eq c1
      val _ = T.alpha_equal c2 c2' orelse
              raise Error "Eq_mp: not alpha equivalent"
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end

fun Subst sub th =
    let
      val Thm {axioms,sequent,...} = th
      val {hyp,concl} = sequent
      val subst = TU.subst sub
      val hyp = TAS.foldl (fn (tm,z) => TAS.add z (subst tm)) empty_hyp hyp
      and concl = subst concl
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end;

fun Mk_comb th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2
      val {hyp = h1, concl = c1} = s1
      and {hyp = h2, concl = c2} = s2
      val (l1,r1) = T.dest_eq c1
      and (l2,r2) = T.dest_eq c2
      val axioms = SS.union a1 a2
      and hyp = TAS.union h1 h2
      and concl = T.mk_eq (T.mk_comb (l1,l2), T.mk_comb (r1,r2))
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end;

fun Refl t =
    let
      val axioms = empty_axioms
      and hyp = empty_hyp
      and concl = T.mk_eq (t,t)
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end;
 
(* ------------------------------------------------------------------------- *)
(* Definitions                                                               *)
(* ------------------------------------------------------------------------- *)

fun Define_const name t =
    let
      val ty = T.type_of t
      val _ = VS.null (T.free_vars t) orelse raise Error "term not closed"
      val _ = NS.subset (T.type_vars t) (Ty.type_vars ty) orelse
              raise Error "extra type variables in term"
      val () = T.declare_const name ty
      val axioms = empty_axioms
      and hyp = empty_hyp
      and concl = T.mk_eq (T.mk_const (name,ty), t)
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
    end
    handle Error err => raise Error ("Define_const: " ^ err);

fun Define_type name {abs,rep} ty_vars non_empty_th =
    let
      val Thm {axioms,sequent,...} = non_empty_th
      val {hyp,concl} = sequent
      val _ = TAS.null hyp orelse
              raise Error "existence theorem must not have hypotheses"
      val (P,t) = T.dest_comb concl
      val _ = VS.null (T.free_vars P) orelse
              raise Error "predicate is not closed"
      val _ = NS.equal (NS.fromList ty_vars) (T.type_vars P) orelse
              raise Error "supplied type vars are not the type vars in P"
      val _ = NS.size (NS.fromList ty_vars) = length ty_vars orelse
              raise Error "supplied type variables contain duplicates"
      val arity = length ty_vars
      val () = Ty.declare_type name arity
      val aty = Ty.mk_op (name, map Ty.mk_var ty_vars)
      and rty = T.type_of t
      val abs_ty = Ty.mk_fun (rty,aty)
      and rep_ty = Ty.mk_fun (aty,rty)
      val () = T.declare_const abs abs_ty
      and () = T.declare_const rep rep_ty
      val abs_tm = T.mk_const (abs,abs_ty)
      and rep_tm = T.mk_const (rep,rep_ty)
      val abs_rep_th =
          let
            val a = T.mk_var ("a",aty)
            val concl = T.mk_eq (T.mk_comb (abs_tm, T.mk_comb (rep_tm,a)), a)
            val sequent = {hyp = hyp, concl = concl}
          in
            Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
          end
      val rep_abs_th =
          let
            val r = T.mk_var ("r",rty)
            val concl =
                T.mk_eq
                  (T.mk_comb (P,r),
                   T.mk_eq (T.mk_comb (rep_tm, T.mk_comb (abs_tm,r)), r))
            val sequent = {hyp = hyp, concl = concl}
          in
            Thm {id = next_thm_id (), axioms = axioms, sequent = sequent}
          end
    in
      (abs_rep_th,rep_abs_th)
    end
    handle Error err => raise Error ("Define_type: " ^ err);

end
