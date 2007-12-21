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

datatype thm' = Thm of {axioms : SS.set, sequent : S.sequent};

type thm = thm';

(* ------------------------------------------------------------------------- *)
(* Destructors                                                               *)
(* ------------------------------------------------------------------------- *)

fun dest (th : thm) = th;

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference                                              *)
(* ------------------------------------------------------------------------- *)

val emptyAxioms = SS.empty;
val singleAxiom = SS.singleton;

val emptyHyp = TAS.empty;
val singleHyp = TAS.singleton;

fun axiom sequent =
    let
      val _ = S.boolean sequent orelse
              raise Error "Thm.axiom: sequent is not boolean"
      val axioms = singleAxiom sequent
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun abs v th =
    let
      val Thm {axioms,sequent,...} = th
      val {hyp,concl} = sequent
      val (a,b) = T.destEq concl
      val fv = TAS.foldl (fn (t,z) => VS.union z (T.freeVars t)) VS.empty hyp
      val _ = not (VS.member v fv) orelse
              raise Error "Thm.abs: free in hypothesis"
      val concl = T.mkEq (T.mkAbs (v,a), T.mkAbs (v,b))
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun assume t =
    let
      val _ = Ty.equal (T.typeOf t) Ty.boolTy orelse
              raise Error "Thm.assume: not a proposition"
      val axioms = emptyAxioms
      and hyp = singleHyp t
      and concl = t
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun betaConv t =
    let
      val (v,t1,t2) =
          case T.dest t of
            T.App (t',t2) =>
            (case T.dest t' of
               T.Lam (v,t1) => (v,t1,t2)
             | _ => raise Error "Thm.betaConv: term function not a lambda")
          | _ => raise Error "Thm.betaConv: term not a function application"
      val u = if T.equalVar v t2 then t1 else TU.subst (TU.singleton (v,t2)) t1
      val axioms = emptyAxioms
      and hyp = emptyHyp
      and concl = T.mkEq (t,u)
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun deductAntisym th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2
      val {hyp = h1, concl = c1} = s1
      and {hyp = h2, concl = c2} = s2
      val h1 = if TAS.member c2 h1 then TAS.delete h1 c2 else h1
      and h2 = if TAS.member c1 h2 then TAS.delete h2 c1 else h2
      val axioms = SS.union a1 a2
      and hyp = TAS.union h1 h2
      and concl = T.mkEq (c1,c2)
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun eqMp th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2
      val {hyp = h1, concl = c1} = s1
      and {hyp = h2, concl = c2} = s2
      val axioms = SS.union a1 a2
      and hyp = TAS.union h1 h2
      val (c2',concl) = T.destEq c1
      val _ = T.alphaEqual c2 c2' orelse
              raise Error "Thm.eqMp: not alpha equivalent"
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end

fun subst sub th =
    let
      val Thm {axioms,sequent,...} = th
      val {hyp,concl} = sequent
      val subst = TU.subst sub
      val hyp = TAS.foldl (fn (tm,z) => TAS.add z (subst tm)) emptyHyp hyp
      and concl = subst concl
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun comb th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2
      val {hyp = h1, concl = c1} = s1
      and {hyp = h2, concl = c2} = s2
      val (l1,r1) = T.destEq c1
      and (l2,r2) = T.destEq c2
      val axioms = SS.union a1 a2
      and hyp = TAS.union h1 h2
      and concl = T.mkEq (T.mkComb (l1,l2), T.mkComb (r1,r2))
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun refl t =
    let
      val axioms = emptyAxioms
      and hyp = emptyHyp
      and concl = T.mkEq (t,t)
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;
 
(* ------------------------------------------------------------------------- *)
(* Definitions                                                               *)
(* ------------------------------------------------------------------------- *)

fun defineConst name t =
    let
      val ty = T.typeOf t
      val _ = VS.null (T.freeVars t) orelse raise Error "term not closed"
      val _ = NS.subset (T.typeVars t) (Ty.typeVars ty) orelse
              raise Error "extra type variables in term"
      val () = T.declareConst name ty
      val axioms = emptyAxioms
      and hyp = emptyHyp
      and concl = T.mkEq (T.mkConst (name,ty), t)
      val sequent = {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end
    handle Error err => raise Error ("Thm.defineConst: " ^ err);

fun defineType name {abs,rep} tyVars nonEmptyTh =
    let
      val Thm {axioms,sequent,...} = nonEmptyTh
      val {hyp,concl} = sequent
      val _ = TAS.null hyp orelse
              raise Error "existence theorem must not have hypotheses"
      val (P,t) = T.destComb concl
      val _ = VS.null (T.freeVars P) orelse
              raise Error "predicate is not closed"
      val _ = NS.equal (NS.fromList tyVars) (T.typeVars P) orelse
              raise Error "supplied type vars are not the type vars in P"
      val _ = NS.size (NS.fromList tyVars) = length tyVars orelse
              raise Error "supplied type variables contain duplicates"
      val arity = length tyVars
      val () = Ty.declareType name arity
      val aty = Ty.mkOp (name, map Ty.mkVar tyVars)
      and rty = T.typeOf t
      val absTy = Ty.mkFun (rty,aty)
      and repTy = Ty.mkFun (aty,rty)
      val () = T.declareConst abs absTy
      and () = T.declareConst rep repTy
      val absTm = T.mkConst (abs,absTy)
      and repTm = T.mkConst (rep,repTy)
      val absRepTh =
          let
            val a = T.mkVar ("a",aty)
            val concl = T.mkEq (T.mkComb (absTm, T.mkComb (repTm,a)), a)
            val sequent = {hyp = hyp, concl = concl}
          in
            Thm {axioms = axioms, sequent = sequent}
          end
      val repAbsTh =
          let
            val r = T.mkVar ("r",rty)
            val concl =
                T.mkEq
                  (T.mkComb (P,r),
                   T.mkEq (T.mkComb (repTm, T.mkComb (absTm,r)), r))
            val sequent = {hyp = hyp, concl = concl}
          in
            Thm {axioms = axioms, sequent = sequent}
          end
    in
      (absRepTh,repAbsTh)
    end
    handle Error err => raise Error ("Thm.defineType: " ^ err);

end
