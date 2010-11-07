(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEOREMS                                               *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Thm :> Thm =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* The abstract type of theorem.                                             *)
(* ------------------------------------------------------------------------- *)

datatype thm' =
    Thm of
      {axioms : SequentSet.set,
       sequent : Sequent.sequent};

type thm = thm';

(* ------------------------------------------------------------------------- *)
(* Theorem destructors.                                                      *)
(* ------------------------------------------------------------------------- *)

fun dest (th : thm) = th;

fun axioms th =
    let
      val Thm {axioms = a, ...} = dest th
    in
      a
    end;

fun sequent th =
    let
      val Thm {sequent = s, ...} = dest th
    in
      s
    end;

fun hyp th = Sequent.hyp (sequent th);

fun concl th = Sequent.concl (sequent th);

(* ------------------------------------------------------------------------- *)
(* A total order on theorems modulo alpha equivalence.                       *)
(* ------------------------------------------------------------------------- *)

fun compare (th1,th2) =
    let
      val Thm {sequent = s1, ...} = th1
      and Thm {sequent = s2, ...} = th2
    in
      Sequent.compare (s1,s2)
    end;

fun equal th1 th2 = compare (th1,th2) = EQUAL;

fun dealphaCompare (th1,th2) =
    let
      val Thm {sequent = s1, ...} = th1
      and Thm {sequent = s2, ...} = th2
    in
      Sequent.dealphaCompare (s1,s2)
    end;

fun dealphaEqual th1 th2 = dealphaCompare (th1,th2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference.                                             *)
(* ------------------------------------------------------------------------- *)

val emptyAxioms = SequentSet.empty;

val singleAxiom = SequentSet.singleton;

val emptyHyp = TermAlphaSet.empty;

val singleHyp = TermAlphaSet.singleton;

fun axiom sequent =
    let
      val _ = Sequent.boolean sequent orelse
              raise Error "Thm.axiom: sequent is not boolean"

      val axioms = singleAxiom sequent
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun abs v th =
    let
      val Thm {axioms,sequent,...} = th

      val Sequent.Sequent {hyp,concl} = sequent

      val (a,b) = Term.destEq concl

      val fv =
          let
            fun add (t,z) = VarSet.union z (Term.freeVars t)
          in
            TermAlphaSet.foldl add VarSet.empty hyp
          end

      val _ = not (VarSet.member v fv) orelse
              raise Error "Thm.abs: free in hypothesis"

      val concl = Term.mkEq (Term.mkAbs (v,a), Term.mkAbs (v,b))

      val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun app th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2

      val Sequent.Sequent {hyp = h1, concl = c1} = s1
      and Sequent.Sequent {hyp = h2, concl = c2} = s2

      val (l1,r1) = Term.destEq c1
      and (l2,r2) = Term.destEq c2

      val axioms = SequentSet.union a1 a2
      and hyp = TermAlphaSet.union h1 h2
      and concl = Term.mkEq (Term.mkApp (l1,l2), Term.mkApp (r1,r2))

      val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun assume t =
    let
      val _ = Type.equal (Term.typeOf t) Type.bool orelse
              raise Error "Thm.assume: not a proposition"

      val axioms = emptyAxioms
      and hyp = singleHyp t
      and concl = t

      val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun betaConv t =
    let
      val (v,t1,t2) =
          case Term.dest t of
            TypeTerm.App' (t',t2) =>
            (case Term.dest t' of
               TypeTerm.Abs' (v,t1) => (v,t1,t2)
             | _ => raise Error "Thm.betaConv: term function not a lambda")
          | _ => raise Error "Thm.betaConv: term not a function application"

      val u =
          if Term.equalVar v t2 then t1
          else
            let
              val tmSubMap = TermSubst.singletonTermMap (v,t2)

              val subMap = (TypeSubst.emptyMap,tmSubMap)

              val sub = TermSubst.mk subMap
            in
              Option.getOpt (TermSubst.subst sub t1, t1)
            end

      val axioms = emptyAxioms
      and hyp = emptyHyp
      and concl = Term.mkEq (t,u)

      val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun deductAntisym th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2

      val Sequent.Sequent {hyp = h1, concl = c1} = s1
      and Sequent.Sequent {hyp = h2, concl = c2} = s2

      val h1 = TermAlphaSet.remove h1 c2
      and h2 = TermAlphaSet.remove h2 c1

      val axioms = SequentSet.union a1 a2
      and hyp = TermAlphaSet.union h1 h2
      and concl = Term.mkEq (c1,c2)

      val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun eqMp th1 th2 =
    let
      val Thm {axioms = a1, sequent = s1, ...} = th1
      and Thm {axioms = a2, sequent = s2, ...} = th2

      val Sequent.Sequent {hyp = h1, concl = c1} = s1
      and Sequent.Sequent {hyp = h2, concl = c2} = s2

      val axioms = SequentSet.union a1 a2
      and hyp = TermAlphaSet.union h1 h2

      val (c2',concl) = Term.destEq c1

      val _ = Term.alphaEqual c2 c2' orelse
              raise Error "Thm.eqMp: not alpha equivalent"

      val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun refl t =
    let
      val axioms = emptyAxioms
      and hyp = emptyHyp
      and concl = Term.mkEq (t,t)

      val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

fun subst sub th =
    let
      val Thm {axioms,sequent,...} = th

      val sequent = Option.getOpt (Sequent.subst sub sequent, sequent)
    in
      Thm {axioms = axioms, sequent = sequent}
    end;

(* ------------------------------------------------------------------------- *)
(* Definitions.                                                              *)
(* ------------------------------------------------------------------------- *)

fun defineConst name t =
    let
      val ty = Term.typeOf t

      val _ = VarSet.null (Term.freeVars t) orelse
              raise Error "Thm.defineConst: term not closed"

      val _ = NameSet.subset (Term.typeVars t) (Type.typeVars ty) orelse
              raise Error "Thm.defineConst: extra type variables in term"

      val c =
          TypeTerm.Const
            {name = name,
             prov = TypeTerm.DefProvConst (TypeTerm.DefConst t)}

      val axioms = emptyAxioms
      and hyp = emptyHyp
      and concl = Term.mkEq (Term.mkConst (c,ty), t)

      val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
    in
      (c, Thm {axioms = axioms, sequent = sequent})
    end;

fun defineTypeOp name {abs} {rep} tyVars existenceTh =
    let
      val Thm {axioms,sequent,...} = existenceTh

      val Sequent.Sequent {hyp,concl} = sequent

      val _ = TermAlphaSet.null hyp orelse
              raise Error "Thm.defineTypeOp: existence theorem must not have hypotheses"

      val (pTm,tTm) = Term.destApp concl

      val _ = VarSet.null (Term.freeVars pTm) orelse
              raise Error "Thm.defineTypeOp: predicate is not closed"

      val _ = NameSet.equal (NameSet.fromList tyVars) (Term.typeVars pTm) orelse
              raise Error "Thm.defineTypeOp: supplied type vars are not the type vars in p"

      val _ = NameSet.size (NameSet.fromList tyVars) = length tyVars orelse
              raise Error "Thm.defineTypeOp: supplied type variables contain duplicates"

      val prov =
          TypeTerm.DefOpTy
            {pred = pTm,
             vars = tyVars}

      val ot =
          TypeTerm.OpTy
            {name = name,
             prov = TypeTerm.DefProvOpTy prov}

      val absC =
          TypeTerm.Const
            {name = abs,
             prov = TypeTerm.AbsProvConst prov}

      val repC =
          TypeTerm.Const
            {name = rep,
             prov = TypeTerm.RepProvConst prov}

      val aTy = Type.mkOp (ot, map Type.mkVar tyVars)
      and rTy = Term.typeOf tTm

      val absTy = Type.mkFun (rTy,aTy)
      and repTy = Type.mkFun (aTy,rTy)

      val absTm = Term.mkConst (absC,absTy)
      and repTm = Term.mkConst (repC,repTy)

      val absRepTh =
          let
            val aVar = TypeTerm.Var (Name.mkGlobal "a", aTy)

            val aTm = Term.mkVar aVar

            val concl =
                Term.mkEq (Term.mkApp (absTm, Term.mkApp (repTm,aTm)), aTm)

            val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
          in
            Thm {axioms = axioms, sequent = sequent}
          end

      val repAbsTh =
          let
            val rVar = TypeTerm.Var (Name.mkGlobal "r", rTy)

            val rTm = Term.mkVar rVar

            val concl =
                Term.mkEq
                  (Term.mkApp (pTm,rTm),
                   Term.mkEq (Term.mkApp (repTm, Term.mkApp (absTm,rTm)), rTm))

            val sequent = Sequent.Sequent {hyp = hyp, concl = concl}
          in
            Thm {axioms = axioms, sequent = sequent}
          end
    in
      (ot, {abs = absC}, {rep = repC}, absRepTh,repAbsTh)
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

type grammar = Sequent.grammar;

val defaultGrammar =
    let
      val Sequent.Grammar
            {connective = _,
             hypGrammar,
             conclGrammar,
             showHyp} =
          Sequent.defaultGrammar

      val connective = "|-"
    in
      Sequent.Grammar
        {connective = connective,
         hypGrammar = hypGrammar,
         conclGrammar = conclGrammar,
         showHyp = showHyp}
    end;

fun ppWithGrammar gram =
    let
      val ppWS = Sequent.ppWithGrammar gram
    in
      fn show => ppWS show o sequent
    end;

val ppWithShow = ppWithGrammar defaultGrammar;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

end

structure ThmOrdered =
struct type t = Thm.thm val compare = Thm.compare end

structure ThmMap = KeyMap (ThmOrdered)

structure ThmSet =
struct

local
  structure S = ElementSet (ThmMap);
in
  open S;
end;

fun splitThm (th,(req,prov)) =
    let
      val Thm.Thm {axioms,sequent} = Thm.dest th

      val req = SequentSet.union req axioms

      val prov = SequentSet.add prov sequent
    in
      (req,prov)
    end;

val axioms =
    let
      fun add (th,acc) = SequentSet.union acc (Thm.axioms th)
    in
      foldl add SequentSet.empty
    end;

val sequents =
    let
      fun add (th,acc) = SequentSet.add acc (Thm.sequent th)
    in
      foldl add SequentSet.empty
    end;

end
