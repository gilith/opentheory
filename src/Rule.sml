(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Rule :> Rule =
struct

open Useful Syntax;

(* ------------------------------------------------------------------------- *)
(* Primitive rules, repeated from the logical kernel                         *)
(* ------------------------------------------------------------------------- *)

val axiom = Thm.axiom;

val abs = Thm.abs;

val assume = Thm.assume;

val betaConv = Thm.betaConv;

val deductAntisym = Thm.deductAntisym;

val eqMp = Thm.eqMp;

val subst = Thm.subst;

val comb = Thm.comb;

val refl = Thm.refl;

(* ------------------------------------------------------------------------- *)
(* Primitive rules, repeated from the logical kernel                         *)
(* ------------------------------------------------------------------------- *)

type constDef = {tm : Term.term, def : Thm.thm};

type typeDef =
     {abs : Name.name, rep : Name.name, tyVars : Name.name list,
      nonEmptyTh : Thm.thm, absRepTh : Thm.thm, repAbsTh : Thm.thm};

local
  datatype constDefs = ConstDefs of constDef NameMap.map;

  val constDefs = ref (ConstDefs (NameMap.new ()));

  fun constDefAdd n_def =
      case !constDefs of
        ConstDefs m => constDefs := ConstDefs (NameMap.insert m n_def);
in
  fun constDef n = case !constDefs of ConstDefs m => NameMap.peek m n;

  fun defineConst name tm =
      (case constDef name of
         NONE =>
         let
           val def = Thm.defineConst name tm
           val () = constDefAdd (name, {tm = tm, def = def})
         in
           def
         end
       | SOME {tm = tm', def} =>
         let
           val _ = Term.alphaEqual tm tm' orelse
                   raise Error "redefinition not alpha equivalent"
         in
           def
         end)
      handle Error err => raise Error ("Rule.defineConst: " ^ err);
end;

local
  datatype typeDefs = TypeDefs of typeDef NameMap.map;

  val typeDefs = ref (TypeDefs (NameMap.new ()));

  fun typeDefAdd n_def =
      case !typeDefs of
        TypeDefs m => typeDefs := TypeDefs (NameMap.insert m n_def);
in
  fun typeDef n = case !typeDefs of TypeDefs m => NameMap.peek m n;

  fun defineType name {abs,rep} tyVars nonEmptyTh =
      (case typeDef name of
         NONE =>
         let
           val def as (absRepTh,repAbsTh) =
               Thm.defineType name {abs = abs, rep = rep} tyVars nonEmptyTh
           val info =
               {abs = abs, rep = rep, tyVars = tyVars, nonEmptyTh = nonEmptyTh,
                absRepTh = absRepTh, repAbsTh = repAbsTh}
           val () = typeDefAdd (name,info)
         in
           def
         end
       | SOME {abs = abs', rep = rep', tyVars = tyVars',
               nonEmptyTh = nonEmptyTh', absRepTh, repAbsTh} =>
         let
           val _ = Name.equal abs abs' orelse
                   raise Error "redefinition with different abs"
           val _ = Name.equal rep rep' orelse
                   raise Error "redefinition with different rep"
           val _ = listEqual Name.equal tyVars tyVars' orelse
                   raise Error "redefinition with different tyVars"
           val P = rator (concl nonEmptyTh)
           and P' = rator (concl nonEmptyTh')
           val _ = Term.alphaEqual P P' orelse
                   raise Error "predicate not alpha equivalent"
         in
           (absRepTh,repAbsTh)
         end)
      handle Error err => raise Error ("Rule.defineType: " ^ err);
end;

(* ------------------------------------------------------------------------- *)
(* Derived rules.                                                            *)
(* ------------------------------------------------------------------------- *)

(* Alpha conversion *)

fun alpha seq th =
    let
      val dealpha = TermSet.fromList o TermAlphaSet.toList

      fun norm th = (dealpha (hyp th), th)

      fun check (t,(ts,th)) =
          if TermSet.member t ts then (ts,th)
          else
            let
              val th0 = assume t
              val th1 = deductAntisym th0 th
              val th = eqMp th1 th0
            in
              norm th
            end

(*OpenTheoryTrace5
      val _ = Print.trace ppSequent "seq" seq
      val _ = Print.trace ppThm "th" th
*)
      val {concl = c, hyp = h} = seq
      val th = if Term.equal c (concl th) then th else eqMp (refl c) th
      val (_,th) = TermAlphaSet.foldl check (norm th) h
      val _ = Term.equal (concl th) c orelse
              raise Error "concl is wrong"
      val _ = TermSet.equal (dealpha h) (dealpha (hyp th)) orelse
              raise Error "hyp is wrong"
    in
      th
    end
    handle Error err => raise Error ("Rule.alpha: " ^ err);

fun findAlpha set seq =
    case ThmSet.peek set (Thm.axiom seq) of
      SOME th => SOME (alpha seq th)
    | NONE => NONE;

(* Transitivity of equality *)

fun trans th1 th2 =
    let
      val tm = rator (concl th1)
      val th3 = comb (refl tm) th2
    in
      eqMp th3 th1
    end;

(* Constant definition by supplying the required theorem *)

fun define tm =
    let
      val (v,t) = destEq tm
      val n = Var.name (destVar v)
    in
      defineConst n t
    end;

end
