(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Rule :> Rule =
struct

structure N = Name;
structure NM = NameMap;
structure T = Term;
structure TS = TermSet;
structure TAS = TermAlphaSet;

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
(* Alpha conversion, a derived rule                                          *)
(* ------------------------------------------------------------------------- *)

fun alpha (h,c) th =
    let
      fun norm th = (TS.fromList (TAS.toList (hyp th)), th)

      fun check (t,(ts,th)) =
          if TS.member t ts then (ts,th)
          else
            let
              val th0 = assume t
              val th1 = deductAntisym th0 th
              val th = eqMp th1 th0
            in
              norm th
            end

(*TRACE5
      val _ =
          Parser.ppTrace
            (Parser.ppBinop " |-" (Parser.ppList ppTerm) ppTerm) "h |- c" (h,c)
      val _ = Parser.ppTrace ppThm "th" th
*)
      val th = if T.equal c (concl th) then th else eqMp (refl c) th
      val (_,th) = foldl check (norm th) h
      val _ = T.equal (concl th) c orelse
              raise Error "alpha: concl is wrong"
      val _ = TS.equal (TS.fromList h) (fst (norm th)) orelse
              raise Error "alpha: hyp is wrong"
    in
      th
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive rules, repeated from the logical kernel                         *)
(* ------------------------------------------------------------------------- *)

local
  datatype constDefs = ConstDefs of {t' : term, def' : thm} NM.map;

  val constDefs = ref (ConstDefs (NM.new ()));

  fun constDef n = case !constDefs of ConstDefs m => NM.peek m n;

  fun constDefAdd n_def =
      case !constDefs of
        ConstDefs m => constDefs := ConstDefs (NM.insert m n_def);
in
  fun defineConst name t =
      (case constDef name of
         NONE =>
         let
           val def = Thm.defineConst name t
           val () = constDefAdd (name, {t' = t, def' = def})
         in
           def
         end
       | SOME {t',def'} =>
         let
           val _ = T.alphaEqual t t' orelse
                   raise Error "redefinition not alpha equivalent"
         in
           alpha ([], mkEq (lhs (concl def'), t)) def'
         end)
      handle Error err => raise Error ("Rule.defineConst: " ^ err);
end;

local
  datatype typeDefs =
      TypeDefs of
        {abs' : name, rep' : name,
         tyVars' : name list, nonEmptyTh' : thm,
         absRepTh' : thm, repAbsTh' : thm} NM.map;

  val typeDefs = ref (TypeDefs (NM.new ()));

  fun typeDef n = case !typeDefs of TypeDefs m => NM.peek m n;

  fun typeDefAdd n_def =
      case !typeDefs of
        TypeDefs m => typeDefs := TypeDefs (NM.insert m n_def);
in
  fun defineType name {abs,rep} tyVars nonEmptyTh =
      (case typeDef name of
         NONE =>
         let
           val def as (absRepTh,repAbsTh) =
               Thm.defineType name {abs = abs, rep = rep} tyVars nonEmptyTh
           val info =
               {abs' = abs, rep' = rep,
                tyVars' = tyVars, nonEmptyTh' = nonEmptyTh,
                absRepTh' = absRepTh, repAbsTh' = repAbsTh}
           val () = typeDefAdd (name,info)
         in
           def
         end
       | SOME {abs',rep',tyVars',nonEmptyTh',absRepTh',repAbsTh'} =>
         let
           val _ = abs = abs' orelse
                   raise Error "redefinition with different abs"
           val _ = rep = rep' orelse
                   raise Error "redefinition with different rep"
           val _ = tyVars = tyVars' orelse
                   raise Error "redefinition with different tyVars"
           val _ = TAS.null (hyp nonEmptyTh) orelse
                   raise Error "existence theorem must not have hypotheses"
           val P = rator (concl nonEmptyTh)
           and P' = rator (concl nonEmptyTh')
           val _ = T.equal P P' orelse
                   raise Error "redefinition with different predicate"
         in
           (absRepTh',repAbsTh')
         end)
      handle Error err => raise Error ("Rule.defineType: " ^ err);
end;

(* ------------------------------------------------------------------------- *)
(* Derived rules                                                             *)
(* ------------------------------------------------------------------------- *)

fun trans th1 th2 =
    let
      val tm = rator (concl th1)
      val th3 = comb (refl tm) th2
    in
      eqMp th3 th1
    end;

fun define tm =
    let
      val (v,t) = destEq tm
      val (n,_) = destVar v
    in
      defineConst n t
    end;

end
