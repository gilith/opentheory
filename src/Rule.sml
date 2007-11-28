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

val Axiom = Thm.Axiom;

val Abs = Thm.Abs;

val Assume = Thm.Assume;

val Beta_conv = Thm.Beta_conv;

val Deduct_antisym = Thm.Deduct_antisym;

val Eq_mp = Thm.Eq_mp;

val Subst = Thm.Subst;

val Mk_comb = Thm.Mk_comb;

val Refl = Thm.Refl;

(* ------------------------------------------------------------------------- *)
(* Alpha conversion, a derived rule                                          *)
(* ------------------------------------------------------------------------- *)

fun Alpha (h,c) th =
    let
      fun norm th = (TS.fromList (TAS.toList (hyp th)), th)

      fun check (t,(ts,th)) =
          if TS.member t ts then (ts,th)
          else
            let
              val th0 = Assume t
              val th1 = Deduct_antisym th0 th
              val th = Eq_mp th1 th0
            in
              norm th
            end

(*TRACE5
      val _ =
          Parser.ppTrace
            (Parser.ppBinop " |-" (Parser.ppList pp_term) pp_term) "h |- c" (h,c)
      val _ = Parser.ppTrace pp_thm "th" th
*)
      val th = if T.equal c (concl th) then th else Eq_mp (Refl c) th
      val (_,th) = foldl check (norm th) h
      val _ = T.equal (concl th) c orelse
              raise Error "Alpha: concl is wrong"
      val _ = TS.equal (TS.fromList h) (fst (norm th)) orelse
              raise Error "Alpha: hyp is wrong"
    in
      th
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive rules, repeated from the logical kernel                         *)
(* ------------------------------------------------------------------------- *)

local
  datatype const_defs = ConstDefs of {t' : term, def' : thm} NM.map;

  val const_defs = ref (ConstDefs (NM.new ()));

  fun const_def n = case !const_defs of ConstDefs m => NM.peek m n;

  fun const_def_add n_x =
      case !const_defs of
        ConstDefs m => const_defs := ConstDefs (NM.insert m n_x);
in
  fun Define_const name t =
      (case const_def name of
         NONE =>
         let
           val def = Thm.Define_const name t
           val () = const_def_add (name, {t' = t, def' = def})
         in
           def
         end
       | SOME {t',def'} =>
         let
           val _ = T.alpha_equal t t' orelse
                   raise Error "redefinition not alpha equivalent"
         in
           Alpha ([], mk_eq (lhs (concl def'), t)) def'
         end)
      handle Error err => raise Error ("Rule.Define_const: " ^ err);
end;

local
  datatype type_defs =
           TypeDefs of {abs' : name, rep' : name,
                        ty_vars' : name list, non_empty_th' : thm,
                        abs_rep_th' : thm, rep_abs_th' : thm} NM.map;

  val type_defs = ref (TypeDefs (NM.new ()));

  fun type_def n = case !type_defs of TypeDefs m => NM.peek m n;

  fun type_def_add n_def =
      case !type_defs of
        TypeDefs m => type_defs := TypeDefs (NM.insert m n_def);
in
  fun Define_type name {abs,rep} ty_vars non_empty_th =
      (case type_def name of
         NONE =>
         let
           val def as (abs_rep_th,rep_abs_th) =
               Thm.Define_type name {abs = abs, rep = rep} ty_vars non_empty_th
           val info =
               {abs' = abs, rep' = rep,
                ty_vars' = ty_vars, non_empty_th' = non_empty_th,
                abs_rep_th' = abs_rep_th, rep_abs_th' = rep_abs_th}
           val () = type_def_add (name,info)
         in
           def
         end
       | SOME {abs',rep',ty_vars',non_empty_th',abs_rep_th',rep_abs_th'} =>
         let
           val _ = abs = abs' orelse
                   raise Error "redefinition with different abs"
           val _ = rep = rep' orelse
                   raise Error "redefinition with different rep"
           val _ = ty_vars = ty_vars' orelse
                   raise Error "redefinition with different ty_vars"
           val _ = TAS.null (hyp non_empty_th) orelse
                   raise Error "existence theorem must not have hypotheses"
           val P = rator (concl non_empty_th)
           and P' = rator (concl non_empty_th')
           val _ = T.equal P P' orelse
                   raise Error "redefinition with different predicate"
         in
           (abs_rep_th',rep_abs_th')
         end)
      handle Error err => raise Error ("Rule.Define_type: " ^ err);
end;

(* ------------------------------------------------------------------------- *)
(* Derived rules                                                             *)
(* ------------------------------------------------------------------------- *)

fun Trans th1 th2 =
    let
      val tm = rator (concl th1)
      val th3 = Mk_comb (Refl tm) th2
    in
      Eq_mp th3 th1
    end;

fun Define tm =
    let
      val (v,t) = dest_eq tm
      val (n,_) = dest_var v
    in
      Define_const n t
    end;

end
