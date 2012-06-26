(* ========================================================================= *)
(* REWRITING HIGHER ORDER LOGIC TERMS                                        *)
(* Copyright (c) 2010 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure TermRewrite :> TermRewrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Bottom-up term rewrites: return NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    Rewrite of
      {tyRewr : TypeRewrite.rewrite,
       apply : Term.term' -> Term.term option,
       seen : Term.term option IntMap.map};

fun new tyRewr apply =
    let
      val seen = IntMap.new ()
    in
      Rewrite
        {tyRewr = tyRewr,
         apply = apply,
         seen = seen}
    end;

val id = new TypeRewrite.id (K NONE);

local
  fun undefTerm tm' =
      case tm' of
        TypeTerm.Const' (c,ty) =>
        if Const.isUndef c then NONE
        else SOME (Term.mkConst (Const.mkUndef (Const.name c), ty))
      | _ => NONE;
in
  val undef = new TypeRewrite.undef undefTerm;
end;

(* ------------------------------------------------------------------------- *)
(* The bottom-up traversal.                                                  *)
(* ------------------------------------------------------------------------- *)

fun rewriteTm apply tm tyRewr seen =
    let
      val i = Term.id tm
    in
      case IntMap.peek seen i of
        SOME tm' => (tm',tyRewr,seen)
      | NONE =>
        let
          val (tm',tyRewr,seen) = rewriteTm' apply (Term.dest tm) tyRewr seen

          val seen = IntMap.insert seen (i,tm')
        in
          (tm',tyRewr,seen)
        end
    end

and rewriteTm' apply tm' tyRewr seen =
    case tm' of
      TypeTerm.Const' (c,ty) =>
      let
        val (ty',tyRewr) = TypeRewrite.sharingRewriteType ty tyRewr

        val (unchanged,tm') =
            case ty' of
              SOME ty => (false, TypeTerm.Const' (c,ty))
            | NONE => (true,tm')

        val result = apply tm'

        val result =
            if unchanged orelse Option.isSome result then result
            else SOME (Term.mk tm')
      in
        (result,tyRewr,seen)
      end
    | TypeTerm.Var' v =>
      let
        val (v',tyRewr) = Var.sharingRewrite v tyRewr

        val (unchanged,tm') =
            case v' of
              SOME v => (false, TypeTerm.Var' v)
            | NONE => (true,tm')

        val result = apply tm'

        val result =
            if unchanged orelse Option.isSome result then result
            else SOME (Term.mk tm')
      in
        (result,tyRewr,seen)
      end
    | TypeTerm.App' (f,a) =>
      let
        val (f',tyRewr,seen) = rewriteTm apply f tyRewr seen

        val (a',tyRewr,seen) = rewriteTm apply a tyRewr seen

        val (unchanged,tm') =
            case (f',a') of
              (SOME f, SOME a) => (false, TypeTerm.App' (f,a))
            | (SOME f, NONE) => (false, TypeTerm.App' (f,a))
            | (NONE, SOME a) => (false, TypeTerm.App' (f,a))
            | (NONE,NONE) => (true,tm')

        val result = apply tm'

        val result =
            if unchanged orelse Option.isSome result then result
            else SOME (Term.mk tm')
      in
        (result,tyRewr,seen)
      end
    | TypeTerm.Abs' (v,b) =>
      let
        val (v',tyRewr) = Var.sharingRewrite v tyRewr

        val (b',tyRewr,seen) = rewriteTm apply b tyRewr seen

        val (unchanged,tm') =
            case (v',b') of
              (SOME v, SOME b) => (false, TypeTerm.Abs' (v,b))
            | (SOME v, NONE) => (false, TypeTerm.Abs' (v,b))
            | (NONE, SOME b) => (false, TypeTerm.Abs' (v,b))
            | (NONE,NONE) => (true,tm')

        val result = apply tm'

        val result =
            if unchanged orelse Option.isSome result then result
            else SOME (Term.mk tm')
      in
        (result,tyRewr,seen)
      end;

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

(* Lists *)

fun sharingRewriteList srX xs rewr =
    case xs of
      [] => (NONE,rewr)
    | x :: xs =>
      let
        val (x',rewr) = srX x rewr

        val (xs',rewr) = sharingRewriteList srX xs rewr

        val result =
            case x' of
              SOME x => SOME (x :: Option.getOpt (xs',xs))
            | NONE =>
              case xs' of
                SOME xs => SOME (x :: xs)
              | NONE => NONE
      in
        (result,rewr)
      end;

fun rewriteList srX rewr xs =
    let
      val (xs',_) = sharingRewriteList srX xs rewr
    in
      xs'
    end;

(* Types *)

fun sharingRewriteType ty rewr =
    let
      val Rewrite {tyRewr,apply,seen} = rewr

      val (ty',tyRewr) = TypeRewrite.sharingRewriteType ty tyRewr

      val rewr = Rewrite {tyRewr = tyRewr, apply = apply, seen = seen}
    in
      (ty',rewr)
    end;

fun rewriteType rewr ty =
    let
      val Rewrite {tyRewr,...} = rewr
    in
      TypeRewrite.rewriteType tyRewr ty
    end;

(* Variables *)

fun sharingRewriteVar v rewr =
    let
      val Rewrite {tyRewr,apply,seen} = rewr

      val (v',tyRewr) = Var.sharingRewrite v tyRewr

      val rewr = Rewrite {tyRewr = tyRewr, apply = apply, seen = seen}
    in
      (v',rewr)
    end;

fun rewriteVar rewr v =
    let
      val Rewrite {tyRewr,...} = rewr
    in
      Var.rewrite tyRewr v
    end;

(* Terms *)

fun sharingRewriteTerm tm rewr =
    let
      val Rewrite {tyRewr,apply,seen} = rewr

      val (tm',tyRewr,seen) = rewriteTm apply tm tyRewr seen

      val rewr = Rewrite {tyRewr = tyRewr, apply = apply, seen = seen}
    in
      (tm',rewr)
    end;

fun rewriteTerm rewr tm =
    let
      val Rewrite {tyRewr,apply,seen} = rewr

      val (tm',_,_) = rewriteTm apply tm tyRewr seen
    in
      tm'
    end;

(* Term lists *)

val sharingRewriteTermList = sharingRewriteList sharingRewriteTerm;

val rewriteTermList = rewriteList sharingRewriteTerm;

(* Term sets *)

local
  fun add (tm,(tms,unchanged,rewr)) =
      let
        val (tm',rewr) = sharingRewriteTerm tm rewr

        val (tms,unchanged) =
            case tm' of
              SOME tm => (tm :: tms, false)
            | NONE => (tm :: tms, unchanged)
      in
        (tms,unchanged,rewr)
      end;
in
  fun sharingRewriteTermAlphaSet set rewr =
      let
        val (tms,unchanged,rewr) = TermAlphaSet.foldl add ([],true,rewr) set

        val set' = if unchanged then NONE else SOME (TermAlphaSet.fromList tms)
      in
        (set',rewr)
      end;
end;

fun rewriteTermAlphaSet rewr set =
    let
      val (set',_) = sharingRewriteTermAlphaSet set rewr
    in
      set'
    end;

end
