(* ========================================================================= *)
(* REWRITING HIGHER ORDER LOGIC TYPES                                        *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure TypeRewrite :> TypeRewrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Bottom-up type rewrites: return NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    Rewrite of
      {apply : Type.ty' -> Type.ty option,
       seen : Type.ty option IntMap.map};

fun new apply =
    let
      val seen = IntMap.new ()
    in
      Rewrite
        {apply = apply,
         seen = seen}
    end;

(* ------------------------------------------------------------------------- *)
(* The bottom-up traversal.                                                  *)
(* ------------------------------------------------------------------------- *)

fun rewriteTy apply ty seen =
    let
      val i = Type.id ty
    in
      case IntMap.peek seen i of
        SOME ty' => (ty',seen)
      | NONE =>
        let
          val (ty',seen) = rewriteTy' apply (Type.dest ty) seen

          val seen = IntMap.insert seen (i,ty')
        in
          (ty',seen)
        end
    end

and rewriteTy' apply ty' seen =
    case ty' of
      TypeTerm.VarTy' _ => (apply ty', seen)
    | TypeTerm.OpTy' (ot,tys) =>
      let
        val (tys',seen) = rewriteTyList apply tys seen

        val (unchanged,ty') =
            case tys' of
              SOME tys => (false, TypeTerm.OpTy' (ot,tys))
            | NONE => (true,ty')

        val result = apply ty'

        val result =
            if unchanged orelse Option.isSome result then result
            else SOME (Type.mk ty')
      in
        (result,seen)
      end

and rewriteTyList apply tys seen =
    case tys of
      [] => (NONE,seen)
    | ty :: tys =>
      let
        val (ty',seen) = rewriteTy apply ty seen

        val (tys',seen) = rewriteTyList apply tys seen

        val result =
            case tys' of
              SOME tys => SOME (Option.getOpt (ty',ty) :: tys)
            | NONE =>
              case ty' of
                NONE => NONE
              | SOME ty => SOME (ty :: tys)
      in
        (result,seen)
      end;

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

fun sharingRewrite ty rewr =
    let
      val Rewrite {apply,seen} = rewr

      val (ty',seen) = rewriteTy apply ty seen

      val rewr = Rewrite {apply = apply, seen = seen}
    in
      (ty',rewr)
    end;

fun rewrite rewr ty =
    let
      val Rewrite {apply,seen} = rewr

      val (ty',_) = rewriteTy apply ty seen
    in
      ty'
    end;

end
