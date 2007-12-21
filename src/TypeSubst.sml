(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TYPES                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure TypeSubst :> TypeSubst =
struct

open Useful;

structure N = Name;
structure NM = NameMap;
structure NS = NameSet;
structure Ty = Type;

infixr ==

val op== = Portable.pointerEqual;

(* ------------------------------------------------------------------------- *)
(* Substitutions                                                             *)
(* ------------------------------------------------------------------------- *)

datatype subst = TypeSubst of Ty.ty NM.map;

val empty = TypeSubst (NM.new ());

fun null (TypeSubst m) = NM.null m;

fun add n_ty (TypeSubst sub) = TypeSubst (NM.insert sub n_ty);

fun peek (TypeSubst sub) n = NM.peek sub n;

fun norm (TypeSubst sub) =
    let
      fun f (n,ty,z) = if Type.equalVar n ty then z else NM.insert z (n,ty)

      val sub = NM.foldl f (NM.new ()) sub
    in
      TypeSubst sub
    end;

fun subst sub =
    let
      val sub as TypeSubst m = norm sub

      fun f ty =
          case Ty.dest ty of
            Ty.TypeVar n => Option.getOpt (NM.peek m n, ty)
          | Ty.TypeOp (n,l) =>
            let
              val l' = Sharing.map f l
            in
              if l == l' then ty else Ty.mkOp (n,l')
            end
    in
      if null sub then I else f
    end;

fun toList (TypeSubst sub) = NM.toList sub;

(* ------------------------------------------------------------------------- *)
(* Matching                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun rawMatch sub [] = sub
    | rawMatch sub ((ty1,ty2) :: rest) =
      case Ty.dest ty1 of
        Ty.TypeVar n1 =>
        (case peek sub n1 of
           NONE => add (n1,ty2) sub
         | SOME ty2' =>
           if Ty.equal ty2 ty2' then rawMatch sub rest
           else raise Error "incompatible variable substitutions")
      | Ty.TypeOp (n1,l1) =>
        let
          val (n2,l2) = Ty.destOp ty2
        in
          if n1 = n2 then rawMatch sub (zip l1 l2 @ rest)
          else raise Error "different type operators"
        end;
in
  fun matchList' sub tyl =
      rawMatch sub tyl
      handle Error err => raise Error ("TypeSubst.matchList': " ^ err);

  fun matchList tyl =
      rawMatch empty tyl
      handle Error err => raise Error ("TypeSubst.matchList: " ^ err);

  fun match' sub ty1 ty2 =
      rawMatch sub [(ty1,ty2)]
      handle Error err => raise Error ("TypeSubst.match': " ^ err);

  fun match ty1 ty2 =
      rawMatch empty [(ty1,ty2)]
      handle Error err => raise Error ("TypeSubst.match: " ^ err);
end;

end
