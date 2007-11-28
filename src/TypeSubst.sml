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

datatype subst = Type_subst of Ty.ty NM.map;

val empty = Type_subst (NM.new ());

fun null (Type_subst m) = NM.null m;

fun add n_ty (Type_subst sub) = Type_subst (NM.insert sub n_ty);

fun peek (Type_subst sub) n = NM.peek sub n;

fun norm (Type_subst sub) =
    let
      fun f (n,ty,z) = if Type.equal_var n ty then z else NM.insert z (n,ty)

      val sub = NM.foldl f (NM.new ()) sub
    in
      Type_subst sub
    end;

fun subst sub =
    let
      val sub as Type_subst m = norm sub

      fun f ty =
          case Ty.dest ty of
            Ty.Type_var n => Option.getOpt (NM.peek m n, ty)
          | Ty.Type_op (n,l) =>
            let
              val l' = Sharing.map f l
            in
              if l == l' then ty else Ty.mk_op (n,l')
            end
    in
      if null sub then I else f
    end;

fun to_list (Type_subst sub) = NM.toList sub;

(* ------------------------------------------------------------------------- *)
(* Matching                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun raw_match sub [] = sub
    | raw_match sub ((ty1,ty2) :: rest) =
      case Ty.dest ty1 of
        Ty.Type_var n1 =>
        (case peek sub n1 of
           NONE => add (n1,ty2) sub
         | SOME ty2' =>
           if Ty.equal ty2 ty2' then raw_match sub rest
           else raise Error "incompatible variable substitutions")
      | Ty.Type_op (n1,l1) =>
        let
          val (n2,l2) = Ty.dest_op ty2
        in
          if n1 = n2 then raw_match sub (zip l1 l2 @ rest)
          else raise Error "different type operators"
        end;
in
  fun match_list' sub tyl =
      raw_match sub tyl
      handle Error err => raise Error ("Typesubst.match_list': " ^ err);

  fun match_list tyl =
      raw_match empty tyl
      handle Error err => raise Error ("Typesubst.match_list: " ^ err);

  fun match' sub ty1 ty2 =
      raw_match sub [(ty1,ty2)]
      handle Error err => raise Error ("Typesubst.match': " ^ err);

  fun match ty1 ty2 =
      raw_match empty [(ty1,ty2)]
      handle Error err => raise Error ("Typesubst.match: " ^ err);
end;

end
