(* ========================================================================= *)
(* HIGHER ORDER LOGIC VARIABLES                                              *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Var :> Var =
struct

open Useful

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic term variables.                              *)
(* ------------------------------------------------------------------------- *)

datatype var = Var of Name.name * Type.ty;

(* ------------------------------------------------------------------------- *)
(* The name of a variable.                                                   *)
(* ------------------------------------------------------------------------- *)

fun name (Var (n,_)) = n;

(* ------------------------------------------------------------------------- *)
(* The type of a variable.                                                   *)
(* ------------------------------------------------------------------------- *)

fun typeOf (Var (_,ty)) = ty;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (Var n_ty1, Var n_ty2) =
    prodCompare Name.compare Type.compare (n_ty1,n_ty2);

fun equal (Var (n1,ty1)) (Var (n2,ty2)) =
    Name.equal n1 n2 andalso Type.equal ty1 ty2;

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

fun addSharingTypeVars tyShare (Var (_,ty)) =
    Type.addSharingTypeVars tyShare [ty];

fun typeVars (Var (_,ty)) = Type.typeVars ty;

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

fun addSharingTypeOps tyShare (Var (_,ty)) =
    Type.addSharingTypeOps tyShare [ty];

fun typeOps (Var (_,ty)) = Type.typeOps ty;

(* ------------------------------------------------------------------------- *)
(* Fresh variables.                                                          *)
(* ------------------------------------------------------------------------- *)

fun renameAvoiding avoid (v as Var (n,ty)) =
    if not (NameSet.member n avoid) then v
    else
      let
        fun acceptable n = not (NameSet.member n avoid)

        val n = Name.variantNum acceptable n
      in
        Var (n,ty)
      end;

(* ------------------------------------------------------------------------- *)
(* Type substitutions.                                                       *)
(* ------------------------------------------------------------------------- *)

fun sharingSubst (Var (n,ty)) tyShare =
    let
      val (ty',tyShare) = TypeSubst.sharingSubst ty tyShare
      val v' =
          case ty' of
            SOME ty => SOME (Var (n,ty))
          | NONE => NONE
    in
      (v',tyShare)
    end;

fun subst sub (Var (n,ty)) =
    case TypeSubst.subst sub ty of
      SOME ty => SOME (Var (n,ty))
    | NONE => NONE;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val showTypes = ref false;

val pp =
    let
      val pp1 = Print.ppBracket "(" ")" (Print.ppOp2 " :" Name.pp Type.pp)
      val pp2 = Print.ppMap fst Name.pp
    in
      fn Var n_ty => (if !showTypes then pp1 else pp2) n_ty
    end;

val toString = Print.toString pp;

end

structure VarSet =
ElementSet (struct type t = Var.var val compare = Var.compare end);

structure VarMap =
KeyMap (struct type t = Var.var val compare = Var.compare end);
