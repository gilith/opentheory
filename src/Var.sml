(* ========================================================================= *)
(* HIGHER ORDER LOGIC VARIABLES                                              *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Var :> Var =
struct

open Useful

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic term variables.                              *)
(* ------------------------------------------------------------------------- *)

type var = TypeTerm.var;

val mk = TypeTerm.Var;

fun dest (TypeTerm.Var n_ty) = n_ty;

(* ------------------------------------------------------------------------- *)
(* The name of a variable.                                                   *)
(* ------------------------------------------------------------------------- *)

val name = TypeTerm.nameVar;

(* ------------------------------------------------------------------------- *)
(* The type of a variable.                                                   *)
(* ------------------------------------------------------------------------- *)

val typeOf = TypeTerm.typeOfVar;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = TypeTerm.compareVar;

val equal = TypeTerm.equalVar;

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

fun addSharingTypeVars v tyShare =
    Type.addSharingTypeVars (typeOf v) tyShare;

fun typeVars v = Type.typeVars (typeOf v);

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

fun addSharingTypeOps v tyShare =
    Type.addSharingTypeOps (typeOf v) tyShare;

fun typeOps v = Type.typeOps (typeOf v);

(* ------------------------------------------------------------------------- *)
(* Fresh variables.                                                          *)
(* ------------------------------------------------------------------------- *)

fun renameAvoiding avoid (v as TypeTerm.Var (n,ty)) =
    if not (NameSet.member n avoid) then v
    else
      let
        fun acceptable n = not (NameSet.member n avoid)

        val n = Name.variantNum acceptable n
      in
        TypeTerm.Var (n,ty)
      end;

(* ------------------------------------------------------------------------- *)
(* Type substitutions.                                                       *)
(* ------------------------------------------------------------------------- *)

fun sharingSubst (TypeTerm.Var (n,ty)) tyShare =
    let
      val (ty',tyShare) = TypeSubst.sharingSubst ty tyShare

      val v' =
          case ty' of
            SOME ty => SOME (TypeTerm.Var (n,ty))
          | NONE => NONE
    in
      (v',tyShare)
    end;

fun subst sub (TypeTerm.Var (n,ty)) =
    case TypeSubst.subst sub ty of
      SOME ty => SOME (TypeTerm.Var (n,ty))
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
      fn TypeTerm.Var n_ty => (if !showTypes then pp1 else pp2) n_ty
    end;

val toString = Print.toString pp;

end

structure VarOrdered =
struct type t = Var.var val compare = Var.compare end

structure VarMap = KeyMap (VarOrdered)

structure VarSet = ElementSet (VarMap)
