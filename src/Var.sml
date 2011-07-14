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

val listEqual = Useful.listEqual equal;

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
        fun avoidFn n = NameSet.member n avoid

        val n = Name.variantNum {avoid = avoidFn} n
      in
        TypeTerm.Var (n,ty)
      end;

(* ------------------------------------------------------------------------- *)
(* Type substitutions.                                                       *)
(* ------------------------------------------------------------------------- *)

fun sharingSubst (TypeTerm.Var (n,ty)) sub =
    let
      val (ty',sub) = TypeSubst.sharingSubst ty sub

      val v' =
          case ty' of
            SOME ty => SOME (TypeTerm.Var (n,ty))
          | NONE => NONE
    in
      (v',sub)
    end;

fun subst sub (TypeTerm.Var (n,ty)) =
    case TypeSubst.subst sub ty of
      SOME ty => SOME (TypeTerm.Var (n,ty))
    | NONE => NONE;

(* ------------------------------------------------------------------------- *)
(* Type rewrites.                                                            *)
(* ------------------------------------------------------------------------- *)

fun sharingRewrite (TypeTerm.Var (n,ty)) rewr =
    let
      val (ty',rewr) = TypeRewrite.sharingRewrite ty rewr

      val v' =
          case ty' of
            SOME ty => SOME (TypeTerm.Var (n,ty))
          | NONE => NONE
    in
      (v',rewr)
    end;

fun rewrite rewr (TypeTerm.Var (n,ty)) =
    case TypeRewrite.rewrite rewr ty of
      SOME ty => SOME (TypeTerm.Var (n,ty))
    | NONE => NONE;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap name Name.pp;

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

fun toHtml show =
    let
      val ppTy = Type.ppHtml show
    in
      fn var =>
         let
           val (name,ty) = dest var

           val attrs =
               let
                 val class = "var"

                 and title = Name.toString name ^ " : " ^ Print.toString ppTy ty
               in
                 Html.fromListAttrs [("class",class),("title",title)]
               end

           val inlines = Name.toHtml name
         in
           [Html.Span (attrs,inlines)]
         end
    end;

end

structure VarOrdered =
struct type t = Var.var val compare = Var.compare end

structure VarMap = KeyMap (VarOrdered)

structure VarSet = ElementSet (VarMap)
