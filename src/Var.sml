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

type var = Name.name * Type.ty;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = prodCompare Name.compare Type.compare;

fun equal (n1,ty1) (n2,ty2) = Name.equal n1 n2 andalso Type.equal ty1 ty2;

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

fun typeOps (_,ty) = Type.typeOps ty;

(* ------------------------------------------------------------------------- *)
(* Fresh variables.                                                          *)
(* ------------------------------------------------------------------------- *)

fun variant (n,ty) : var =
    let
      val (ns,s) = Name.dest n
      val s = s ^ "'"
      val n = Name.mk (ns,s)
    in
      (n,ty)
    end;

end

structure VarSet =
ElementSet (struct type t = Var.var val compare = Var.compare end);

structure VarMap =
KeyMap (struct type t = Var.var val compare = Var.compare end);
