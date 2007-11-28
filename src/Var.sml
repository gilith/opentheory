(* ========================================================================= *)
(* HIGHER ORDER LOGIC VARIABLES                                              *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Var :> Var =
struct

open Useful

type var = Name.name * Type.ty;

val compare = prodCompare Name.compare Type.compare;

fun equal (n1,ty1) (n2,ty2) = n1 = n2 andalso Type.equal ty1 ty2;

fun variant (n,ty) : var = (n ^ "'", ty);

end

structure VarSet =
ElementSet (struct type t = Var.var val compare = Var.compare end);

structure VarMap =
KeyMap (struct type t = Var.var val compare = Var.compare end);
