(* ========================================================================= *)
(* HIGHER ORDER LOGIC VARIABLES                                              *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Var =
sig

type var = Name.name * Type.ty

val compare : var * var -> order

val equal : var -> var -> bool

val variant : var -> var

end
