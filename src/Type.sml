(* ========================================================================= *)
(* A MINIMAL HIGHER ORDER LOGIC KERNEL                                       *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Type :> Type =
struct

open Useful;

structure N = Name;
structure NS = NameSet;
structure NM = NameMap;

infixr ==

val op== = Portable.pointerEqual;

(* ------------------------------------------------------------------------- *)
(* Types                                                                     *)
(* ------------------------------------------------------------------------- *)

datatype ty' = Type_var of N.name | Type_op of N.name * ty' list;

type ty = ty';

(* ------------------------------------------------------------------------- *)
(* A total order                                                             *)
(* ------------------------------------------------------------------------- *)

fun compare ty1_ty2 =
    if op== ty1_ty2 then EQUAL
    else
      case ty1_ty2 of
        (Type_var n1, Type_var n2) => N.compare (n1,n2)
      | (Type_var _, Type_op _) => LESS
      | (Type_op _, Type_var _) => GREATER
      | (Type_op n1_l1, Type_op n2_l2) =>
        prodCompare N.compare (lexCompare compare) (n1_l1,n2_l2);

fun equal (x : ty) y = x = y;
 
(* ------------------------------------------------------------------------- *)
(* The type registry (initially contains the primitive type operators)       *)
(* ------------------------------------------------------------------------- *)

datatype registry = Registry of {all : N.name list, arities : int NM.map};

val registry = ref (Registry {all = [], arities = NM.new ()});

fun type_arity name =
    let
      val Registry {arities,...} = !registry
    in
      case NM.peek arities name of
        NONE => raise Error ("type_arity: no type operator with name "^name)
      | SOME arity => arity
    end;

fun all_types name =
    let
      val Registry {all,...} = !registry
    in
      all
    end;

fun declare_type name arity =
    let
      val _ = not (can type_arity name) orelse
              raise Error ("already a type operator with name " ^ name)
      val Registry {all,arities} = !registry
      val all = name :: all
      and arities = NM.insert arities (name,arity)
    in
      registry := Registry {all = all, arities = arities}
    end
    handle Error err => raise Error ("Type.declare_type: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors                                              *)
(* ------------------------------------------------------------------------- *)

fun mk (ty : ty) = ty;

fun dest (ty : ty) = ty;

val mk_var = Type_var;

fun dest_var (Type_var n) = n
  | dest_var _ = raise Error "Type.dest_var";

val is_var = can dest_var;

fun equal_var name (Type_var n) = name = n
  | equal_var _ _ = false;

fun mk_op (n,l) =
    let
      val arity = type_arity n
      val _ = length l = arity orelse
              raise Error ("bad arity for type operator " ^ n)
    in
      Type_op (n,l)
    end
    handle Error err => raise Error ("Type.mk_op: " ^ err);

fun dest_op (Type_op n_l) = n_l
  | dest_op _ = raise Error "Type.dest_op";

val is_op = can dest_op;

(* ------------------------------------------------------------------------- *)
(* Type variables                                                            *)
(* ------------------------------------------------------------------------- *)

val type_vars =
    let
      fun fv (Type_var n, acc) = NS.add acc n
        | fv (Type_op (_,tys), acc) = foldl fv acc tys
    in
      fn ty => fv (ty,NS.empty)
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive types                                                           *)
(* ------------------------------------------------------------------------- *)

val alpha = mk_var "'a";

local
  val n = "bool";

  val () = declare_type n 0;
in
  val bool = mk_op (n,[]);
end;

local
  val n = "fun";

  val () = declare_type n 2;
in
  fun mk_fun (x,y) = mk_op (n,[x,y]);

  fun dest_fun ty =
      case dest_op ty of
        (m,[x,y]) => if n = m then (x,y) else raise Error "Type.dest_fun"
      | _ => raise Error "Type.dest_fun";

  val is_fun = can dest_fun;
end;

local
  val n = "ind";

  val () = declare_type n 0;
in
  val ind = mk_op (n,[]);
end;

end

structure TypeSet =
ElementSet (struct type t = Type.ty val compare = Type.compare end);

structure TypeMap =
KeyMap (struct type t = Type.ty val compare = Type.compare end);
