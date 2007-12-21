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

datatype ty' = TypeVar of N.name | TypeOp of N.name * ty' list;

type ty = ty';

(* ------------------------------------------------------------------------- *)
(* A total order                                                             *)
(* ------------------------------------------------------------------------- *)

fun compare ty1_ty2 =
    if op== ty1_ty2 then EQUAL
    else
      case ty1_ty2 of
        (TypeVar n1, TypeVar n2) => N.compare (n1,n2)
      | (TypeVar _, TypeOp _) => LESS
      | (TypeOp _, TypeVar _) => GREATER
      | (TypeOp n1_l1, TypeOp n2_l2) =>
        prodCompare N.compare (lexCompare compare) (n1_l1,n2_l2);

fun equal (x : ty) y = x = y;
 
(* ------------------------------------------------------------------------- *)
(* The type registry (initially contains the primitive type operators)       *)
(* ------------------------------------------------------------------------- *)

datatype registry = Registry of {all : N.name list, arities : int NM.map};

val registry = ref (Registry {all = [], arities = NM.new ()});

fun typeArity name =
    let
      val Registry {arities,...} = !registry
    in
      case NM.peek arities name of
        NONE => raise Error ("typeArity: no type operator with name "^name)
      | SOME arity => arity
    end;

fun allTypes name =
    let
      val Registry {all,...} = !registry
    in
      all
    end;

fun declareType name arity =
    let
      val _ = not (can typeArity name) orelse
              raise Error ("already a type operator with name " ^ name)
      val Registry {all,arities} = !registry
      val all = name :: all
      and arities = NM.insert arities (name,arity)
    in
      registry := Registry {all = all, arities = arities}
    end
    handle Error err => raise Error ("Type.declareType: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors                                              *)
(* ------------------------------------------------------------------------- *)

fun mk (ty : ty) = ty;

fun dest (ty : ty) = ty;

val mkVar = TypeVar;

fun destVar (TypeVar n) = n
  | destVar _ = raise Error "Type.destVar";

val isVar = can destVar;

fun equalVar name (TypeVar n) = name = n
  | equalVar _ _ = false;

fun mkOp (n,l) =
    let
      val arity = typeArity n
      val _ = length l = arity orelse
              raise Error ("bad arity for type operator " ^ n)
    in
      TypeOp (n,l)
    end
    handle Error err => raise Error ("Type.mkOp: " ^ err);

fun destOp (TypeOp n_l) = n_l
  | destOp _ = raise Error "Type.destOp";

val isOp = can destOp;

(* ------------------------------------------------------------------------- *)
(* Type variables                                                            *)
(* ------------------------------------------------------------------------- *)

val typeVars =
    let
      fun fv (TypeVar n, acc) = NS.add acc n
        | fv (TypeOp (_,tys), acc) = foldl fv acc tys
    in
      fn ty => fv (ty,NS.empty)
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive types                                                           *)
(* ------------------------------------------------------------------------- *)

val alphaTy = mkVar "'a";

local
  val n = "bool";

  val () = declareType n 0;
in
  val boolTy = mkOp (n,[]);
end;

local
  val n = "fun";

  val () = declareType n 2;
in
  fun mkFun (x,y) = mkOp (n,[x,y]);

  fun destFun ty =
      case destOp ty of
        (m,[x,y]) => if n = m then (x,y) else raise Error "Type.destFun"
      | _ => raise Error "Type.destFun";

  val isFun = can destFun;
end;

local
  val n = "ind";

  val () = declareType n 0;
in
  val indTy = mkOp (n,[]);
end;

end

structure TypeOrdered =
struct type t = Type.ty val compare = Type.compare end

structure TypeSet =
ElementSet (TypeOrdered)

structure TypeMap =
KeyMap (TypeOrdered)
