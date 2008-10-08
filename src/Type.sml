(* ========================================================================= *)
(* A MINIMAL HIGHER ORDER LOGIC KERNEL                                       *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Type :> Type =
struct

open Useful;

infixr ==

val op== = Portable.pointerEqual;

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic types.                                       *)
(* ------------------------------------------------------------------------- *)

datatype ty' =
    TypeVar of Name.name
  | TypeOp of Name.name * ty' list;

type ty = ty';

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare ty1_ty2 =
    if op== ty1_ty2 then EQUAL
    else
      case ty1_ty2 of
        (TypeVar n1, TypeVar n2) => Name.compare (n1,n2)
      | (TypeVar _, TypeOp _) => LESS
      | (TypeOp _, TypeVar _) => GREATER
      | (TypeOp n1_l1, TypeOp n2_l2) =>
        prodCompare Name.compare (lexCompare compare) (n1_l1,n2_l2);

fun equal ty1 ty2 = compare (ty1,ty2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* The type registry (initially contains the primitive type operators).      *)
(* ------------------------------------------------------------------------- *)

datatype registry =
    Registry of
      {all : Name.name list,
       arities : int NameMap.map};

val registry = ref (Registry {all = [], arities = NameMap.new ()});

fun typeArity name =
    let
      val Registry {arities,...} = !registry
    in
      NameMap.peek arities name
    end;

fun allTypes name =
    let
      val Registry {all,...} = !registry
    in
      all
    end;

fun declareType name arity =
    let
      val _ = not (Option.isSome (typeArity name)) orelse
              raise Error ("already a type operator with name " ^
                           Name.toString name)
      val Registry {all,arities} = !registry
      val all = name :: all
      and arities = NameMap.insert arities (name,arity)
    in
      registry := Registry {all = all, arities = arities}
    end
    handle Error err => raise Error ("Type.declareType: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk (ty : ty) = ty;

fun dest (ty : ty) = ty;

val mkVar = TypeVar;

fun destVar (TypeVar n) = n
  | destVar _ = raise Error "Type.destVar";

val isVar = can destVar;

fun equalVar name (TypeVar n) = Name.equal name n
  | equalVar _ _ = false;

fun mkOp (n,l) =
    let
      val () =
          case typeArity n of
            NONE => ()
          | SOME arity =>
            if length l = arity then ()
            else raise Error ("bad arity for type operator " ^ Name.toString n)
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

val alphaTy = mkVar (Name.mkGlobal "'a");

val typeVars =
    let
      fun fv (TypeVar n, acc) = NameSet.add acc n
        | fv (TypeOp (_,tys), acc) = foldl fv acc tys
    in
      fn ty => fv (ty,NameSet.empty)
    end;

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

val typeOps =
    let
      fun f (TypeVar _, acc) = acc
        | f (TypeOp (n,tys), acc) = foldl f (NameSet.add acc n) tys
    in
      fn ty => f (ty,NameSet.empty)
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive types                                                           *)
(* ------------------------------------------------------------------------- *)

local
  val n = Name.mkGlobal "bool";

  val () = declareType n 0;
in
  val boolTy = mkOp (n,[]);
end;

local
  val n = Name.mkGlobal "fun";

  val () = declareType n 2;
in
  fun mkFun (x,y) = mkOp (n,[x,y]);

  fun destFun ty =
      case destOp ty of
        (m,[x,y]) =>
        if Name.equal n m then (x,y)
        else raise Error "Type.destFun"
      | _ => raise Error "Type.destFun";

  val isFun = can destFun;
end;

end

structure TypeOrdered =
struct type t = Type.ty val compare = Type.compare end

structure TypeSet = ElementSet (TypeOrdered)

structure TypeMap = KeyMap (TypeOrdered)
