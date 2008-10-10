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

datatype ty =
    Type of
      {ty : ty',
       vars : NameSet.set,
       ops : NameSet.set}

and ty' =
    TypeVar of Name.name
  | TypeOp of Name.name * ty list;

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
(* Type variables                                                            *)
(* ------------------------------------------------------------------------- *)

fun typeVars (Type {vars,...}) = vars;

val typeVarsList =
    let
      fun add (ty,set) = NameSet.union set (typeVars ty)
    in
      foldl add NameSet.empty
    end;

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

fun typeOps (Type {ops,...}) = ops;

val typeOpsList =
    let
      fun add (ty,set) = NameSet.union set (typeOps ty)
    in
      foldl add NameSet.empty
    end;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* General *)

fun mk ty =
    case ty of
      TypeVar n =>
      let
        val vars = NameSet.singleton n
        val ops = NameSet.empty
      in
        Type
          {ty = ty,
           vars = vars,
           ops = ops}
      end
    | TypeOp (n,tys) =>
      let
        val () =
            case typeArity n of
              NONE => ()
            | SOME arity =>
              if length tys = arity then ()
              else raise Error ("Type.mk: bad arity for type operator " ^
                                Name.toString n)

        val vars = typeVarsList tys
        val ops = NameSet.add (typeOpsList tys) n
      in
        Type
          {ty = ty,
           vars = vars,
           ops = ops}
      end;

fun dest (Type {ty,...}) = ty;

(* Variables *)

fun mkVar' n = TypeVar n;

fun destVar' (TypeVar n) = n
  | destVar' _ = raise Error "Type.destVar'";

val isVar' = can destVar';

fun equalVar' name (TypeVar n) = Name.equal name n
  | equalVar' _ _ = false;

fun mkVar n = mk (mkVar' n);

fun destVar ty = destVar' (dest ty);

val isVar = can destVar;

fun equalVar name ty = equalVar' name (dest ty);

(* Operators *)

fun mkOp' n_tys = TypeOp n_tys;

fun destOp' (TypeOp n_tys) = n_tys
  | destOp' _ = raise Error "Type.destOp'";

val isOp' = can destOp';

fun mkOp n_tys = mk (mkOp' n_tys);

fun destOp ty = destOp' (dest ty);

val isOp = can destOp;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare ty1_ty2 =
    if op== ty1_ty2 then EQUAL
    else
      let
        val (Type {ty = ty1, ...}, Type {ty = ty2, ...}) = ty1_ty2
      in
        compare' (ty1,ty2)
      end

and compare' ty1_ty2 =
    case ty1_ty2 of
      (TypeVar n1, TypeVar n2) => Name.compare (n1,n2)
    | (TypeVar _, TypeOp _) => LESS
    | (TypeOp _, TypeVar _) => GREATER
    | (TypeOp n1_l1, TypeOp n2_l2) =>
      prodCompare Name.compare (lexCompare compare) (n1_l1,n2_l2);

fun equal ty1 ty2 = compare (ty1,ty2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Type variables                                                            *)
(* ------------------------------------------------------------------------- *)

val alphaTy = mkVar (Name.mkGlobal "'a");

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
