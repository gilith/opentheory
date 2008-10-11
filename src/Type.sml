(* ========================================================================= *)
(* A MINIMAL HIGHER ORDER LOGIC KERNEL                                       *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Type :> Type =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic types.                                       *)
(* ------------------------------------------------------------------------- *)

type tyId = int;

datatype ty =
    Type of
      {id : tyId,
       ty : ty',
       sz : int}

and ty' =
    TypeVar of Name.name
  | TypeOp of Name.name * ty list;

(* ------------------------------------------------------------------------- *)
(* The type registry (initially contains the primitive type operators).      *)
(* ------------------------------------------------------------------------- *)

datatype registry =
    Registry of
      {all : NameSet.set,
       arities : int NameMap.map};

val emptyRegistry =
    let
      val all = NameSet.empty
      val arities = NameMap.new ()
    in
      Registry
        {all = all,
         arities = arities}
    end;

val theRegistry = ref emptyRegistry;

fun declaredArity name =
    let
      val Registry {arities,...} = !theRegistry
    in
      NameMap.peek arities name
    end;

fun allDeclared () =
    let
      val Registry {all,...} = !theRegistry
    in
      all
    end;

fun declare name arity =
    let
      val _ = not (NameSet.member name (allDeclared ())) orelse
              raise Error ("already a type operator with name " ^
                           Name.toString name)

      val Registry {all,arities} = !theRegistry

      val all = NameSet.add all name
      and arities = NameMap.insert arities (name,arity)

      val registry = Registry {all = all, arities = arities}
    in
      theRegistry := registry
    end
    handle Error err => raise Error ("Type.declareType: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

val newTypeId : unit -> tyId =
    let
      val counter = ref 0
    in
      fn () =>
         let
           val ref count = counter
           val () = counter := count + 1
         in
           count
         end
    end;

fun id (Type {id = i, ...}) = i;

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

fun size (Type {sz,...}) = sz;

val sizeList =
    let
      fun add (ty,n) = size ty + n
    in
      foldl add 0
    end;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* General *)

fun mk ty =
    case ty of
      TypeVar n =>
      let
        val id = newTypeId ()
        val sz = 1
      in
        Type
          {id = id,
           ty = ty,
           sz = sz}
      end
    | TypeOp (n,tys) =>
      let
        val () =
            case declaredArity n of
              NONE => ()
            | SOME arity =>
              if length tys = arity then ()
              else raise Error ("Type.mk: bad arity for type operator " ^
                                Name.toString n)

        val id = newTypeId ()
        val sz = sizeList tys + 1
      in
        Type
          {id = id,
           ty = ty,
           sz = sz}
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

fun compare (ty1,ty2) =
    let
      val Type {ty = ty1, id = id1, ...} = ty1
      and Type {ty = ty2, id = id2, ...} = ty2
    in
      if id1 = id2 then EQUAL else compare' (ty1,ty2)
    end

and compare' ty1_ty2 =
    case ty1_ty2 of
      (TypeVar n1, TypeVar n2) => Name.compare (n1,n2)
    | (TypeVar _, TypeOp _) => LESS
    | (TypeOp _, TypeVar _) => GREATER
    | (TypeOp n1_tys1, TypeOp n2_tys2) =>
      prodCompare Name.compare (lexCompare compare) (n1_tys1,n2_tys2);

fun equal ty1 ty2 = compare (ty1,ty2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

val alpha = mkVar (Name.mkGlobal "'a");

local
  fun calcVars _ acc [] = acc
    | calcVars seen acc (ty :: tys) =
      let
        val Type {id,ty,...} = ty
      in
        if IntSet.member id seen then calcVars seen acc tys
        else
          let
            val seen = IntSet.add seen id
          in
            calcVars' seen acc ty tys
          end
      end

  and calcVars' seen acc ty tys =
      case ty of
        TypeVar n =>
        let
          val acc = NameSet.add acc n
        in
          calcVars seen acc tys
        end
      | TypeOp (_,tys') =>
        let
          val tys = List.revAppend (tys',tys)
        in
          calcVars seen acc tys
        end;
in
  val typeVarsList = calcVars IntSet.empty NameSet.empty;

  fun typeVars ty = typeVarsList [ty];
end;

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun calcOps _ acc [] = acc
    | calcOps seen acc (ty :: tys) =
      let
        val Type {id,ty,...} = ty
      in
        if IntSet.member id seen then calcOps seen acc tys
        else
          let
            val seen = IntSet.add seen id
          in
            calcOps' seen acc ty tys
          end
      end

  and calcOps' seen acc ty tys =
      case ty of
        TypeVar _ => calcOps seen acc tys
      | TypeOp (n,tys') =>
        let
          val acc = NameSet.add acc n
          val tys = List.revAppend (tys',tys)
        in
          calcOps seen acc tys
        end;
in
  val typeOpsList = calcOps IntSet.empty NameSet.empty;

  fun typeOps ty = typeOpsList [ty];
end;

(* ------------------------------------------------------------------------- *)
(* Primitive types                                                           *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val boolString = "bool";

val boolName = Name.mkGlobal boolString;

val boolArity = 0;

val () = declare boolName boolArity;

val bool = mkOp (boolName,[]);

(* Function spaces *)

val funString = "fun";

val funName = Name.mkGlobal "fun";

val funArity = 2;

val () = declare funName funArity;

fun mkFun (x,y) = mkOp (funName,[x,y]);

fun destFun ty =
    case destOp ty of
      (n,[x,y]) =>
      if Name.equal n funName then (x,y)
      else raise Error "Type.destFun"
    | _ => raise Error "Type.destFun";

val isFun = can destFun;

end

structure TypeOrdered =
struct type t = Type.ty val compare = Type.compare end

structure TypeSet = ElementSet (TypeOrdered)

structure TypeMap = KeyMap (TypeOrdered)
