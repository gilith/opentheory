(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES AND TERMS                                        *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure TypeTerm :> TypeTerm =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type idTy = int;

val newIdTy : unit -> idTy =
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

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = int;

val newId : unit -> id =
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

(* ------------------------------------------------------------------------- *)
(* The mutually recursive datatype of higher order logic types and terms.    *)
(* ------------------------------------------------------------------------- *)

datatype ty =
    Ty of
      {id : idTy,
       ty : ty',
       sz : int}

and ty' =
    VarTy of Name.name
  | OpTy of Name.name * ty list * opTy

and opTy =
    UndefTy
  | DefTy of defOpTy

and defOpTy =
    DefOpTy of
      {pred : term,
       vars : Name.name list}

and var =
    VarV of Name.name * ty

and term =
    Term of
      {id : id,
       tm : term',
       sz : int,
       ty : ty}

and term' =
    Const of Name.name * ty * const
  | Var of var
  | App of term * term
  | Abs of var * term

and const =
    Undef
  | Def of defConst
  | AbsTy of defOpTy
  | RepTy of defOpTy

and defConst =
    DefConst of term;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compareTy (ty1,ty2) =
    let
      val Type {id = id1, ty = ty1, sz = sz1} = ty1
      and Type {id = id2, ty = ty2, sz = sz2} = ty2
    in
      if id1 = id2 then EQUAL
      else
        case Int.compare (sz1,sz2) of
          LESS => LESS
        | EQUAL => compareTy' (ty1,ty2)
        | GREATER => GREATER
    end

and compareTy' ty1_ty2 =
    case ty1_ty2 of
      (TypeVar n1, TypeVar n2) => Name.compare (n1,n2)
    | (TypeVar _, TypeOp _) => LESS
    | (TypeOp _, TypeVar _) => GREATER
    | (TypeOp n1_tys1, TypeOp n2_tys2) =>
      prodCompare Name.compare (lexCompare compare) (n1_tys1,n2_tys2);

fun equalTy ty1 ty2 = compareTy (ty1,ty2) = EQUAL;

fun 
fun equalTy ty1 ty2 = 
    let
      val Ty {id = id1, ty1' = ty, sz = sz1} = ty1
      and Ty {id = id2, ty2' = ty, sz = sz2} = ty2
    in
      id1 = id2 orelse (sz1 = sz2 andalso equalTy' ty1' ty2')
    end

and equalTy' ty1 ty2 =
    case (ty1,ty2) of
      (VarTy n1, VarTy n2) => Name.equal n1 n2
    | (OpTy (n1,tys1,o1), OpTy (n2,tys2,o2)) =>
      Name.equal n1 n2 andalso 
    | _ => false

    UndefTy
  | DefTy of defOpTy

(* ------------------------------------------------------------------------- *)
(* Equality.                                                                 *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Type constructors and destructors.                                        *)
(* ------------------------------------------------------------------------- *)

fun infoTy (Ty info) = info;

fun idTy ty =
    let
      val {id = i, ...} = infoTy ty
    in
      i
    end;

fun sizeTy ty =
    let
      val {sz,...} = infoTy ty
    in
      sz
    end;

val sizeListTy =
    let
      fun add (ty,n) = sizeTy ty + n
    in
      foldl add 0
    end;

fun destTy ty =
    let
      val {ty = t, ...} = infoTy ty
    in
      t
    end;

fun mkTy ty =
    let
      val id = newIdTy ()
    in
      case ty of
        VarTy n =>
        let
          val sz = 1
        in
          Ty
            {id = id,
             ty = ty,
             sz = sz}
        end
      | OpTy (_,tys,_) =>
        let
          val sz = sizeListTy tys + 1
        in
          Ty
            {id = id,
             ty = ty,
             sz = sz}
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Function types.                                                           *)
(* ------------------------------------------------------------------------- *)

val stringFun = "fun";

val nameFun = Name.mkGlobal stringFun;

fun mkFun (x,y) = mkTy (OpTy (nameFun,[x,y],UndefTy));

fun destFun ty =
    case destTy ty of
      OpTy (n,[x,y],UndefTy) =>
      if Name.equal n nameFun then (x,y)
      else raise Error "TypeTerm.destFun"
    | _ => raise Error "TypeTerm.destFun";

val isFun = can destFun;

(* ------------------------------------------------------------------------- *)
(* Term constructors and destructors.                                        *)
(* ------------------------------------------------------------------------- *)

fun info (Term info) = info;

fun id tm =
    let
      val {id = i, ...} = info tm
    in
      i
    end;

fun size tm =
    let
      val {sz,...} = info tm
    in
      sz
    end;

fun typeOf tm =
    let
      val {ty,...} = info tm
    in
      ty
    end;

fun dest tm =
    let
      val {tm = t, ...} = info tm
    in
      t
    end;

fun mk tm =
    case tm of
      Const (_,ty,_) =>
      let
        val id = newId ()
        val sz = 1
      in
        Term
          {id = id,
           tm = tm,
           sz = sz,
           ty = ty}
      end
    | Var v =>
      let
        val id = newId ()
        val VarV (_,ty) = v
        val sz = 1
      in
        Term
          {id = id,
           tm = tm,
           sz = sz,
           ty = ty}
      end
    | App (f,a) =>
      let
        val (tyA,ty) = destFun (typeOf f)
        val tyA' = typeOf a
        val _ = equalTy tyA tyA' orelse
                raise Error "TypeTerm.mk: incompatible types in App"

        val id = newId ()
        val sz = size f + size a + 1
      in
        Term
          {id = id,
           tm = tm,
           sz = sz,
           ty = ty}
      end
    | Abs (v,b) =>
      let
        val id = newTermId ()
        val sz = size b + 1
        val ty = Type.mkFun (Var.typeOf v, typeOf b);
      in
        Term
          {id = id,
           tm = tm,
           sz = sz,
           ty = ty}
      end;

end
