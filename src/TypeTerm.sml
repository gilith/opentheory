(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES AND TERMS                                        *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure TypeTerm :> TypeTerm =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* The mutually recursive datatype of higher order logic types and terms.    *)
(* ------------------------------------------------------------------------- *)

type idTy = int;

type id = int;

datatype ty =
    Ty of
      {id : idTy,
       ty : ty',
       sz : int}

and ty' =
    VarTy' of Name.name
  | OpTy' of opTy * ty list

and opTy =
    OpTy of
      {name : Name.name,
       arity : int,
       prov : provOpTy}

and provOpTy =
    UndefProvOpTy
  | DefProvOpTy of defOpTy

and defOpTy =
    DefOpTy of
      {pred : term,
       vars : Name.name list}

and var = Var of Name.name * ty

and term =
    Term of
      {id : id,
       tm : term',
       sz : int,
       ty : ty}

and term' =
    Const' of const * ty
  | Var' of var
  | App' of term * term
  | Abs' of var * term

and const = Const of Name.name * provConst

and provConst =
    UndefProvConst
  | DefProvConst of defConst
  | AbsProvConst of defOpTy
  | RepProvConst of defOpTy

and defConst = DefConst of term;

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

and compareListTy tys1_tys2 = lexCompare compareTy tys1_tys2

and compareTy' ty1_ty2 =
    case ty1_ty2 of
      (VarTy' n1, VarTy' n2) => Name.compare (n1,n2)
    | (VarTy' _, OpTy' _) => LESS
    | (OpTy' _, VarTy' _) => GREATER
    | (OpTy' o1_tys1, OpTy' o2_tys2) =>
      prodCompare compareOpTy compareListTy (o1_tys1,o2_tys2)

and compareOpTy (o1,o2) =
    let
      val OpTy {name = n1, arity = a1, prov = p1} = o1
      and OpTy {name = n2, arity = a2, prov = p2} = o2
    in
      case Name.compare (n1,n2) of
        LESS => LESS
      | EQUAL =>
        (case Int.compare (a1,a2) of
           LESS => LESS
         | EQUAL => compareProvOpTy (p1,p2)
         | GREATER => GREATER)
      | GREATER => GREATER
    end

and compareProvOpTy p1_p2 =
    case p1_p2 of
      (UndefProvOpTy,UndefProvOpTy) => EQUAL
    | (UndefProvOpTy, DefProvOpTy _) => LESS
    | (DefProvOpTy _, UndefProvOpTy) => GREATER
    | (DefProvOpTy d1, DefProvOpTy d2) => compareDefOpTy (d1,d2)

and compareDefOpTy (d1,d2) =
    let
      val DefOpTy {pred = p1, vars = v1} = d1
      and DefOpTy {pred = p2, vars = v2} = d2
    in
      case compare (p1,p2) of
        LESS => LESS
      | EQUAL => compareListVar (v1,v2)
      | GREATER => GREATER
    end

and compareVar (Var n1_ty1, Var n2_ty2) =
    prodCompare Name.compare compareTy (n1_ty1,n2_ty2)

and compareVarList vs1_vs2 = lexCompare compareVar vs1_vs2

and compare (tm1,tm2) =
    let
      val Term {id = id1, tm = tm1, sz = sz1, ...} = tm1
      and Term {id = id2, tm = tm2, sz = sz2, ...} = tm2
    in
      if id1 = id2 then EQUAL
      else
        case Int.compare (sz1,sz2) of
          LESS => LESS
        | EQUAL => compare' (tm1,tm2)
        | GREATER => GREATER
    end

and compare' tm1_tm2 =
    case tm1_tm2 of
      (Const c1_ty1, Const c2_ty2) =>
      prodCompare compareConst compareTy (c1_ty1,c2_ty2)
    | (Const _, _) => LESS
    | (_, Const _) => GREATER
    | (Var v1, Var v2) => compareVar (v1,v2)
    | (Var _, _) => LESS
    | (_, Var _) => GREATER
    | (App f1_a1, App f2_a2) => prodCompare compare compare (f1_a1,f2_a2)
    | (App _, _) => LESS
    | (_, App _) => GREATER
    | (Abs v1_b1, Abs v2_b2) => prodCompare compareVar compare (v1_b1,v2_b2)

and compareConst (Const n1_p1, Const n2_p2) =
    prodCompare Name.compare compareProvConst (n1_p1,n2_p2)

and compareProvConst p1_p2 =
    case p1_p2 of
      (UndefProvConst,UndefProvConst) => EQUAL
    | (UndefProvConst,_) => LESS
    | (_,UndefProvConst) => GREATER
    | (DefProvConst d1, DefProvConst d2) => compareDefConst (d1,d2)
    | (DefProvConst,_) => LESS
    | (_,DefProvConst) => GREATER
    | (AbsProvConst d1, AbsProvConst d2) => compareDefOpTy (d1,d2)
    | (AbsProvConst,_) => LESS
    | (_,AbsProvConst) => GREATER
    | (RepProvConst d1, RepProvConst d2) => compareDefOpTy (d1,d2)

and compareDefConst (DefConst tm1, DefConst tm2) = compare (tm1,tm2);

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

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

fun idTy (Ty {id = i, ...}) = i;

fun equalIdTy i ty = i = idTy ty;

(* ------------------------------------------------------------------------- *)
(* The size of a type as the number of constructors.                         *)
(* ------------------------------------------------------------------------- *)

fun sizeTy (Ty {sz,...}) = sz;

val sizeListTy =
    let
      fun add (ty,n) = sizeTy ty + n
    in
      foldl add 0
    end;

(* ------------------------------------------------------------------------- *)
(* A total order on types.                                                   *)
(* ------------------------------------------------------------------------- *)

fun equalTy ty1 ty2 = compareTy (ty1,ty2) = EQUAL;

fun equalListTy tys1 tys2 = compareListTy (tys1,tys2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

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

fun id (Term {id = i, ...}) = i;

fun equalId i tm = i = id tm;

(* ------------------------------------------------------------------------- *)
(* The size of a term as the number of constructors.                         *)
(* ------------------------------------------------------------------------- *)

fun size (Term {sz,...}) = sz;

val sizeList =
    let
      fun add (tm,n) = size tm + n
    in
      foldl add 0
    end;

(* ------------------------------------------------------------------------- *)
(* The type of a term.                                                       *)
(* ------------------------------------------------------------------------- *)

fun typeOf (Term {ty,...}) = ty;

(* ------------------------------------------------------------------------- *)
(* A total order on terms.                                                   *)
(* ------------------------------------------------------------------------- *)

fun equal tm1 tm2 = compare (tm1,tm2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Type operator definitions.                                                *)
(* ------------------------------------------------------------------------- *)

(* Total order *)

fun equalDefOpTy d1 d2 = compareDefOpTy (d1,d2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Type operator provenance.                                                 *)
(* ------------------------------------------------------------------------- *)

(* Total order *)

fun equalProvOpTy p1 p2 = compareProvOpTy (p1,p2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

fun nameOpTy (OpTy {name = n, ...}) = n;

fun arityOpTy (OpTy {arity = a, ...}) = a;

fun provOpTy (OpTy {prov = p, ...}) = p;

(* Total order *)

fun equalOpTy o1 o2 = compareOpTy (o1,o2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Type constructors and destructors.                                        *)
(* ------------------------------------------------------------------------- *)

fun infoTy (Ty info) = info;

fun destTy (Ty {ty = t, ...}) = t;

(* Number of constructors *)

fun sizeTy' ty' =
    case ty' of
      VarTy' _ => 1
    | OpTy (_,tys) => sizeListTy tys + 1;

(* Total order *)

fun equalTy' t1 t2 = compareTy' (t1,t2) = EQUAL;

(* Constructor *)

fun mkTy ty' =
    let
      val () =
          case ty' of
            VarTy' _ => ()
          | OpTy' (ot,tys) =>
            if arityOpTy ot = length tys then ()
            else raise Error "TypeTerm.mkTy: wrong arity"

      val id = newIdTy ()

      val sz = sizeTy' ty'
    in
      Ty
        {id = id,
         ty = ty',
         sz = sz}
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val stringBool = "bool";

val nameBool = Name.mkGlobal stringBool;

val opTyBool =
    let
      val name = nameBool
      val arity = 0
      val prov = UndefTy
    in
      OpTy
        {name = name,
         arity = arity,
         prov = prov}
    end;

val bool = mkTy (OpTy' (opTyBool,[]));

fun isBool ty =
    case destTy ty of
      OpTy' (c,[]) => equalOpTy opTyBool c
    | _ => false;

(* Function spaces *)

val stringFun = "fun";

val nameFun = Name.mkGlobal stringFun;

val opTyFun =
    let
      val name = nameFun
      val arity = 2
      val prov = UndefTy
    in
      OpTy
        {name = name,
         arity = arity,
         prov = prov}
    end;

fun mkFun (x,y) = mkTy (OpTy' (opTyFun,[x,y]));

fun destFun ty =
    case destTy ty of
      OpTy' (c,[x,y]) =>
      if equalOpTy opTyFun c then (x,y)
      else raise Error "TypeTerm.destFun: bad type operator"
    | _ => raise Error "TypeTerm.destFun: bad form";

val isFun = can destFun;

(* ------------------------------------------------------------------------- *)
(* Variables.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Total order *)

fun equalVar v1 v2 = compareVar (v1,v2) = EQUAL;

fun equalVarList vs1 vs2 = compareVarList (vs1,vs2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Constant definitions.                                                     *)
(* ------------------------------------------------------------------------- *)

(* Total order *)

fun equalDefConst d1 d2 = compareDefConst (d1,d2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Constant provenance.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Total order *)

fun equalProvConst p1 p2 = compareProvConst (p1,p2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Total order *)

fun equalConst c1 c2 = compareConst (c1,c2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Term constructors and destructors.                                        *)
(* ------------------------------------------------------------------------- *)

fun info (Term info) = info;

fun dest (Term {tm,...}) = tm;

(* Number of constructors *)

fun size' (Term {sz,...}) = sz;

(* Type *)

fun typeOf' (Term {sz,...}) = sz;

val typeOf' : term' -> ty

(* Total order *)

fun equal' t1 t2 = compare' (t1,t2) = EQUAL;

(* Constructor *)

val mk : term' -> term

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
