(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES AND TERMS                                        *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
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

and const =
    Const of
      {name : Name.name,
       prov : provConst}

and provConst =
    UndefProvConst
  | DefProvConst of defConst
  | AbsProvConst of opTy
  | RepProvConst of opTy

and defConst = DefConst of term;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compareTy (ty1,ty2) =
    let
      val Ty {id = id1, ty = ty1, sz = sz1} = ty1
      and Ty {id = id2, ty = ty2, sz = sz2} = ty2
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
    | (OpTy' (o1,tys1), OpTy' (o2,tys2)) =>
      case compareOpTy (o1,o2) of
        LESS => LESS
      | EQUAL => compareListTy (tys1,tys2)
      | GREATER => GREATER

and compareOpTy (o1,o2) =
    let
      val OpTy {name = n1, prov = p1} = o1
      and OpTy {name = n2, prov = p2} = o2
    in
      case Name.compare (n1,n2) of
        LESS => LESS
      | EQUAL => compareProvOpTy (p1,p2)
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
      | EQUAL => lexCompare Name.compare (v1,v2)
      | GREATER => GREATER
    end

and compareVar (Var (n1,ty1), Var (n2,ty2)) =
    case Name.compare (n1,n2) of
      LESS => LESS
    | EQUAL => compareTy (ty1,ty2)
    | GREATER => GREATER

and compareListVar vs1_vs2 = lexCompare compareVar vs1_vs2

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
      (Const' (c1,ty1), Const' (c2,ty2)) =>
      (case compareConst (c1,c2) of
         LESS => LESS
       | EQUAL => compareTy (ty1,ty2)
       | GREATER => GREATER)
    | (Const' _, _) => LESS
    | (_, Const' _) => GREATER
    | (Var' v1, Var' v2) => compareVar (v1,v2)
    | (Var' _, _) => LESS
    | (_, Var' _) => GREATER
    | (App' (f1,a1), App' (f2,a2)) =>
      (case compare (f1,f2) of
         LESS => LESS
       | EQUAL => compare (a1,a2)
       | GREATER => GREATER)
    | (App' _, _) => LESS
    | (_, App' _) => GREATER
    | (Abs' (v1,b1), Abs' (v2,b2)) =>
      case compareVar (v1,v2) of
        LESS => LESS
      | EQUAL => compare (b1,b2)
      | GREATER => GREATER

and compareConst (c1,c2) =
    let
      val Const {name = n1, prov = p1} = c1
      and Const {name = n2, prov = p2} = c2
    in
      case Name.compare (n1,n2) of
        LESS => LESS
      | EQUAL => compareProvConst (p1,p2)
      | GREATER => GREATER
    end

and compareProvConst p1_p2 =
    case p1_p2 of
      (UndefProvConst,UndefProvConst) => EQUAL
    | (UndefProvConst,_) => LESS
    | (_,UndefProvConst) => GREATER
    | (DefProvConst d1, DefProvConst d2) => compareDefConst (d1,d2)
    | (DefProvConst _, _) => LESS
    | (_, DefProvConst _) => GREATER
    | (AbsProvConst o1, AbsProvConst o2) => compareOpTy (o1,o2)
    | (AbsProvConst _, _) => LESS
    | (_, AbsProvConst _) => GREATER
    | (RepProvConst o1, RepProvConst o2) => compareOpTy (o1,o2)

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
      List.foldl add 0
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
      List.foldl add 0
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
    | OpTy' (_,tys) => sizeListTy tys + 1;

(* Total order *)

fun equalTy' t1 t2 = compareTy' (t1,t2) = EQUAL;

(* Constructor *)

fun mkTy ty =
    let
      val id = newIdTy ()
      val sz = sizeTy' ty
    in
      Ty
        {id = id,
         ty = ty,
         sz = sz}
    end;

(* ------------------------------------------------------------------------- *)
(* Function spaces.                                                          *)
(* ------------------------------------------------------------------------- *)

val opTyFunTy =
    let
      val name = Name.funTypeOp

      val prov = UndefProvOpTy
    in
      OpTy
        {name = name,
         prov = prov}
    end;

fun mkFunTy (x,y) = mkTy (OpTy' (opTyFunTy,[x,y]));

fun destFunTy ty =
    case destTy ty of
      OpTy' (c,[x,y]) =>
      if equalOpTy opTyFunTy c then (x,y)
      else raise Error "TypeTerm.destFunTy: bad type operator"
    | _ => raise Error "TypeTerm.destFunTy: bad form";

val isFunTy = can destFunTy;

(* ------------------------------------------------------------------------- *)
(* Variables.                                                                *)
(* ------------------------------------------------------------------------- *)

fun nameVar (Var (n,_)) = n;

fun typeOfVar (Var (_,ty)) = ty;

(* Total order *)

fun equalVar v1 v2 = compareVar (v1,v2) = EQUAL;

fun equalListVar vs1 vs2 = compareListVar (vs1,vs2) = EQUAL;

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

fun nameConst (Const {name = n, ...}) = n;

fun provConst (Const {prov = p, ...}) = p;

(* Total order *)

fun equalConst c1 c2 = compareConst (c1,c2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Term constructors and destructors.                                        *)
(* ------------------------------------------------------------------------- *)

fun info (Term info) = info;

fun dest (Term {tm,...}) = tm;

(* Number of constructors *)

fun size' tm =
    case tm of
      Const' _ => 1
    | Var' _ => 1
    | App' (f,a) => size f + size a + 1
    | Abs' (_,b) => size b + 1;

(* Type *)

fun typeOf' tm =
    case tm of
      Const' (_,ty) => ty
    | Var' v => typeOfVar v
    | App' (f,a) =>
      let
        val (_,ty) = destFunTy (typeOf f)
      in
        ty
      end
    | Abs' (v,b) => mkFunTy (typeOfVar v, typeOf b);

(* Total order *)

fun equal' t1 t2 = compare' (t1,t2) = EQUAL;

(* Constructor *)

fun mk tm =
    let
      val ty =
          case tm of
            Const' (_,ty) => ty
          | Var' v => typeOfVar v
          | App' (f,a) =>
            let
              val (aty,ty) = destFunTy (typeOf f)
            in
              if equalTy aty (typeOf a) then ty
              else raise Error "TypeTerm.mk: App: incompatible types"
            end
          | Abs' (v,b) => mkFunTy (typeOfVar v, typeOf b)

      val id = newId ()

      val sz = size' tm
    in
      Term
        {id = id,
         tm = tm,
         sz = sz,
         ty = ty}
    end;

end
