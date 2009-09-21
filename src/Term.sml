(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Term :> Term =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic terms.                                       *)
(* ------------------------------------------------------------------------- *)

type term = TypeTerm.term;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

type term' = TypeTerm.term';

val mk = TypeTerm.mk;

val dest = TypeTerm.dest;

(* Constants *)

fun mkConst' c_ty = TypeTerm.Const' c_ty;

fun destConst' tm' =
    case tm' of
      TypeTerm.Const' c_ty => c_ty
    | _ => raise Error "Term.destConst'";

val isConst' = can destConst';

fun mkConst c_ty = mk (mkConst' c_ty);

fun destConst tm = destConst' (dest tm);

val isConst = can destConst;

(* Variables *)

fun mkVar' v = TypeTerm.Var' v;

fun destVar' tm' =
    case tm' of
      TypeTerm.Var' v => v
    | _ => raise Error "Term.destVar'";

val isVar' = can destVar';

fun equalVar' var tm' =
    case tm' of
      TypeTerm.Var' v => Var.equal var v
    | _ => false;

fun mkVar v = mk (mkVar' v);

fun destVar tm = destVar' (dest tm);

val isVar = can destVar;

fun equalVar v tm = equalVar' v (dest tm);

(* Function applications *)

fun mkApp' f_a = TypeTerm.App' f_a;

fun destApp' tm' =
    case tm' of
      TypeTerm.App' f_a => f_a
    | _ => raise Error "Term.destApp'";

val isApp' = can destApp';

fun mkApp f_a = mk (mkApp' f_a);

fun destApp tm = destApp' (dest tm);

val isApp = can destApp;

(* Lambda abstractions *)

fun mkAbs' v_b = TypeTerm.Abs' v_b;

fun destAbs' tm' =
    case tm' of
      TypeTerm.Abs' v_b => v_b
    | _ => raise Error "Term.destAbs'";

val isAbs' = can destAbs';

fun mkAbs v_b = mk (mkAbs' v_b);

fun destAbs tm = destAbs' (dest tm);

val isAbs = can destAbs;

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = TypeTerm.id;

val id = TypeTerm.id;

val equalId = TypeTerm.equalId;

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

val size = TypeTerm.size;

val sizeList = TypeTerm.sizeList;

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence.               *)
(* ------------------------------------------------------------------------- *)

val compare = TypeTerm.compare;

val equal = TypeTerm.equal;

local
  fun acmp n bv1 bv2 bvEq (tm1,tm2) =
      let
        val Term {id = id1, tm = tm1, ...} = tm1
        and Term {id = id2, tm = tm2, ...} = tm2
      in
        if bvEq andalso id1 = id2 then EQUAL
        else acmp' n bv1 bv2 bvEq (tm1,tm2)
      end

  and acmp' n bv1 bv2 bvEq tm1_tm2 =
      case tm1_tm2 of
        (Const c1, Const c2) => constCompare (c1,c2)
      | (Const _, _) => LESS
      | (_, Const _) => GREATER
      | (Var v1, Var v2) =>
        (case (VarMap.peek bv1 v1, VarMap.peek bv2 v2) of
           (NONE,NONE) => Var.compare (v1,v2)
         | (SOME _, NONE) => LESS
         | (NONE, SOME _) => GREATER
         | (SOME n1, SOME n2) => Int.compare (n1,n2))
      | (Var _, _) => LESS
      | (_, Var _) => GREATER
      | (Comb a1, Comb a2) =>
        let
          val cmp = acmp n bv1 bv2 bvEq
        in
          prodCompare cmp cmp (a1,a2)
        end
      | (Comb _, _) => LESS
      | (_, Comb _) => GREATER
      | (Abs (v1,t1), Abs (v2,t2)) =>
        let
          val bvEq = bvEq andalso Var.equal v1 v2
          val bv1 = VarMap.insert bv1 (v1,n)
          val bv2 = if bvEq then bv1 else VarMap.insert bv2 (v2,n)
          val n = n + 1
        in
          acmp n bv1 bv2 bvEq (t1,t2)
        end;
in
  val alphaCompare =
      let
        val n = 0
        val bv = VarMap.new ()
        val bvEq = true
      in
        acmp n bv bv bvEq
      end;
end;

fun alphaEqual tm1 tm2 = alphaCompare (tm1,tm2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* The type of a term.                                                       *)
(* ------------------------------------------------------------------------- *)

val typeOf = TypeTerm.typeOf;

(* ------------------------------------------------------------------------- *)
(* Free variables.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingFreeVars

val newSharingFreeVars : sharingFreeVars

val sharingFreeVars : term -> sharingFreeVars -> VarSet.set * sharingFreeVars

val freeVarsList : term list -> VarSet.set

val freeVars : term -> VarSet.set

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

type sharingConsts

val emptySharingConsts : sharingConsts

val addSharingConsts : sharingConsts -> term list -> sharingConsts

val toSetSharingConsts : sharingConsts -> ConstSet.set

val constsList : term list -> ConstSet.set

val consts : term -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeVars

val emptySharingTypeVars : sharingTypeVars

val addSharingTypeVars : sharingTypeVars -> term list -> sharingTypeVars

val toSetSharingTypeVars : sharingTypeVars -> NameSet.set

val typeVarsList : term list -> NameSet.set

val typeVars : term -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeOps

val emptySharingTypeOps : sharingTypeOps

val addSharingTypeOps : sharingTypeOps -> term list -> sharingTypeOps

val toSetSharingTypeOps : sharingTypeOps -> TypeOpSet.set

val typeOpsList : term list -> TypeOpSet.set

val typeOps : term -> TypeOpSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val mkEqTy : Type.ty -> Type.ty

val destEqTy : Type.ty -> Type.ty

val isEqTy : Type.ty -> bool

val nameEq : Name.name

val constEq : Const.const

val mkEq : term * term -> term

val destEq : term -> term * term

val isEq : term -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val maximumSize : int ref

val showTypes : bool ref

val pp : term Print.pp

val toString : term -> string

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic terms.                                       *)
(* ------------------------------------------------------------------------- *)

type termId = int;

datatype term =
    Term of
      {id : termId,
       tm : term',
       sz : int,
       ty : Type.ty}

and term' =
    Const of Name.name * Type.ty
  | Var of Var.var
  | Comb of term * term
  | Abs of Var.var * term;

(* ------------------------------------------------------------------------- *)
(* The constant registry (initially contains the primitive constants).       *)
(* ------------------------------------------------------------------------- *)

datatype registry =
    Registry of
      {all : NameSet.set,
       types : Type.ty NameMap.map};

val emptyRegistry =
    let
      val all = NameSet.empty
      val types = NameMap.new ()
    in
      Registry
        {all = all,
         types = types}
    end;

val theRegistry = ref emptyRegistry;

fun declaredConst name =
    let
      val Registry {types,...} = !theRegistry
    in
      NameMap.peek types name
    end;

fun allDeclared () =
    let
      val Registry {all,...} = !theRegistry
    in
      all
    end;

fun declare name ty =
    let
      val _ = not (NameSet.member name (allDeclared ())) orelse
              raise Error ("already a constant with name " ^
                           Name.toString name)

      val Registry {all,types} = !theRegistry

      val all = NameSet.add all name
      and types = NameMap.insert types (name,ty)

      val registry = Registry {all = all, types = types}
    in
      theRegistry := registry
    end
    handle Error err => raise Error ("Term.declare: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

val newTermId : unit -> termId =
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

fun equalId ty1 ty2 = id ty1 = id ty2;

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

fun size (Term {sz,...}) = sz;

(* ------------------------------------------------------------------------- *)
(* The type of a term.                                                       *)
(* ------------------------------------------------------------------------- *)

fun typeOf (Term {ty,...}) = ty;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk tm =
    case tm of
      Const (n,ty) =>
      let
        val () =
            case declaredConst n of
              NONE => ()
            | SOME ty' =>
              if can (TypeSubst.match ty') ty then ()
              else raise Error ("Term.mk: bad type for constant " ^
                                Name.toString n)

        val id = newTermId ()
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
        val id = newTermId ()
        val ty = Var.typeOf v
        val sz = 1
      in
        Term
          {id = id,
           tm = tm,
           sz = sz,
           ty = ty}
      end
    | Comb (f,a) =>
      let
        val (tyA,ty) = Type.destFun (typeOf f)
        val tyA' = typeOf a
        val _ = Type.equal tyA tyA' orelse
                raise Error "Term.mk: incompatible types in comb"

        val id = newTermId ()
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

fun dest (Term {tm,...}) = tm;

(* Constants *)

fun mkConst' n_ty = Const n_ty;

fun destConst' (Const n_ty) = n_ty
  | destConst' _ = raise Error "Term.destConst'";

val isConst' = can destConst';

fun mkConst n_ty = mk (mkConst' n_ty);

fun destConst tm = destConst' (dest tm);

val isConst = can destConst;

(* Variables *)

fun mkVar' v = Var v;

fun destVar' (Var v) = v
  | destVar' _ = raise Error "Term.destVar'";

val isVar' = can destVar';

fun equalVar' var (Var v) = Var.equal var v
  | equalVar' _ _ = false;

fun mkVar v = mk (mkVar' v);

fun destVar tm = destVar' (dest tm);

val isVar = can destVar;

fun equalVar var tm = equalVar' var (dest tm);

(* Function applications *)

fun mkComb' f_a = Comb f_a;

fun destComb' (Comb f_a) = f_a
  | destComb' _ = raise Error "Term.destComb'";

val isComb' = can destComb';

fun mkComb f_a = mk (mkComb' f_a);

fun destComb tm = destComb' (dest tm);

val isComb = can destComb;

(* Function abstractions *)

fun mkAbs' v_b = Abs v_b;

fun destAbs' (Abs v_b) = v_b
  | destAbs' _ = raise Error "Term.destAbs'";

val isAbs' = can destAbs';

fun mkAbs v_b = mk (mkAbs' v_b);

fun destAbs tm = destAbs' (dest tm);

val isAbs = can destAbs;

(* ------------------------------------------------------------------------- *)
(* Free variables.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype sharingFreeVars =
    SharingFreeVars of
      {seen : VarSet.set IntMap.map};

val newSharingFreeVars =
    let
      val seen = IntMap.new ()
    in
      SharingFreeVars
        {seen = seen}
    end;

fun rawSharingFreeVars tm seen =
    let
      val Term {id,tm,...} = tm
    in
      case IntMap.peek seen id of
        SOME vs => (vs,seen)
      | NONE =>
        case tm of
          Const _ =>
          let
            val fv = VarSet.empty
            val seen = IntMap.insert seen (id,fv)
          in
            (fv,seen)
          end
        | Var v =>
          let
            val fv = VarSet.singleton v
            val seen = IntMap.insert seen (id,fv)
          in
            (fv,seen)
          end
        | Comb (f,a) =>
          let
            val (fv1,seen) = rawSharingFreeVars f seen
            val (fv2,seen) = rawSharingFreeVars a seen
            val fv = VarSet.union fv1 fv2
            val seen = IntMap.insert seen (id,fv)
          in
            (fv,seen)
          end
        | Abs (v,b) =>
          let
            val (fv,seen) = rawSharingFreeVars b seen
            val fv = if VarSet.member v fv then VarSet.delete fv v else fv
            val seen = IntMap.insert seen (id,fv)
          in
            (fv,seen)
          end
    end;

fun sharingFreeVars tm (SharingFreeVars {seen}) =
    let
      val (fv,seen) = rawSharingFreeVars tm seen
      val share = SharingFreeVars {seen = seen}
    in
      (fv,share)
    end;

local
  fun add (tm,(fvs,share)) =
      let
        val (fv,share) = sharingFreeVars tm share
        val fvs = VarSet.union fvs fv
      in
        (fvs,share)
      end;
in
  fun freeVarsList tms =
      let
        val fvs = VarSet.empty
        val share = newSharingFreeVars
        val (fvs,_) = List.foldl add (fvs,share) tms
      in
        fvs
      end;
end;

fun freeVars tm =
    let
      val share = newSharingFreeVars
      val (fv,_) = sharingFreeVars tm share
    in
      fv
    end;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

fun sharingConsts seen acc [] = (seen,acc)
  | sharingConsts seen acc (tm :: tms) =
    let
      val Term {id,tm,...} = tm
    in
      if IntSet.member id seen then sharingConsts seen acc tms
      else
        let
          val seen = IntSet.add seen id
        in
          sharingConsts' seen acc tm tms
        end
    end

and sharingConsts' seen acc tm tms =
    case tm of
      Const (n,_) =>
      let
        val acc = NameSet.add acc n
      in
        sharingConsts seen acc tms
      end
    | Var _ => sharingConsts seen acc tms
    | Comb (f,a) =>
      let
        val tms = f :: a :: tms
      in
        sharingConsts seen acc tms
      end
    | Abs (_,b) =>
      let
        val tms = b :: tms
      in
        sharingConsts seen acc tms
      end;

fun constsList tms =
    let
      val (_,acc) = sharingConsts IntSet.empty NameSet.empty tms
    in
      acc
    end;

fun consts tm = constsList [tm];

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype sharingTypeVars =
    SharingTypeVars of
      {tyShare : Type.sharingTypeVars,
       seen : IntSet.set};

val emptySharingTypeVars =
    let
      val tyShare = Type.emptySharingTypeVars
      val seen = IntSet.empty
    in
      SharingTypeVars
        {tyShare = tyShare,
         seen = seen}
    end;

fun sharingTypeVars tyShare seen [] = (tyShare,seen)
  | sharingTypeVars tyShare seen (tm :: tms) =
    let
      val Term {id,tm,...} = tm
    in
      if IntSet.member id seen then sharingTypeVars tyShare seen tms
      else
        let
          val seen = IntSet.add seen id
        in
          sharingTypeVars' tyShare seen tm tms
        end
    end

and sharingTypeVars' tyShare seen tm tms =
    case tm of
      Const (_,ty) =>
      let
        val tyShare = Type.addSharingTypeVars tyShare [ty]
      in
        sharingTypeVars tyShare seen tms
      end
    | Var v =>
      let
        val tyShare = Var.addSharingTypeVars tyShare v
      in
        sharingTypeVars tyShare seen tms
      end
    | Comb (f,a) =>
      let
        val tms = f :: a :: tms
      in
        sharingTypeVars tyShare seen tms
      end
    | Abs (v,b) =>
      let
        val tyShare = Var.addSharingTypeVars tyShare v
        val tms = b :: tms
      in
        sharingTypeVars tyShare seen tms
      end;

fun addSharingTypeVars (SharingTypeVars {tyShare,seen}) tms =
    let
      val (tyShare,seen) = sharingTypeVars tyShare seen tms
    in
      SharingTypeVars
        {tyShare = tyShare,
         seen = seen}
    end;

fun toSetSharingTypeVars (SharingTypeVars {tyShare,...}) =
    Type.toSetSharingTypeVars tyShare;

fun typeVarsList tms =
    let
      val share = emptySharingTypeVars
      val share = addSharingTypeVars share tms
    in
      toSetSharingTypeVars share
    end;

fun typeVars tm = typeVarsList [tm];

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype sharingTypeOps =
    SharingTypeOps of
      {tyShare : Type.sharingTypeOps,
       seen : IntSet.set};

val emptySharingTypeOps =
    let
      val tyShare = Type.emptySharingTypeOps
      val seen = IntSet.empty
    in
      SharingTypeOps
        {tyShare = tyShare,
         seen = seen}
    end;

fun sharingTypeOps tyShare seen [] = (tyShare,seen)
  | sharingTypeOps tyShare seen (tm :: tms) =
    let
      val Term {id,tm,...} = tm
    in
      if IntSet.member id seen then sharingTypeOps tyShare seen tms
      else
        let
          val seen = IntSet.add seen id
        in
          sharingTypeOps' tyShare seen tm tms
        end
    end

and sharingTypeOps' tyShare seen tm tms =
    case tm of
      Const (_,ty) =>
      let
        val tyShare = Type.addSharingTypeOps tyShare [ty]
      in
        sharingTypeOps tyShare seen tms
      end
    | Var v =>
      let
        val tyShare = Var.addSharingTypeOps tyShare v
      in
        sharingTypeOps tyShare seen tms
      end
    | Comb (f,a) =>
      let
        val tms = f :: a :: tms
      in
        sharingTypeOps tyShare seen tms
      end
    | Abs (v,b) =>
      let
        val tyShare = Var.addSharingTypeOps tyShare v
        val tms = b :: tms
      in
        sharingTypeOps tyShare seen tms
      end;

fun addSharingTypeOps (SharingTypeOps {tyShare,seen}) tms =
    let
      val (tyShare,seen) = sharingTypeOps tyShare seen tms
    in
      SharingTypeOps
        {tyShare = tyShare,
         seen = seen}
    end;

fun toSetSharingTypeOps (SharingTypeOps {tyShare,...}) =
    Type.toSetSharingTypeOps tyShare;

fun typeOpsList tms =
    let
      val share = emptySharingTypeOps
      val share = addSharingTypeOps share tms
    in
      toSetSharingTypeOps share
    end;

fun typeOps tm = typeOpsList [tm];

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence.               *)
(* ------------------------------------------------------------------------- *)

val constCompare = prodCompare Name.compare Type.compare;

fun compare (tm1,tm2) =
    let
      val Term {id = id1, tm = tm1, ...} = tm1
      and Term {id = id2, tm = tm2, ...} = tm2
    in
      if id1 = id2 then EQUAL else compare' (tm1,tm2)
    end

and compare' tm1_tm2 =
    case tm1_tm2 of
      (Const c1, Const c2) => constCompare (c1,c2)
    | (Const _, _) => LESS
    | (_, Const _) => GREATER
    | (Var v1, Var v2) => Var.compare (v1,v2)
    | (Var _, _) => LESS
    | (_, Var _) => GREATER
    | (Comb a1, Comb a2) => prodCompare compare compare (a1,a2)
    | (Comb _, _) => LESS
    | (_, Comb _) => GREATER
    | (Abs l1, Abs l2) => prodCompare Var.compare compare (l1,l2);

fun equal tm1 tm2 = compare (tm1,tm2) = EQUAL;

local
  fun acmp n bv1 bv2 bvEq (tm1,tm2) =
      let
        val Term {id = id1, tm = tm1, ...} = tm1
        and Term {id = id2, tm = tm2, ...} = tm2
      in
        if bvEq andalso id1 = id2 then EQUAL
        else acmp' n bv1 bv2 bvEq (tm1,tm2)
      end

  and acmp' n bv1 bv2 bvEq tm1_tm2 =
      case tm1_tm2 of
        (Const c1, Const c2) => constCompare (c1,c2)
      | (Const _, _) => LESS
      | (_, Const _) => GREATER
      | (Var v1, Var v2) =>
        (case (VarMap.peek bv1 v1, VarMap.peek bv2 v2) of
           (NONE,NONE) => Var.compare (v1,v2)
         | (SOME _, NONE) => LESS
         | (NONE, SOME _) => GREATER
         | (SOME n1, SOME n2) => Int.compare (n1,n2))
      | (Var _, _) => LESS
      | (_, Var _) => GREATER
      | (Comb a1, Comb a2) =>
        let
          val cmp = acmp n bv1 bv2 bvEq
        in
          prodCompare cmp cmp (a1,a2)
        end
      | (Comb _, _) => LESS
      | (_, Comb _) => GREATER
      | (Abs (v1,t1), Abs (v2,t2)) =>
        let
          val bvEq = bvEq andalso Var.equal v1 v2
          val bv1 = VarMap.insert bv1 (v1,n)
          val bv2 = if bvEq then bv1 else VarMap.insert bv2 (v2,n)
          val n = n + 1
        in
          acmp n bv1 bv2 bvEq (t1,t2)
        end;
in
  val alphaCompare =
      let
        val n = 0
        val bv = VarMap.new ()
        val bvEq = true
      in
        acmp n bv bv bvEq
      end;
end;

fun alphaEqual tm1 tm2 = alphaCompare (tm1,tm2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val stringEq = "=";

val nameEq = Name.mkGlobal stringEq;

val constEq =
    let
      val name = nameEq
      val prov = UndefProvConst
    in
      Const
        {name = name,
         prov = prov}
    end;

fun mkEq (x,y) =
    let
      val a = typeOf y
      val ty = mkFun (a, mkFun (a,bool))
      val t0 = mk (Const' (constEq,ty))
      val t1 = mk (App' (t0,x))
      val tm = mk (App' (t1,y))
    in
      tm
    end;

fun destEq tm =
    case dest tm of
      App' (t1,y) =>
      (case dest t1 of
         App' (t0,x) =>
         (case dest t0 of
            Const' (c,_) =>
            if equalConst constEq c then (x,y)
            else raise Error "TypeTerm.destEq: bad const"
          | _ => raise Error "TypeTerm.destEq: not an App' (App' (Const' _, _), _)")
       | _ => raise Error "TypeTerm.destEq: not an App' (App' _, _)")
    | _ => raise Error "TypeTerm.destEq: not an App' _";

val isEq = can destEq;

(* Equality *)

fun eqType a = Type.mkFun (a, Type.mkFun (a, Type.bool));

val eqString = "=";

val eqName = Name.mkGlobal eqString;

val eqTerm =
    let
      val ty = eqType Type.alpha
      val () = declare eqName ty
    in
      mkConst (eqName,ty)
    end;

fun mkEq (l,r) = mkComb (mkComb (mkConst (eqName, eqType (typeOf l)), l), r);

fun destEq tm =
    let
      val (eq_l,r) = destComb tm
      val (eq,l) = destComb eq_l
      val (n,_) = destConst eq
    in
      if Name.equal n eqName then (l,r) else raise Error "Term.destEq"
    end;

val isEq = can destEq;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val maximumSize = ref 1000;

val showTypes = ref false;

val infixTokens =
    Print.Infixes
      [(* ML style *)
       {token = " / ", precedence = 7, leftAssoc = true},
       {token = " div ", precedence = 7, leftAssoc = true},
       {token = " mod ", precedence = 7, leftAssoc = true},
       {token = " * ", precedence = 7, leftAssoc = true},
       {token = " + ", precedence = 6, leftAssoc = true},
       {token = " - ", precedence = 6, leftAssoc = true},
       {token = " ^ ", precedence = 6, leftAssoc = true},
       {token = " @ ", precedence = 5, leftAssoc = false},
       {token = " :: ", precedence = 5, leftAssoc = false},
       {token = " = ", precedence = 4, leftAssoc = true},
       {token = " <> ", precedence = 4, leftAssoc = true},
       {token = " <= ", precedence = 4, leftAssoc = true},
       {token = " < ", precedence = 4, leftAssoc = true},
       {token = " >= ", precedence = 4, leftAssoc = true},
       {token = " > ", precedence = 4, leftAssoc = true},
       {token = " o ", precedence = 3, leftAssoc = true},
       (* HOL style *)
       {token = " /\\ ", precedence = ~1, leftAssoc = false},
       {token = " \\/ ", precedence = ~2, leftAssoc = false},
       {token = " ==> ", precedence = ~3, leftAssoc = false},
       {token = " <=> ", precedence = ~4, leftAssoc = false}];

local
  val negString = "~";

  val binders = ["\\","!","?","?!","select"];

  val infixStrings = Print.tokensInfixes infixTokens;

  val binderStrings = StringSet.fromList binders;

  val specialStrings =
      StringSet.add (StringSet.union infixStrings binderStrings) negString;

  fun abbreviateConst n =
      case Name.toString n of
        s => s;

  fun specialString n = StringSet.member n specialStrings;

  val ppConst =
      let
        fun f (n,_) =
            let
              val n = abbreviateConst n
            in
              if specialString n then "(" ^ n ^ ")" else n
            end
      in
        Print.ppMap f Print.ppString
      end;

  fun destInfix tm =
      let
        val (t,b) = destComb tm
        val (c,a) = destComb t
        val (n,_) = destConst c
        val n = abbreviateConst n
      in
        if StringSet.member n infixStrings then (n,a,b)
        else raise Error "Syntax.destInfix"
      end;

  val isInfix = can destInfix;

  fun destNeg tm =
      let
        val (c,a) = destComb tm
        val (n,_) = destConst c
        val n = abbreviateConst n
      in
        if n = negString then a else raise Error "Syntax.destNeg"
      end;

  fun countNegs tm =
      case total destNeg tm of
        NONE => (0,tm)
      | SOME t => let val (n,r) = countNegs t in (n + 1, r) end;

  fun destBinder tm =
      let
        val (n,tm) =
            if isAbs tm then ("\\",tm)
            else
              let
                val (c,t) = destComb tm
                val (n,_) = destConst c
                val n = abbreviateConst n
              in
                if StringSet.member n binderStrings then (n,t)
                else raise Error "Syntax.destBinder"
              end

        val (v,b) = destAbs tm
      in
        (n,v,b)
      end;

  val isBinder = can destBinder;

  fun stripBinder tm =
      let
        val (n,v,b) = destBinder tm

        fun dest vs t =
            case total destBinder t of
              NONE => (vs,t)
            | SOME (n',v,b) =>
              if n' = n then dest (v :: vs) b else (vs,t)

        val (vs,b) = dest [] b
      in
        (n, v, rev vs, b)
      end;

  val infixPrinter = Print.ppInfixes infixTokens (total destInfix);

  fun basic tm =
      if isVar tm then Var.pp (destVar tm)
      else if isConst tm then ppConst (destConst tm)
      else ppBtm tm

  and application tm =
      case total destComb tm of
        NONE => basic tm
      | SOME (f,x) =>
        Print.program
          [function f,
           Print.addBreak 1,
           basic x]

  and function tm = if isInfix tm then ppBtm tm else binder (tm,true)

  and binder (tm,r) =
      let
        fun ppBind tm =
            let
              val (sym,v,vs,body) = stripBinder tm

              val printSym =
                  case String.size sym of
                    0 => Print.addString "EmptyBinder"
                  | n =>
                    let
                      val pp = Print.addString sym
                    in
                      if not (Char.isAlphaNum (String.sub (sym, n - 1))) then pp
                      else Print.sequence pp (Print.addString " ")
                    end
            in
              Print.program
                [printSym,
                 Var.pp v,
                 Print.program
                   (map (Print.sequence (Print.addBreak 1) o Var.pp) vs),
                 Print.addString ".",
                 Print.addBreak 1,
                 if isBinder body then ppBind body else ppTm (body,false)]
            end

        val ppBinder = Print.block Print.Inconsistent 2 o ppBind
      in
        if not (isBinder tm) then application
        else (if r then Print.ppBracket "(" ")" else I) ppBinder
      end tm

  and negs (tm,r) =
      let
        val (n,tm) = countNegs tm
      in
        Print.blockProgram Print.Inconsistent n
          [Print.duplicate n (Print.addString negString),
           if isInfix tm then ppBtm tm else binder (tm,r)]
      end

  and ppBtm tm = Print.ppBracket "(" ")" ppTm (tm,false)

  and ppTm tmr = infixPrinter negs tmr;
in
  fun pp tm =
      let
        val n = size tm
      in
        if n <= !maximumSize then ppTm (tm,false)
        else Print.addString ("term{" ^ Int.toString n ^ "}")
      end;
end;

val toString = Print.toString pp;

end

structure TermOrdered =
struct type t = Term.term val compare = Term.compare end

structure TermSet = ElementSet (TermOrdered)

structure TermMap = KeyMap (TermOrdered)

structure TermAlphaOrdered =
struct type t = Term.term val compare = Term.alphaCompare end

structure TermAlphaSet =
struct

  local
    structure S = ElementSet (TermAlphaOrdered);
  in
    open S;
  end;

  val typeOps =
      let
        fun addNames (tm,acc) = NameSet.union acc (Term.typeOps tm)
      in
        foldl addNames NameSet.empty
      end;

  val consts =
      let
        fun addNames (tm,acc) = NameSet.union acc (Term.consts tm)
      in
        foldl addNames NameSet.empty
      end;

end

structure TermAlphaMap = KeyMap (TermAlphaOrdered)
