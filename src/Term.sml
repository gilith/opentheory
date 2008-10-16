(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Term :> Term =
struct

open Useful;

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
