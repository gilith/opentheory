(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES                                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Type :> Type =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic types.                                       *)
(* ------------------------------------------------------------------------- *)

type ty = TypeTerm.ty;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

type ty' = TypeTerm.ty';

val mk = TypeTerm.mkTy;

val dest = TypeTerm.destTy;

(* Variables *)

fun mkVar' n = TypeTerm.VarTy' n;

fun destVar' ty' =
    case ty' of
      TypeTerm.VarTy' n => n
    | _ => raise Error "Type.destVar'";

val isVar' = can destVar';

fun equalVar' name ty' =
    case ty' of
      TypeTerm.VarTy' n => Name.equal name n
    | _ => false;

fun mkVar n = mk (mkVar' n);

fun destVar ty = destVar' (dest ty);

val isVar = can destVar;

fun equalVar n ty = equalVar' n (dest ty);

(* Operators *)

fun mkOp' o_tys = TypeTerm.OpTy' o_tys;

fun destOp' ty' =
    case ty' of
      TypeTerm.OpTy' o_tys => o_tys
    | _ => raise Error "Type.destOp'";

val isOp' = can destOp';

fun mkOp o_tys = mk (mkOp' o_tys);

fun destOp ty = destOp' (dest ty);

val isOp = can destOp;

fun destOpTy ot ty =
    let
      val (ot',tys) = destOp ty
    in
      if TypeOp.equal ot ot' then tys
      else raise Error "Type.destOpTy"
    end;

fun isOpTy ot = can (destOpTy ot);

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = TypeTerm.idTy;

val id = TypeTerm.idTy;

val equalId = TypeTerm.equalIdTy;

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

val size = TypeTerm.sizeTy;

val sizeList = TypeTerm.sizeListTy;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = TypeTerm.compareTy;

val compareList = TypeTerm.compareListTy;

val equal = TypeTerm.equalTy;

val equalList = TypeTerm.equalListTy;

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype sharingTypeVars =
    SharingTypeVars of
      {seen : IntSet.set,
       vars : NameSet.set};

val emptySharingTypeVars =
    let
      val seen = IntSet.empty
      val vars = NameSet.empty
    in
      SharingTypeVars
        {seen = seen,
         vars = vars}
    end;

fun sharingTypeVars seen acc tys =
    case tys of
      [] => (seen,acc)
    | ty :: tys =>
      let
        val {id,ty,...} = TypeTerm.infoTy ty
      in
        if IntSet.member id seen then sharingTypeVars seen acc tys
        else
          let
            val seen = IntSet.add seen id
          in
            sharingTypeVars' seen acc ty tys
          end
      end

and sharingTypeVars' seen acc ty tys =
    case ty of
      TypeTerm.VarTy' n =>
      let
        val acc = NameSet.add acc n
      in
        sharingTypeVars seen acc tys
      end
    | TypeTerm.OpTy' (_,tys') =>
      let
        val tys = List.revAppend (tys',tys)
      in
        sharingTypeVars seen acc tys
      end;

fun addListSharingTypeVars tys (SharingTypeVars {seen,vars}) =
    let
      val (seen,vars) = sharingTypeVars seen vars tys
    in
      SharingTypeVars
        {seen = seen,
         vars = vars}
    end;

fun addSharingTypeVars ty share = addListSharingTypeVars [ty] share;

fun toSetSharingTypeVars (SharingTypeVars {vars,...}) = vars;

fun typeVarsList tys =
    let
      val share = emptySharingTypeVars

      val share = addListSharingTypeVars tys share
    in
      toSetSharingTypeVars share
    end;

fun typeVars ty = typeVarsList [ty];

val alpha = mkVar (Name.mkGlobal "'a");

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype sharingTypeOps =
    SharingTypeOps of
      {seen : IntSet.set,
       ops : TypeOpSet.set};

val emptySharingTypeOps =
    let
      val seen = IntSet.empty
      val ops = TypeOpSet.empty
    in
      SharingTypeOps
        {seen = seen,
         ops = ops}
    end;

fun addTypeOpSharingTypeOps ot share =
    let
      val SharingTypeOps {seen,ops} = share

      val ops = TypeOpSet.add ops ot
    in
      SharingTypeOps
        {seen = seen,
         ops = ops}
    end;

fun addTypeOpSetSharingTypeOps ots share =
    let
      val SharingTypeOps {seen,ops} = share

      val ops = TypeOpSet.union ops ots
    in
      SharingTypeOps
        {seen = seen,
         ops = ops}
    end;

fun unionSharingTypeOps share1 share2 =
    let
      val SharingTypeOps {seen = seen1, ops = ops1} = share1
      and SharingTypeOps {seen = seen2, ops = ops2} = share2

      val seen = IntSet.union seen1 seen2

      val ops = TypeOpSet.union ops1 ops2
    in
      SharingTypeOps
        {seen = seen,
         ops = ops}
    end;

fun sharingTypeOps seen acc tys =
    case tys of
      [] => (seen,acc)
    | ty :: tys =>
      let
        val {id,ty,...} = TypeTerm.infoTy ty
      in
        if IntSet.member id seen then sharingTypeOps seen acc tys
        else
          let
            val seen = IntSet.add seen id
          in
            sharingTypeOps' seen acc ty tys
          end
      end

and sharingTypeOps' seen acc ty tys =
    case ty of
      TypeTerm.VarTy' _ => sharingTypeOps seen acc tys
    | TypeTerm.OpTy' (ot,tys') =>
      let
        val acc = TypeOpSet.add acc ot
        val tys = List.revAppend (tys',tys)
      in
        sharingTypeOps seen acc tys
      end;

fun addListSharingTypeOps tys (SharingTypeOps {seen,ops}) =
    let
      val (seen,ops) = sharingTypeOps seen ops tys
    in
      SharingTypeOps
        {seen = seen,
         ops = ops}
    end;

fun addSharingTypeOps ty share = addListSharingTypeOps [ty] share;

fun toSetSharingTypeOps (SharingTypeOps {ops,...}) = ops;

fun typeOpsList tys =
    let
      val share = emptySharingTypeOps

      val share = addListSharingTypeOps tys share
    in
      toSetSharingTypeOps share
    end;

fun typeOps ty = typeOpsList [ty];

(* ------------------------------------------------------------------------- *)
(* Primitive types.                                                          *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val stringBool = "bool";

val nameBool = Name.mkGlobal stringBool;

val typeOpBool =
    let
      val name = nameBool
      val prov = TypeTerm.UndefProvOpTy
    in
      TypeTerm.OpTy
        {name = name,
         prov = prov}
    end;

val bool = mkOp (typeOpBool,[]);

fun isBool ty =
    case dest ty of
      TypeTerm.OpTy' (ot,[]) => TypeOp.equal typeOpBool ot
    | _ => false;

(* Function spaces *)

val nameFun = TypeTerm.nameFunTy;

val typeOpFun = TypeTerm.opTyFunTy;

val mkFun = TypeTerm.mkFunTy;

val destFun = TypeTerm.destFunTy;

val isFun = TypeTerm.isFunTy;

fun domainFun ty = fst (destFun ty);

fun rangeFun ty = snd (destFun ty);

fun listMkFun (xs,ty) = List.foldl mkFun ty (rev xs);

local
  fun strip acc ty =
      case total destFun ty of
        NONE => (rev acc, ty)
      | SOME (x,ty) => strip (x :: acc) ty;
in
  val stripFun = strip [];
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val maximumSize = ref 1000;

val infixTokens =
    Print.Infixes
      [{token = "*", precedence = 3, assoc = Print.RightAssoc},
       {token = "+", precedence = 2, assoc = Print.RightAssoc},
       {token = "->", precedence = 1, assoc = Print.RightAssoc}];

local
  val typeInfixStrings = Print.tokensInfixes infixTokens;

  val ppTypeVar = Name.pp;

  fun destTypeInfix ty =
      let
        val (f,xs) = destOp ty

        val f = TypeOp.toString f

        val _ = StringSet.member f typeInfixStrings orelse
                raise Error "destTypeInfix"
      in
        case xs of
          [a,b] => (f,a,b)
        | _ => raise Bug ("destTypeInfix: bad arity of type operator " ^ f)
      end;

  val isTypeInfix = can destTypeInfix;

  fun ppTypeInfix tok =
      Print.program
        [Print.ppString " ",
         Print.ppString tok,
         Print.addBreak 1];

  val typeInfixPrinter =
      Print.ppInfixes infixTokens (total destTypeInfix) ppTypeInfix;

  fun basic ty =
      if isVar ty then ppTypeVar (destVar ty)
      else if isTypeInfix ty then ppBtype ty
      else
        let
          val (f,xs) = destOp ty
        in
          Print.blockProgram Print.Inconsistent 0
            [(case xs of
                [] => Print.skip
              | [x] => Print.sequence (basic ty) (Print.addBreak 1)
              | _ =>
                Print.sequence
                  (Print.ppBracket "(" ")" (Print.ppOpList "," ppTypeTop) xs)
                  (Print.addBreak 1)),
             TypeOp.pp f]
        end

  and basicr (ty,_) = basic ty

  and ppBtype ty = Print.ppBracket "(" ")" ppTypeTop ty

  and ppTyper tyr = typeInfixPrinter basicr tyr

  and ppTypeTop ty = ppTyper (ty,false);
in
  fun pp ty =
      let
        val n = size ty
      in
        if n <= !maximumSize then ppTypeTop ty
        else Print.ppBracket "type{" "}" Print.ppInt n
      end;
end;

val toString = Print.toString pp;

end

structure TypeOrdered =
struct type t = Type.ty val compare = Type.compare end

structure TypeMap = KeyMap (TypeOrdered)

structure TypeSet = ElementSet (TypeMap)
