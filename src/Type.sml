(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES                                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the MIT license            *)
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

fun checkEqual (_ : TypeTerm.term -> TypeTerm.term -> unit) ty1 ty2 =
    if equal ty1 ty2 then ()
    else raise Error "types not equal";

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

val alpha = mkVar (Name.mkGlobal "A");

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

val bool = mkOp (TypeOp.bool,[]);

fun isBool ty =
    case dest ty of
      TypeTerm.OpTy' (ot,[]) => TypeOp.isBool ot
    | _ => false;

(* Function spaces *)

val mkFun = TypeTerm.mkFunTy;

val destFun = TypeTerm.destFunTy;

val isFun = TypeTerm.isFunTy;

fun domainFun ty = fst (destFun ty);

fun rangeFun ty = snd (destFun ty);

fun listMkFun (xs,ty) = List.foldl mkFun ty (List.rev xs);

local
  fun strip acc ty =
      case total destFun ty of
        NONE => (List.rev acc, ty)
      | SOME (x,ty) => strip (x :: acc) ty;
in
  val stripFun = strip [];
end;

(* Individuals *)

val ind = mkOp (TypeOp.ind,[]);

fun isInd ty =
    case dest ty of
      TypeTerm.OpTy' (ot,[]) => TypeOp.isInd ot
    | _ => false;

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

fun mkEq a = mkFun (a, mkFun (a,bool));

fun destEq ty =
    let
      val (x,yb) = destFun ty

      val (y,b) = destFun yb

      val () = if equal b bool then ()
               else raise Error "Type.destEq: not a relation"

      val () = if equal x y then ()
               else raise Error "Type.destEq: different argument types"
    in
      x
    end;

val isEq = can destEq;

val boolEq = mkEq bool;

val isBoolEq = equal boolEq;

(* Hilbert's choice operator *)

fun mkSelect a = mkFun (mkFun (a,bool), a);

fun destSelect ty =
    let
      val (xb,y) = destFun ty

      val (x,b) = destFun xb

      val () = if isBool b then ()
               else raise Error "Type.destSelect: not a predicate"

      val () = if equal x y then ()
               else raise Error "Type.destSelect: different result type"
    in
      x
    end;

val isSelect = can destSelect;

(* ------------------------------------------------------------------------- *)
(* General syntax operations.                                                *)
(* ------------------------------------------------------------------------- *)

(* Nullary operators *)

fun destNullaryOp p ty =
    case dest ty of
      TypeTerm.VarTy' _ => raise Error "Type.destNullaryOp: variable"
    | TypeTerm.OpTy' (ot,tys) =>
      case tys of
        [] =>
        if p ot then ()
        else raise Error "Type.destNullaryOp: wrong type operator"
      | _ => raise Error "Type.destNullaryOp: wrong arity";

fun isNullaryOp p = can (destNullaryOp p);

(* Unary operators *)

fun destUnaryOp p ty =
    case dest ty of
      TypeTerm.VarTy' _ => raise Error "Type.destUnaryOp: variable"
    | TypeTerm.OpTy' (ot,tys) =>
      case tys of
        [a] =>
        if p ot then a
        else raise Error "Type.destUnaryOp: wrong type operator"
      | _ => raise Error "Type.destUnaryOp: wrong arity";

(* Binary operators *)

fun destBinaryOp p ty =
    case dest ty of
      TypeTerm.VarTy' _ => raise Error "Type.destBinaryOp: variable"
    | TypeTerm.OpTy' (ot,tys) =>
      case tys of
        [a,b] =>
        if p ot then (a,b)
        else raise Error "Type.destBinaryOp: wrong type operator"
      | _ => raise Error "Type.destBinaryOp: wrong arity";

fun stripBinaryOp p =
    let
      fun strip tys ty =
          case total (destBinaryOp p) ty of
            NONE => (tys,ty)
          | SOME (a,ty) => strip (a :: tys) ty
    in
      strip []
    end;

(* Ternary operators *)

fun destTernaryOp p ty =
    case dest ty of
      TypeTerm.VarTy' _ => raise Error "Type.destBinaryOp: variable"
    | TypeTerm.OpTy' (ot,tys) =>
      case tys of
        [a,b,c] =>
        if p ot then (a,b,c)
        else raise Error "Type.destBinaryOp: wrong type operator"
      | _ => raise Error "Type.destBinaryOp: wrong arity";

(* ------------------------------------------------------------------------- *)
(* Special syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Lists *)

fun destList ty =
    destUnaryOp TypeOp.isList ty
(*OpenTheoryDebug
    handle Error err => raise Error ("Type.destList:\n" ^ err);
*)

val isList = can destList;

(* Pairs *)

fun destPair ty =
    destBinaryOp TypeOp.isPair ty
(*OpenTheoryDebug
    handle Error err => raise Error ("Type.destPair:\n" ^ err);
*)

val isPair = can destPair;

(* Random streams *)

fun destRandom ty =
    destNullaryOp TypeOp.isRandom ty
(*OpenTheoryDebug
    handle Error err => raise Error ("Type.destRandom:\n" ^ err);
*)

val isRandom = can destRandom;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {infixes : Print.infixes,
       showTypeOp : Show.show -> TypeOp.typeOp * int -> Name.name,
       ppVar : Name.name Print.pp,
       ppTypeOp : Show.show -> (TypeOp.typeOp * int) Print.pp,
       ppInfix : Show.show -> TypeOp.typeOp Print.pp,
       maximumSize : int};

local
  val crossString = Namespace.crossLatexComponent
  and funString = Namespace.funTypeOpComponent
  and pairString = Namespace.pairTypeOpComponent
  and sumString = Namespace.sumTypeOpComponent;

  val infixes =
      Print.Infixes
        [(* Primitive *)
         {token = funString, precedence = 1, assoc = Print.RightAssoc},
         (* Products *)
         {token = pairString, precedence = 3, assoc = Print.RightAssoc},
         {token = crossString, precedence = 3, assoc = Print.RightAssoc},
         (* Sums *)
         {token = sumString, precedence = 2, assoc = Print.RightAssoc}];

  fun ppInfixBuffer ppInf ot_n =
      Print.program [Print.space, ppInf ot_n, Print.break];

  (* Plain text *)

  fun showTypeOp show (ot,i) = TypeOp.showName show (ot, SOME i);

  val ppVar = Name.pp;

  fun ppTypeOp show =
      let
        fun toName oti = showTypeOp show oti
      in
        Print.ppMap toName Name.pp
      end;

  fun ppInfix show =
      let
        fun toName ot = (ot, showTypeOp show (ot,2))

        fun ppInf (_,n) = Name.pp n
      in
        Print.ppMap toName (ppInfixBuffer ppInf)
      end;

  (* HTML *)

  fun showTypeOpHtml show (ot,i) = TypeOp.showNameHtml show (ot, SOME i);

  val ppVarHtml = Print.ppMap Name.toHtml Html.ppFixed;

  fun ppTypeOpHtml show =
      let
        fun toName oti = showTypeOpHtml show oti
      in
        Print.ppMap toName (Print.ppMap Name.toHtml Html.ppFixed)
      end;

  fun ppInfixHtml show =
      let
        fun toName ot = (ot, showTypeOpHtml show (ot,2))

        fun ppInf (_,n) = Html.ppFixed (Name.toHtml n)
      in
        Print.ppMap toName (ppInfixBuffer ppInf)
      end;

  val maximumSize = 10000;
in
  val defaultGrammar =
      Grammar
        {infixes = infixes,
         showTypeOp = showTypeOp,
         ppVar = ppVar,
         ppTypeOp = ppTypeOp,
         ppInfix = ppInfix,
         maximumSize = maximumSize};

  val htmlGrammar =
      Grammar
        {infixes = infixes,
         showTypeOp = showTypeOpHtml,
         ppVar = ppVarHtml,
         ppTypeOp = ppTypeOpHtml,
         ppInfix = ppInfixHtml,
         maximumSize = maximumSize};
end;

local
  val mkName = Name.mkGlobal;

  val mkMap =
      let
        fun add (s,m) =
            let
              val n = mkName s
            in
              case NameMap.peek m n of
                NONE => NameMap.insert m (n,s)
              | SOME s' =>
                let
                  val bug =
                      "Type.pp.mkMap: name clash: \"" ^
                      s ^ "\" and \"" ^ s' ^ "\""
                in
                  raise Bug bug
                end
            end

        val emptyMap : Print.token NameMap.map = NameMap.new ();
      in
        StringSet.foldl add emptyMap
      end;

  fun ppType infixNames specialNames showTypeOp
             ppInfixes ppTypeOpName ppInfixName ppVar show =
      let
        fun ppTypeOp oti =
            let
              val n = showTypeOp oti
            in
              if NameSet.member n specialNames then
                Print.ppBracket "(" ")" ppTypeOpName oti
              else
                ppTypeOpName oti
            end

        fun destInfixType ty =
            let
              val (ot,xs) = destOp ty
            in
              case xs of
                [a,b] => ((ot, showTypeOp (ot,2)), a, b)
              | _ => raise Error "Type.pp.destInfixType"
            end;

        fun destInfix ty =
            let
              val ((_,n),a,b) = destInfixType ty
            in
              case NameMap.peek infixNames n of
                SOME s => (s,a,b)
              | NONE => raise Error "Type.pp.destInfix"
            end

        val isInfix = can destInfix

        fun ppInfixToken (ty,_) =
            let
              val ((ot,_),_,_) = destInfixType ty
            in
              ppInfixName ot
            end

        val ppInfix = ppInfixes (total destInfix) ppInfixToken

        fun ppBasicType ty =
            if isVar ty then ppVar (destVar ty)
            else if isInfix ty then ppBracketType ty
            else
              let
                val (ot,xs) = destOp ty
              in
                case xs of
                  [] => ppTypeOp (ot,0)
                | [x] =>
                  Print.inconsistentBlock 0
                    [ppBasicType x,
                     Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
                     ppTypeOp (ot,1)]
                | _ =>
                  Print.inconsistentBlock 0
                    [Print.ppBracket "(" ")"
                       (Print.ppOpList "," ppNormalType) xs,
                     Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
                     ppTypeOp (ot, length xs)]
              end

        and ppBasicHangingType (ty,_) = ppBasicType ty

        and ppHangingType ty_r = ppInfix ppBasicHangingType ty_r

        and ppNormalType ty = ppHangingType (ty,false)

        and ppBracketType ty = Print.ppBracket "(" ")" ppNormalType ty
      in
        ppNormalType
      end;
in
  fun ppWithGrammar gram =
      let
        val Grammar
              {infixes,showTypeOp,
               ppVar,ppTypeOp,ppInfix,
               maximumSize} = gram

        val ppInfixes = Print.ppInfixes infixes

        val infixNames = mkMap (Print.tokensInfixes infixes)

        val specialNames = NameSet.domain infixNames
      in
        fn show =>
           let
             val showTypeOp = showTypeOp show
             and ppTypeOp = ppTypeOp show
             and ppInfix = ppInfix show
           in
             fn ty =>
                let
                  val n = size ty
                in
                  if n <= maximumSize then
                    ppType
                      infixNames specialNames showTypeOp
                      ppInfixes ppTypeOp ppInfix ppVar show ty
                  else
                    let
                      val () = warn "type too large to print"
                    in
                      Print.ppBracket "type{" "}" Print.ppInt n
                    end
                end
           end
      end;
end;

val ppWithShow = ppWithGrammar defaultGrammar;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val ppHtml = ppWithGrammar htmlGrammar;

(* ------------------------------------------------------------------------- *)
(* Debugging.                                                                *)
(* ------------------------------------------------------------------------- *)

(*OpenTheoryDebug
local
  fun ppPath path = Print.program (List.map Print.ppInt (List.rev path));

  fun ppTypes ppIntro (ty1,ty2) =
      Print.consistentBlock 2
        [ppIntro,
         Print.ppBreak (Print.Break {size = 1, extraIndent = 3}),
         pp ty1,
         Print.break,
         Print.consistentBlock 3
           [Print.ppString "vs",
            Print.space,
            pp ty2]];

  fun ppComplaint (err,path,ty1,ty2) =
      if List.null path then
        Print.consistentBlock 0
          [Print.ppString err,
           Print.ppString " at type root"]
      else
        let
          val ppIntro =
              Print.consistentBlock 0
                [Print.ppString err,
                 Print.ppString " at path ",
                 ppPath path,
                 Print.ppString " subtypes:"]
        in
          ppTypes ppIntro (ty1,ty2)
        end;

  fun complain err path ty1 ty2 =
      Print.toString ppComplaint (err,path,ty1,ty2);

  fun check chkTm path ty1 ty2 =
      case (dest ty1, dest ty2) of
        (TypeTerm.VarTy' n1, TypeTerm.VarTy' n2) =>
        if Name.equal n1 n2 then ()
        else
          let
            val err = complain "different type variables" path ty1 ty2
          in
            raise Error err
          end
      | (TypeTerm.VarTy' _, TypeTerm.OpTy' _) =>
        raise Error (complain "different type structure" path ty1 ty2)
      | (TypeTerm.OpTy' _, TypeTerm.VarTy' _) =>
        raise Error (complain "different type structure" path ty1 ty2)
      | (TypeTerm.OpTy' (o1,tys1), TypeTerm.OpTy' (o2,tys2)) =>
        let
          val () =
              TypeOp.checkEqual chkTm o1 o2
              handle Error err =>
                let
                  val err =
                      complain "different type operators" path ty1 ty2 ^
                      "\n" ^ err
                in
                  raise Error err
                end

          val () =
              if List.length tys1 = List.length tys2 then ()
              else
                let
                  val err =
                      complain "different type operator arities" path ty1 ty2
                in
                  raise Error err
                end

          val tys = enumerate (zip tys1 tys2)

          val () = List.app (checkArg chkTm path) tys
        in
          ()
        end

  and checkArg chkTm path (n,(ty1,ty2)) =
      let
        val path = n :: path
      in
        check chkTm path ty1 ty2
      end;

  fun checkRoot chkTm ty1 ty2 =
      check chkTm [] ty1 ty2
      handle Error err =>
        let
          val ppTys = ppTypes (Print.ppString "different types:")
        in
          raise Error (Print.toString ppTys (ty1,ty2) ^ "\n" ^ err)
        end;
in
  val checkEqual = fn chkTm => fn ty1 => fn ty2 =>
      let
        val () =
            checkEqual chkTm ty1 ty2
            handle Error _ =>
              let
                val () = checkRoot chkTm ty1 ty2
              in
                raise Bug "Type.checkEqual failed but debug version succeeded"
              end
      in
        ()
      end;
end;
*)

end

structure TypeOrdered =
struct type t = Type.ty val compare = Type.compare end

structure TypeMap = KeyMap (TypeOrdered)

structure TypeSet = ElementSet (TypeMap)
