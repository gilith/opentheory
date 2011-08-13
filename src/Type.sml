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

fun listMkFun (xs,ty) = List.foldl mkFun ty (rev xs);

local
  fun strip acc ty =
      case total destFun ty of
        NONE => (rev acc, ty)
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

      val _ = equal b bool orelse
              raise Error "Type.destEq: not a relation"

      val _ = equal x y orelse
              raise Error "Type.destEq: different argument types"
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

      val _ = isBool b orelse
              raise Error "Type.destSelect: not a predicate"

      val _ = equal x y orelse
              raise Error "Type.destSelect: different result type"
    in
      x
    end;

val isSelect = can destSelect;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {infixes : Print.infixes,
       ppVar : Name.name Print.pp,
       ppTypeOp : Show.show -> (TypeOp.typeOp * int) Print.pp,
       ppInfix : Show.show -> TypeOp.typeOp Print.pp,
       maximumSize : int};

local
  val infixes =
      Print.Infixes
        [{token = "*", precedence = 3, assoc = Print.RightAssoc},
         {token = "+", precedence = 2, assoc = Print.RightAssoc},
         {token = "->", precedence = 1, assoc = Print.RightAssoc}];

  local
    val pairName = Name.mkGlobal ",";
  in
    fun ppInfixBuffer ppInf ot_n =
        let
          val (_,n) = ot_n

          val pps = [ppInf ot_n, Print.addBreak 1]

          val pps =
              if Name.equal n pairName then pps
              else Print.ppString " " :: pps
        in
          Print.program pps
        end;
  end;

  val ppVar = Name.pp;

  fun ppTypeOp show =
      let
        fun toName (ot,_) = Show.showName show (TypeOp.name ot)
      in
        Print.ppMap toName Name.pp
      end;

  fun ppInfix show =
      let
        fun toName ot = (ot, Show.showName show (TypeOp.name ot))

        fun ppInf (_,n) = Name.pp n
      in
        Print.ppMap toName (ppInfixBuffer ppInf)
      end;

  val ppVarHtml = Print.ppMap Name.toHtml Html.ppFixed;

  fun ppTypeOpHtml show =
      let
        fun toName (ot,_) = Show.showName show (TypeOp.name ot)
      in
        Print.ppMap toName (Print.ppMap Name.toHtml Html.ppFixed)
      end;

  val ppInfixHtml = ppInfixBuffer (Print.ppMap snd Name.pp);

  fun ppInfixHtml show =
      let
        fun toName ot = (ot, Show.showName show (TypeOp.name ot))

        fun ppInf (_,n) = Html.ppFixed (Name.toHtml n)
      in
        Print.ppMap toName (ppInfixBuffer ppInf)
      end;

  val maximumSize = 1000;
in
  val defaultGrammar =
      Grammar
        {infixes = infixes,
         ppVar = ppVar,
         ppTypeOp = ppTypeOp,
         ppInfix = ppInfix,
         maximumSize = maximumSize};

  val htmlGrammar =
      Grammar
        {infixes = infixes,
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
              | SOME s' => raise Error ("Type.pp.mkMap: name clash: \"" ^
                                        s ^ "\" and \"" ^ s' ^ "\"")
            end

        val emptyMap : Print.token NameMap.map = NameMap.new ();
      in
        StringSet.foldl add emptyMap
      end;

  fun showTypeOp show ot = Show.showName show (TypeOp.name ot);

  fun ppType infixNames specialNames
             ppInfixes ppTypeOpName ppInfixName ppVar show =
      let
        fun ppTypeOp (ot,a) =
            let
              val n = showTypeOp show ot
            in
              if NameSet.member n specialNames then
                Print.ppBracket "(" ")" ppTypeOpName (ot,a)
              else
                ppTypeOpName (ot,a)
            end

        fun destInfixType ty =
            let
              val (ot,xs) = destOp ty

              val n = showTypeOp show ot
            in
              case xs of
                [a,b] => ((ot,n),a,b)
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
                Print.blockProgram Print.Inconsistent 0
                  [(case xs of
                      [] => Print.skip
                    | [x] => Print.sequence (ppBasicType x) (Print.addBreak 1)
                    | _ =>
                      Print.sequence
                        (Print.ppBracket "(" ")"
                           (Print.ppOpList "," ppNormalType) xs)
                        (Print.addBreak 1)),
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
        val Grammar {infixes,ppVar,ppTypeOp,ppInfix,maximumSize} = gram

        val ppInfixes = Print.ppInfixes infixes

        val infixNames = mkMap (Print.tokensInfixes infixes)

        val specialNames = NameSet.domain infixNames
      in
        fn show =>
           let
             val ppTypeOp = ppTypeOp show
             and ppInfix = ppInfix show
           in
             fn ty =>
                let
                  val n = size ty
                in
                  if n <= maximumSize then
                    ppType
                      infixNames specialNames
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

end

structure TypeOrdered =
struct type t = Type.ty val compare = Type.compare end

structure TypeMap = KeyMap (TypeOrdered)

structure TypeSet = ElementSet (TypeMap)
