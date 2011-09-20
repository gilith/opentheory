(* ========================================================================= *)
(* HIGHER ORDER LOGIC CONSTANTS                                              *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Const :> Const =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of constants.                                                      *)
(* ------------------------------------------------------------------------- *)

type const = TypeTerm.const;

type constData =
     {name : Name.name,
      prov : TypeTerm.provConst};

val mk = TypeTerm.Const;

fun dest (TypeTerm.Const data) = data;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name = TypeTerm.nameConst;

val prov = TypeTerm.provConst;

fun mkUndef name =
    let
      val prov = TypeTerm.UndefProvConst
    in
      mk
        {name = name,
         prov = prov}
    end;

fun isUndef c =
    case prov c of
      TypeTerm.UndefProvConst => true
    | _ => false;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = TypeTerm.compareConst;

val equal = TypeTerm.equalConst;

fun checkEqual (_ : TypeTerm.term -> TypeTerm.term -> unit) c1 c2 =
    if equal c1 c2 then ()
    else raise Error "constants not equal";

(* ------------------------------------------------------------------------- *)
(* Reconstructing the type from the provenance.                              *)
(* ------------------------------------------------------------------------- *)

local
  fun isoTys ot =
      let
        val (pred,tyVars) =
            case TypeOp.prov ot of
              TypeTerm.DefProvOpTy def =>
              let
                val TypeTerm.DefOpTy {pred,vars} = def
              in
                (pred,vars)
              end
            | _ => raise Bug "Const.typeOf.AbsProvConst: not a defined type op"

        val abs = Type.mkOp (ot, List.map Type.mkVar tyVars)
        and rep = Type.domainFun (TypeTerm.typeOf pred)
      in
        {abs = abs, rep = rep}
      end;
in
  fun typeOf c =
      case prov c of
        TypeTerm.UndefProvConst => NONE
      | TypeTerm.DefProvConst def =>
        let
          val TypeTerm.DefConst tm = def
        in
          SOME (TypeTerm.typeOf tm)
        end
      | TypeTerm.AbsProvConst ot =>
        let
          val {abs,rep} = isoTys ot
        in
          SOME (Type.mkFun (rep,abs))
        end
      | TypeTerm.RepProvConst ot =>
        let
          val {abs,rep} = isoTys ot
        in
          SOME (Type.mkFun (abs,rep))
        end;
end;

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val eq = mkUndef Name.eqConst;

val isEq = equal eq;

(* Hilbert's choice operator *)

val select = mkUndef Name.selectConst;

val isSelect = equal select;

(* ------------------------------------------------------------------------- *)
(* Special syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Boolean *)

fun isCond c = Name.equal Name.condConst (name c);

fun isConj c = Name.equal Name.conjConst (name c);

fun isDisj c = Name.equal Name.disjConst (name c);

fun isExists c = Name.equal Name.existsConst (name c);

fun isExistsUnique c = Name.equal Name.existsUniqueConst (name c);

fun isFalse c = Name.equal Name.falseConst (name c);

fun isForall c = Name.equal Name.forallConst (name c);

fun isImp c = Name.equal Name.impConst (name c);

fun isNeg c = Name.equal Name.negConst (name c);

fun isTrue c = Name.equal Name.trueConst (name c);

(* Natural numbers *)

fun isBit0 c = Name.equal Name.bit0Const (name c);

fun isBit1 c = Name.equal Name.bit1Const (name c);

fun isFromNatural c = Name.isFromNaturalConst (name c);

fun isZero c = Name.equal Name.zeroConst (name c);

(* Sets *)

fun isFromPredicate c = Name.equal Name.fromPredicateConst (name c);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun rename n s = Name.mk (Name.namespace n, s);

  fun renaming n s = (n, rename n s);

  val iffName = rename Name.eqConst Namespace.iffSyntaxComponent;

  val htmlRenaming =
      NameMap.fromList
        [renaming Name.composeConst Namespace.circLatexComponent,
         renaming Name.differenceConst Namespace.backslashLatexComponent,
         renaming Name.emptyConst Namespace.emptysetLatexComponent,
         renaming Name.intersectConst Namespace.capLatexComponent,
         renaming Name.memberConst Namespace.inLatexComponent,
         renaming Name.negConst Namespace.lnotLatexComponent,
         renaming Name.properSubsetConst Namespace.subsetLatexComponent,
         renaming Name.subsetConst Namespace.subseteqLatexComponent,
         renaming Name.unionConst Namespace.cupLatexComponent];

  fun eqName ty =
      let
        val b =
            case ty of
              SOME t => Type.isBoolEq t
            | NONE => false
      in
        if b then iffName else Name.eqConst
      end;

  fun textName (c,ty) = if isEq c then eqName ty else name c;

  fun htmlName (c,ty) =
      if isEq c then eqName ty
      else
        let
          val n = name c
        in
          case total Name.destCase n of
            SOME (n,_) => n
          | NONE => Option.getOpt (NameMap.peek htmlRenaming n, n)
        end;
in
  fun showName show cty = Show.showName show (textName cty);

  fun showNameHtml show cty = Show.showName show (htmlName cty);
end;

fun ppWithShow show c =
    let
      val n = showName show (c,NONE)
    in
      Name.pp n
    end;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun titleHtml show =
      let
        val ppTy = Type.ppHtml show
      in
        fn (c,ty) =>
           let
             val title = Html.encode (Name.toString (name c))
           in
             case ty of
               NONE => title
             | SOME t => title ^ " : " ^ Print.toLine ppTy t
           end
      end;
in
  fun toHtml show =
      let
        val mkTitle = titleHtml show
        and mkName = showNameHtml show
      in
        fn c_ty =>
           let
             val class = "const"

             val title = mkTitle c_ty

             val attrs = Html.fromListAttrs [("class",class),("title",title)]

             val inlines = Name.toHtml (mkName c_ty)
           in
             [Html.Span (attrs,inlines)]
           end
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Debugging.                                                                *)
(* ------------------------------------------------------------------------- *)

(*OpenTheoryDebug
local
  fun ppConsts (c1,c2) =
      Print.consistentBlock 2
        [Print.ppString "different constants:",
         Print.ppBreak (Print.Break {size = 1, extraIndent = 3}),
         pp c1,
         Print.break,
         Print.consistentBlock 3
           [Print.ppString "vs",
            Print.space,
            pp c2]];

  fun checkEqualDef chkTm d1 d2 =
      let
        val TypeTerm.DefConst tm1 = d1
        and TypeTerm.DefConst tm2 = d2
      in
        chkTm tm1 tm2
      end
      handle Error err =>
        raise Error ("different definitions:\n" ^ err);

  fun checkEqualTypeDef s chkTm o1 o2 =
      TypeOp.checkEqual chkTm o1 o2
      handle Error err =>
        raise Error ("different " ^ s ^ " definitions:\n" ^ err);

  fun checkEqualProv chkTm p1 p2 =
      (case (p1,p2) of
         (TypeTerm.UndefProvConst,TypeTerm.UndefProvConst) =>
         ()
       | (TypeTerm.UndefProvConst,_) =>
         raise Error " undefined vs defined"
       | (_,TypeTerm.UndefProvConst) =>
         raise Error " defined vs undefined"
       | (TypeTerm.DefProvConst d1, TypeTerm.DefProvConst d2) =>
         (checkEqualDef chkTm d1 d2
          handle Error err => raise Error ("\n" ^ err))
       | (TypeTerm.DefProvConst _, _) =>
         raise Error " definition vs abs/rep definition"
       | (_, TypeTerm.DefProvConst _) =>
         raise Error " abs/rep definition vs definition"
       | (TypeTerm.AbsProvConst o1, TypeTerm.AbsProvConst o2) =>
         (checkEqualTypeDef "abs" chkTm o1 o2
          handle Error err => raise Error ("\n" ^ err))
       | (TypeTerm.AbsProvConst _, _) =>
         raise Error " abs vs rep definition"
       | (_, TypeTerm.AbsProvConst _) =>
         raise Error " rep vs abs definition"
       | (TypeTerm.RepProvConst o1, TypeTerm.RepProvConst o2) =>
         (checkEqualTypeDef "rep" chkTm o1 o2
          handle Error err => raise Error ("\n" ^ err)))
      handle Error err =>
        raise Error ("different constant provenances:" ^ err);

  fun checkEqualConst chkTm c1 c2 =
      let
        val TypeTerm.Const {name = n1, prov = p1} = c1
        and TypeTerm.Const {name = n2, prov = p2} = c2

        val () =
            if Name.equal n1 n2 then ()
            else raise Error "different constant names"

        val () = checkEqualProv chkTm p1 p2
      in
        ()
      end
      handle Error err =>
        raise Error (Print.toString ppConsts (c1,c2) ^ "\n" ^ err);
in
  val checkEqual = fn chkTm => fn c1 => fn c2 =>
      let
        val () =
            checkEqual chkTm c1 c2
            handle Error _ =>
              let
                val () = checkEqualConst chkTm c1 c2
              in
                raise Bug "Const.checkEqual failed but debug version succeeded"
              end
      in
        ()
      end;
end;
*)

end

structure ConstOrdered =
struct type t = Const.const val compare = Const.compare end

structure ConstMap = KeyMap (ConstOrdered)

structure ConstSet = ElementSet (ConstMap)
