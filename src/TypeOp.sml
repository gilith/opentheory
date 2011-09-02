(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPE OPERATORS                                         *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure TypeOp :> TypeOp =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

type typeOp = TypeTerm.opTy;

type typeOpData =
     {name : Name.name,
      prov : TypeTerm.provOpTy};

val mk = TypeTerm.OpTy;

fun dest (TypeTerm.OpTy data) = data;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name = TypeTerm.nameOpTy;

val prov = TypeTerm.provOpTy;

fun mkUndef name =
    let
      val prov = TypeTerm.UndefProvOpTy
    in
      mk
        {name = name,
         prov = prov}
    end;

fun isUndef ot =
    case prov ot of
      TypeTerm.UndefProvOpTy => true
    | _ => false;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = TypeTerm.compareOpTy;

val equal = TypeTerm.equalOpTy;

(* ------------------------------------------------------------------------- *)
(* Reconstructing the arity from the provenance.                             *)
(* ------------------------------------------------------------------------- *)

fun varsDef ot =
    case prov ot of
      TypeTerm.UndefProvOpTy => NONE
    | TypeTerm.DefProvOpTy def =>
      let
        val TypeTerm.DefOpTy {vars = vs, ...} = def
      in
        SOME vs
      end;

fun arityDef ot =
    case varsDef ot of
      SOME vs => SOME (length vs)
    | NONE => NONE;

(* ------------------------------------------------------------------------- *)
(* Primitive type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val bool = mkUndef Name.boolTypeOp;

val isBool = equal bool;

(* Function spaces *)

val func = TypeTerm.opTyFunTy;

val isFun = equal func;

(* Individuals *)

val ind = mkUndef Name.indTypeOp;

val isInd = equal ind;

(* ------------------------------------------------------------------------- *)
(* Special syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

fun isList ot = Name.equal Name.listTypeOp (name ot);

fun isPair ot = Name.equal Name.pairTypeOp (name ot);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun ppWithShow show = Print.ppMap (Show.showName show o name) Name.pp;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun ppVars (ot,vso) =
      Print.inconsistentBlock 0
        [(case vso of
            NONE => Print.skip
          | SOME vs =>
            case vs of
              [] => Print.skip
            | [v] => Print.sequence (Name.pp v) Print.break
            | _ =>
              Print.sequence
                (Print.ppBracket "(" ")" (Print.ppOpList "," Name.pp) vs)
                Print.break),
         Name.pp (name ot)];
in
  fun toHtml show (ot,vso) =
      let
        val n = Show.showName show (name ot)

        val class = "type-operator"

        val title = Print.toString ppVars (ot,vso)

        val attrs = Html.fromListAttrs [("class",class),("title",title)]

        val inlines = Name.toHtml n
      in
        [Html.Span (attrs,inlines)]
      end;
end;

end

structure TypeOpOrdered =
struct type t = TypeOp.typeOp val compare = TypeOp.compare end

structure TypeOpMap = KeyMap (TypeOpOrdered)

structure TypeOpSet = ElementSet (TypeOpMap)
