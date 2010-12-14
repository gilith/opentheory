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
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun ppWithShow show = Print.ppMap (Show.showName show o name) Name.pp;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

local
  fun ppVars (ot,vso) =
      Print.blockProgram Print.Inconsistent 0
        [(case vso of
            NONE => Print.skip
          | SOME vs =>
            case vs of
              [] => Print.skip
            | [v] => Print.sequence (Name.pp v) (Print.addBreak 1)
            | _ =>
              Print.sequence
                (Print.ppBracket "(" ")" (Print.ppOpList "," Name.pp) vs)
                (Print.addBreak 1)),
         Name.pp (name ot)];
in
  fun toHtml (ot_vso,n) =
      let
        val class = "type-operator"

        val title = Print.toString ppVars ot_vso

        val attrs = Html.fromListAttrs [("class",class),("title",title)]

        val inlines = Name.toHtml n
      in
        Html.Span (attrs,inlines)
      end;
end;

end

structure TypeOpOrdered =
struct type t = TypeOp.typeOp val compare = TypeOp.compare end

structure TypeOpMap = KeyMap (TypeOpOrdered)

structure TypeOpSet = ElementSet (TypeOpMap)
