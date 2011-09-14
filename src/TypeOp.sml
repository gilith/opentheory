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

(* ------------------------------------------------------------------------- *)
(* Debugging.                                                                *)
(* ------------------------------------------------------------------------- *)

local
  fun ppVars (n1,n2) =
      Print.consistentBlock 0
        [Print.ppString "different type definition variables:",
         Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
         Name.ppList n1,
         Print.break,
         Print.ppString "vs",
         Print.ppBreak (Print.Break {size = 1, extraIndent = 2}),
         Name.ppList n2];
in
  fun checkEqualVars n1 n2 =
      if Name.equalList n1 n2 then ()
      else raise Error (Print.toString ppVars (n1,n2));
end;

fun checkEqualDef chkTm d1 d2 =
    let
      val TypeTerm.DefOpTy {pred = p1, vars = v1} = d1
      and TypeTerm.DefOpTy {pred = p2, vars = v2} = d2

      val () =
          chkTm p1 p2
          handle Error err =>
            raise Error ("different type definition predicates:\n" ^ err)

      val () = checkEqualVars v1 v2
    in
      ()
    end
    handle Error err =>
      raise Error ("different type definitions:\n" ^ err);

fun checkEqualProv chkTm p1 p2 =
    (case (p1,p2) of
       (TypeTerm.UndefProvOpTy,TypeTerm.UndefProvOpTy) =>
       ()
     | (TypeTerm.UndefProvOpTy, TypeTerm.DefProvOpTy _) =>
       raise Error "undefined vs defined"
     | (TypeTerm.DefProvOpTy _, TypeTerm.UndefProvOpTy) =>
       raise Error "defined vs undefined"
     | (TypeTerm.DefProvOpTy d1, TypeTerm.DefProvOpTy d2) =>
       checkEqualDef chkTm d1 d2)
    handle Error err =>
      raise Error ("different type operator provenances: " ^ err);

fun checkEqual chkTm o1 o2 =
    let
      val TypeTerm.OpTy {name = n1, prov = p1} = o1
      and TypeTerm.OpTy {name = n2, prov = p2} = o2

      val () =
          if Name.equal n1 n2 then ()
          else raise Error "different type operator names"

      val () = checkEqualProv chkTm p1 p2
    in
      ()
    end
    handle Error err =>
      let
        val err =
            "different type operators: " ^
            toString o1 ^ " and " ^ toString o2 ^
            ":\n" ^ err
      in
        raise Error err
      end;

end

structure TypeOpOrdered =
struct type t = TypeOp.typeOp val compare = TypeOp.compare end

structure TypeOpMap = KeyMap (TypeOpOrdered)

structure TypeOpSet = ElementSet (TypeOpMap)
