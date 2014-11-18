(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPE OPERATORS                                         *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
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

fun checkEqual (_ : TypeTerm.term -> TypeTerm.term -> unit) o1 o2 =
    if equal o1 o2 then ()
    else raise Error "type operators not equal";

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

(* The standard primitives *)

val primitives = [bool,func,ind];

(* ------------------------------------------------------------------------- *)
(* Special syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

fun isList ot = Name.equal Name.listTypeOp (name ot);

fun isPair ot = Name.equal Name.pairTypeOp (name ot);

fun isRandom ot = Name.equal Name.randomTypeOp (name ot);

fun isStream ot = Name.equal Name.streamTypeOp (name ot);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun showName show (ot, _ : int option) = Show.showName show (name ot);

fun ppWithShow show ot =
    let
      val n = showName show (ot,NONE)
    in
      Name.pp n
    end;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun rename n s = Name.mk (Name.namespace n, s);

  fun renaming n s = (n, rename n s);

  val htmlRenaming =
      NameMap.fromList
        [renaming Name.pairTypeOp Namespace.crossLatexComponent];

  fun htmlName (ot, _ : int option) =
      let
        val n = name ot
      in
        Option.getOpt (NameMap.peek htmlRenaming n, n)
      end;
in
  fun showNameHtml show oti = Show.showName show (htmlName oti);
end;

local
  val ppVar = Name.pp;

  fun ppVars vs =
      case vs of
        [v] => ppVar v
      | _ => Print.ppBracket "(" ")" (Print.ppOpList "," ppVar) vs;

  fun mkTitle vso ot =
      let
        val title = Html.encode (Name.toString (name ot))
      in
        case vso of
          NONE => title
        | SOME vs =>
          if List.null vs then title
          else Print.toLine ppVars vs ^ " " ^ title
      end;

  fun mkName show vso ot =
      let
        val io =
            case vso of
              SOME vs => SOME (List.length vs)
            | NONE => NONE
      in
        showNameHtml show (ot,io)
      end;
in
  fun toHtml show (ot,vso) =
      let
        val class = "type-operator"

        val title = mkTitle vso ot

        val attrs = Html.fromListAttrs [("class",class),("title",title)]

        val inlines = Name.toHtml (mkName show vso ot)
      in
        [Html.Span (attrs,inlines)]
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Debugging.                                                                *)
(* ------------------------------------------------------------------------- *)

(*OpenTheoryDebug
local
  fun ppVars (n1,n2) =
      Print.consistentBlock 2
        [Print.ppString "different type definition variables:",
         Print.ppBreak (Print.Break {size = 1, extraIndent = 3}),
         Name.ppList n1,
         Print.break,
         Print.consistentBlock 3
           [Print.ppString "vs",
            Print.space,
            Name.ppList n2]];

  fun ppTypeOps (o1,o2) =
      Print.consistentBlock 2
        [Print.ppString "different type operators:",
         Print.ppBreak (Print.Break {size = 1, extraIndent = 3}),
         pp o1,
         Print.break,
         Print.consistentBlock 3
           [Print.ppString "vs",
            Print.space,
            pp o2]];

  fun checkEqualVars n1 n2 =
      if Name.equalList n1 n2 then ()
      else raise Error (Print.toString ppVars (n1,n2));

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
         raise Error " undefined vs defined"
       | (TypeTerm.DefProvOpTy _, TypeTerm.UndefProvOpTy) =>
         raise Error " defined vs undefined"
       | (TypeTerm.DefProvOpTy d1, TypeTerm.DefProvOpTy d2) =>
         (checkEqualDef chkTm d1 d2
          handle Error err => raise Error ("\n" ^ err)))
      handle Error err =>
        raise Error ("different type operator provenances:" ^ err);

  fun checkEqualTypeOp chkTm o1 o2 =
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
        raise Error (Print.toString ppTypeOps (o1,o2) ^ "\n" ^ err);
in
  val checkEqual = fn chkTm => fn o1 => fn o2 =>
      let
        val () =
            checkEqual chkTm o1 o2
            handle Error _ =>
              let
                val () = checkEqualTypeOp chkTm o1 o2
              in
                raise Bug
                  "TypeOp.checkEqual failed but debug version succeeded"
              end
      in
        ()
      end;
end;
*)

end

structure TypeOpOrdered =
struct type t = TypeOp.typeOp val compare = TypeOp.compare end

structure TypeOpMap = KeyMap (TypeOpOrdered)

structure TypeOpSet =
struct

local
  structure S = ElementSet (TypeOpMap);
in
  open S;
end;

val alphabetize =
    let
      fun inc (t,nm) =
          let
            val n = TypeOp.name t

            val ts = Option.getOpt (NameMap.peek nm n, empty)
          in
            NameMap.insert nm (n, add ts t)
          end
    in
      foldl inc (NameMap.new ())
    end;

val pp = Print.ppBracket "{" "}" (Print.ppMap size Print.ppInt);

end
