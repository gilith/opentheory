(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYMBOLS                                                *)
(* Copyright (c) 2011 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure Symbol :> Symbol =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of symbols.                                                        *)
(* ------------------------------------------------------------------------- *)

datatype symbol =
    TypeOp of TypeOp.typeOp
  | Const of Const.const;

(* ------------------------------------------------------------------------- *)
(* Destructors.                                                              *)
(* ------------------------------------------------------------------------- *)

fun name sym =
    case sym of
      TypeOp ot => TypeOp.name ot
    | Const c => Const.name c;

fun isUndef sym =
    case sym of
      TypeOp ot => TypeOp.isUndef ot
    | Const c => Const.isUndef c;

(* ------------------------------------------------------------------------- *)
(* A total order on symbols.                                                 *)
(* ------------------------------------------------------------------------- *)

fun compare (sym1,sym2) =
    case (sym1,sym2) of
      (TypeOp ot1, TypeOp ot2) => TypeOp.compare (ot1,ot2)
    | (TypeOp _, Const _) => LESS
    | (Const _, TypeOp _) => GREATER
    | (Const c1, Const c2) => Const.compare (c1,c2);

fun equal sym1 sym2 = compare (sym1,sym2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap name Name.pp;

val toString = Print.toString pp;

end

structure SymbolOrdered =
struct
  type t = Symbol.symbol;

  val compare = Symbol.compare;
end

structure SymbolMap =
struct

  local
    structure S = KeyMap (SymbolOrdered);
  in
    open S;
  end;

end

structure SymbolSet =
struct

  local
    structure S = ElementSet (SymbolMap);
  in
    open S;
  end;

  local
    val ppSymList = Print.ppBracket "{" "}" (Print.ppOpList "," Symbol.pp);
  in
    val pp = Print.ppMap toList ppSymList;
  end;

end

structure SymbolGraph =
VertexGraph (
  structure KM = SymbolMap
  and ES = SymbolSet
);
