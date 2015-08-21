(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYMBOLS                                                *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
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
(* Primitive symbols.                                                        *)
(* ------------------------------------------------------------------------- *)

val primitives =
    List.map TypeOp TypeOp.primitives @
    List.map Const Const.primitives;

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

val categorize =
    let
      fun cat (s, {typeOps = ts, consts = cs}) =
          case s of
            Symbol.TypeOp t => {typeOps = TypeOpSet.add ts t, consts = cs}
          | Symbol.Const c => {typeOps = ts, consts = ConstSet.add cs c}
    in
      foldl cat {typeOps = TypeOpSet.empty, consts = ConstSet.empty}
    end;

val primitives = fromList Symbol.primitives;

val fromTypeOpSet =
    let
      fun inc (t,s) = add s (Symbol.TypeOp t)
    in
      TypeOpSet.foldl inc empty
    end;

val fromConstSet =
    let
      fun inc (c,s) = add s (Symbol.Const c)
    in
      ConstSet.foldl inc empty
    end;

fun merge {typeOps,consts} =
    union (fromTypeOpSet typeOps) (fromConstSet consts);

local
  fun check xstr xsize (n,xs) =
      let
        val k = xsize xs
      in
        if k = 1 then ()
        else
          let
            val msg =
                Int.toString k ^ " different " ^ xstr ^ "s named " ^
                Name.toString n
          in
            Useful.warn msg
          end
      end;

  val checkTypeOp = check "type operator" TypeOpSet.size
  and checkConst = check "constant" ConstSet.size;
in
  fun warnClashing s =
      let
        val {typeOps = ts, consts = cs} = categorize s

        val () = NameMap.app checkTypeOp (TypeOpSet.alphabetize ts)
        and () = NameMap.app checkConst (ConstSet.alphabetize cs)
      in
        ()
      end;
end;

local
  fun intersperse x =
      let
        fun f h t =
            case t of
              [] => [h]
            | h' :: t => h :: x :: f h' t
      in
        fn [] => []
         | h :: t => f h t
      end;

  fun ppList ppX prefix name xs =
      let
        val n = List.length xs
      in
        if n = 0 then []
        else
          [Print.inconsistentBlock 2
             (Print.ppPrettyInt n ::
              Print.space ::
              Print.ppString prefix ::
              Print.space ::
              Print.ppString name ::
              (if n = 1 then Print.skip else Print.ppString "s") ::
              Print.ppString ":" ::
              List.map (Print.sequence Print.break o ppX) xs)]
      end;

  fun ppTypeOps prefix ts =
      ppList TypeOp.pp prefix "type operator" (TypeOpSet.toList ts);

  fun ppConsts prefix cs =
      ppList Const.pp prefix "constant" (ConstSet.toList cs);
in
  fun pp s =
      let
        val {typeOps = ts, consts = cs} = categorize s

        val (xts,dts) = TypeOpSet.partition TypeOp.isUndef ts
        and (xcs,dcs) = ConstSet.partition Const.isUndef cs

        val blocks =
            ppTypeOps "external" xts @
            ppConsts "external" xcs @
            ppTypeOps "defined" dts @
            ppConsts "defined" dcs
      in
        if List.null blocks then Print.skip
        else
          Print.consistentBlock 0
            (intersperse Print.newline blocks)
      end;
end;

end

structure SymbolGraph =
VertexGraph (
  structure KM = SymbolMap
  and ES = SymbolSet
);
