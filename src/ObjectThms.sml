(* ========================================================================= *)
(* SYMBOLS CONTAINED IN A SET OF THEOREM OBJECTS                             *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectThms :> ObjectThms =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of object set theorems.                                            *)
(* ------------------------------------------------------------------------- *)

datatype thms =
    Thms of
      {thms : Thms.thms,
       typeOps : Object.object NameMap.map,
       consts : Object.object NameMap.map,
       export : ObjectExport.export};

fun thms (Thms {thms = x, ...}) = x;

fun size ths = Thms.size (thms ths);

fun toExport (Thms {export = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Converting between export sets of theorem objects.                        *)
(* ------------------------------------------------------------------------- *)

local
  fun addTypeOp savable sym (ot,otO) =
      let
        val n = TypeOp.name ot

        val obj =
            case ObjectSymbol.peekTypeOp sym ot of
              SOME obj => obj
            | NONE =>
              if TypeOp.isUndef ot then
                Object.mkTypeOp n
              else if not savable then
                Object.mkUnsavable (ObjectData.TypeOp ot)
              else
                raise Bug "ObjectThms.fromExport.addTypeOp"
      in
        NameMap.insert otO (n,obj)
      end;

  fun addConst savable sym (c,conO) =
      let
        val n = Const.name c

        val obj =
            case ObjectSymbol.peekConst sym c of
              SOME obj => obj
            | NONE =>
              if Const.isUndef c then
                Object.mkConst n
              else if not savable then
                Object.mkUnsavable (ObjectData.Const c)
              else
                raise Bug "ObjectThms.fromExport.addConst"
      in
        NameMap.insert conO (n,obj)
      end;
in
  fun fromExport exp =
      let
        val ths = ObjectExport.toThms exp

        val sym = Thms.symbol ths

        val ots = SymbolTable.typeOps sym
        and cons = SymbolTable.consts sym

        val otO = NameMap.new ()
        and conO = NameMap.new ()

        val savable = ObjectExport.savable exp

        val sym =
            if savable then ObjectExport.thmDefinitions exp
            else ObjectSymbol.empty

        val otO = TypeOpSet.foldl (addTypeOp savable sym) otO ots
        and conO = ConstSet.foldl (addConst savable sym) conO cons
      in
        Thms
          {thms = ths,
           typeOps = otO,
           consts = conO,
           export = exp}
      end
end;

fun new sav = fromExport (ObjectExport.empty sav);

val empty = new {savable = true};

(* ------------------------------------------------------------------------- *)
(* Looking up symbols and theorems.                                          *)
(* ------------------------------------------------------------------------- *)

fun peekThm (Thms {export,...}) seq =
    case ObjectExport.peek export seq of
      NONE => NONE
    | SOME th => SOME (ObjectThm.proof th);

fun peekTypeOp (Thms {typeOps,...}) n = NameMap.peek typeOps n;

fun peekConst (Thms {consts,...}) n = NameMap.peek consts n;

fun peekSpecificTypeOp ths ot =
    case peekTypeOp ths (TypeOp.name ot) of
      NONE => NONE
    | SOME obj => if Object.equalTypeOp ot obj then SOME obj else NONE;

fun peekSpecificConst ths c =
    case peekConst ths (Const.name c) of
      NONE => NONE
    | SOME obj => if Object.equalConst c obj then SOME obj else NONE;

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun pickEarliest ((_,obj1),(_,obj2)) =
      let
        val obj = if Object.id obj1 <= Object.id obj2 then obj1 else obj2
      in
        SOME obj
      end;
in
  fun union thms1 thms2 =
      let
        val Thms
              {thms = ths1,
               typeOps = ots1,
               consts = cons1,
               export = exp1} = thms1

        and Thms
              {thms = ths2,
               typeOps = ots2,
               consts = cons2,
               export = exp2} = thms2

        val ths = Thms.union ths1 ths2
        and ots = NameMap.union pickEarliest ots1 ots2
        and cons = NameMap.union pickEarliest cons1 cons2
        and exp = ObjectExport.union exp1 exp2
      in
        Thms
          {thms = ths,
           typeOps = ots,
           consts = cons,
           export = exp}
      end;
end;

local
  fun uncurriedUnion (thms1,thms2) = union thms1 thms2;
in
  fun unionList thmsl =
      case thmsl of
        [] => empty
      | thms :: thmsl => List.foldl uncurriedUnion thms thmsl;
end;

end
