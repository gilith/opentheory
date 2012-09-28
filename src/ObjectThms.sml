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
       seqs : Object.object SequentMap.map,
       export : ObjectExport.export};

fun thms (Thms {thms = x, ...}) = x;

fun toExport (Thms {export = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Converting between export sets of theorem objects.                        *)
(* ------------------------------------------------------------------------- *)

local
  fun split (th,(ths,seqs)) =
      let
        val ObjectThm.Thm {proof = obj, ...} = th

        val th = ObjectThm.thm th

        val seq = Thm.sequent th

        val ths = Thms.add ths th
        and seqs = SequentMap.insert seqs (seq,obj)
      in
        (ths,seqs)
      end;

  fun mkTypeOp savable sym (ot,otO) =
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
                raise Bug "ObjectThms.fromExport.mkTypeOp"
      in
        NameMap.insert otO (n,obj)
      end;

  fun mkConst savable sym (c,conO) =
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
                raise Bug "ObjectThms.fromExport.mkConst"
      in
        NameMap.insert conO (n,obj)
      end;
in
  fun fromExport exp =
      let
        val ths = Thms.empty
        and seqs = SequentMap.new ()

        val (ths,seqs) = ObjectExport.foldr split (ths,seqs) exp

        val sym = Thms.symbol ths

        val ots = SymbolTable.typeOps sym
        and cons = SymbolTable.consts sym

        val otO = NameMap.new ()
        and conO = NameMap.new ()

        val savable = ObjectExport.savable exp

        val sym =
            if savable then ObjectExport.symbol exp
            else ObjectSymbol.empty

        val otO = TypeOpSet.foldl (mkTypeOp savable sym) otO ots
        and conO = ConstSet.foldl (mkConst savable sym) conO cons
      in
        Thms
          {thms = ths,
           typeOps = otO,
           consts = conO,
           seqs = seqs,
           export = exp}
      end
end;

fun new sav = fromExport (ObjectExport.new sav);

val empty = new {savable = true};

(* ------------------------------------------------------------------------- *)
(* Looking up symbols and theorems.                                          *)
(* ------------------------------------------------------------------------- *)

fun peekThm (Thms {seqs,...}) seq = SequentMap.peek seqs seq;

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
  fun pickSnd (_,(_,x)) = SOME x;
in
  fun union thms1 thms2 =
      let
        val Thms
              {thms = ths1,
               typeOps = ots1,
               consts = cons1,
               seqs = seqs1,
               export = exp1} = thms1

        and Thms
              {thms = ths2,
               typeOps = ots2,
               consts = cons2,
               seqs = seqs2,
               export = exp2} = thms2

        val ths = Thms.union ths1 ths2
        and ots = NameMap.union pickSnd ots1 ots2
        and cons = NameMap.union pickSnd cons1 cons2
        and seqs = SequentMap.union pickSnd seqs1 seqs2
        and exp = ObjectExport.union exp1 exp2
      in
        Thms
          {thms = ths,
           typeOps = ots,
           consts = cons,
           seqs = seqs,
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
