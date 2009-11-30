(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYMBOL TABLES                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Symbol :> Symbol =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of symbol tables.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype symbol =
    Symbol of
      {opS : Term.sharingTypeOps,
       opM : TypeOp.typeOp NameMap.map,
       conS : Term.sharingConsts,
       conM : Const.const NameMap.map};

val empty =
    let
      val opS = Term.emptySharingTypeOps

      val opM = NameMap.new ()

      val conS = Term.emptySharingConsts

      val conM = NameMap.new ()
    in
      Symbol
        {opS = opS,
         opM = opM,
         conS = conS,
         conM = conM}
    end;

fun typeOps (Symbol {opS,...}) = Term.toSetSharingTypeOps opS;

fun consts (Symbol {conS,...}) = Term.toSetSharingConsts conS;

(* ------------------------------------------------------------------------- *)
(* Looking up entries.                                                       *)
(* ------------------------------------------------------------------------- *)

fun peekTypeOp (Symbol {opM,...}) n = NameMap.peek opM n;

fun peekConst (Symbol {conM,...}) n = NameMap.peek conM n;

fun knownTypeOp (Symbol {opM,...}) n = NameMap.inDomain n opM;

fun knownConst (Symbol {conM,...}) n = NameMap.inDomain n conM;

fun mkTypeOp syms n =
    let
      fun peek sym = peekTypeOp sym n
    in
      case List.mapPartial peek syms of
        [] => TypeOp.mkUndef n
      | ot :: ots =>
        if List.all (TypeOp.equal ot) ots then ot
        else raise Error "Symbol.mkTypeOp: duplicate type names"
    end;

fun mkConst syms n =
    let
      fun peek sym = peekConst sym n
    in
      case List.mapPartial peek syms of
        [] => Const.mkUndef n
      | c :: cs =>
        if List.all (Const.equal c) cs then c
        else raise Error "Symbol.mkConst: duplicate constant names"
    end;

(* ------------------------------------------------------------------------- *)
(* Adding entries.                                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun addOp (ot,m) =
      let
        val n = TypeOp.name ot
      in
        case NameMap.peek m n of
          NONE => NameMap.insert m (n,ot)
        | SOME ot' =>
          if TypeOp.equal ot ot' then m
          else raise Error "Symbol.addTypeOps: duplicate name"
      end;

  fun addCon (c,m) =
      let
        val n = Const.name c
      in
        case NameMap.peek m n of
          NONE => NameMap.insert m (n,c)
        | SOME c' =>
          if Const.equal c c' then m
          else raise Error "Symbol.addConsts: duplicate name"
      end;
in
  fun addX addXOp addXCon sym x =
      let
        val Symbol {opS,opM,conS,conM} = sym

        (* Add type operators in X *)

        val ots' = Term.toSetSharingTypeOps opS

        val opS = addXOp x opS

        val ots = Term.toSetSharingTypeOps opS

        val opM =
            if TypeOpSet.size ots = TypeOpSet.size ots' then opM
            else TypeOpSet.foldl addOp opM (TypeOpSet.difference ots ots')

        (* Add constants in X *)

        val cs' = Term.toSetSharingConsts conS

        val conS = addXCon x conS

        val cs = Term.toSetSharingConsts conS

        val conM =
            if ConstSet.size cs = ConstSet.size cs' then conM
            else ConstSet.foldl addCon conM (ConstSet.difference cs cs')
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

fun addNothing _ share = share;

val addTypeOp = addX Term.addTypeOpSharingTypeOps addNothing;

val addTypeOpSet = addX Term.addTypeOpSetSharingTypeOps addNothing;

val addConst = addX addNothing Term.addConstSharingConsts;

val addConstSet = addX addNothing Term.addConstSetSharingConsts;

val addType = addX Term.addTypeSharingTypeOps addNothing;

val addTerm = addX Term.addSharingTypeOps Term.addSharingConsts;

val addSequent = addX Sequent.addSharingTypeOps Sequent.addSharingConsts;

val addSequentSet =
    addX SequentSet.addSharingTypeOps SequentSet.addSharingConsts;

(* ------------------------------------------------------------------------- *)
(* Merging symbol tables.                                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun mergeTypeOps (ot1,ot2) =
      if TypeOp.equal ot1 ot2 then SOME ot2
      else raise Error ("Symbol.union: duplicate type op name" ^
                        Name.toString (TypeOp.name ot1));

  fun mergeConsts (c1,c2) =
      if Const.equal c1 c2 then SOME c2
      else raise Error ("Symbol.union: duplicate const name: " ^
                        Name.toString (Const.name c1));
in
  fun union sym1 sym2 =
      let
        val Symbol {opS = opS1, opM = opM1, conS = conS1, conM = conM1} = sym1
        and Symbol {opS = opS2, opM = opM2, conS = conS2, conM = conM2} = sym2

        val opM = NameMap.union mergeTypeOps opM1 opM2

        val conM = NameMap.union mergeConsts conM1 conM2

        val opS = Term.unionSharingTypeOps opS1 opS2

        val conS = Term.unionSharingConsts conS1 conS2
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

local
  fun uncurriedUnion (s1,s2) = union s1 s2;
in
  fun unionList syms =
      case syms of
        [] => empty
      | sym :: syms => List.foldl uncurriedUnion sym syms;
end;

(* ------------------------------------------------------------------------- *)
(* Partition symbol table entries into undefined and defined.                *)
(* ------------------------------------------------------------------------- *)

fun partitionUndef sym =
    let
      val sym1 = empty
      and sym2 = empty

      val (opS1,opS2) = TypeOpSet.partition TypeOp.isUndef (typeOps sym)

      val sym1 = addTypeOpSet sym1 opS1
      and sym2 = addTypeOpSet sym2 opS2

      val (conS1,conS2) = ConstSet.partition Const.isUndef (consts sym)

      val sym1 = addConstSet sym1 conS1
      and sym2 = addConstSet sym2 conS2
    in
      (sym1,sym2)
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppNameMap (name,nm) =
      let
        val ns = map fst (NameMap.toList nm)
      in
        if null ns then Print.skip
        else
          Print.sequence
            (Print.blockProgram Print.Inconsistent 2
               (Print.addString (name ^ ":") ::
                map (Print.sequence (Print.addBreak 1) o Name.pp) ns))
            Print.addNewline
      end;
in
  fun pp sym =
      let
        val Symbol {opM,conM,...} = sym
      in
        Print.blockProgram Print.Consistent 0
          [ppNameMap ("types",opM),
           ppNameMap ("consts",conM)]
      end;
end;

val toString = Print.toString pp;

end
