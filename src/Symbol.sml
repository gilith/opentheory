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

fun mkTypeOp sym n =
    case peekTypeOp sym n of
      SOME ot => ot
    | NONE => TypeOp.mkUndef n;

fun mkConst sym n =
    case peekConst sym n of
      SOME c => c
    | NONE => Const.mkUndef n;

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
          else raise Error ("duplicate type operator name " ^
                            Name.quotedToString n)
      end;

  fun addCon (c,m) =
      let
        val n = Const.name c
      in
        case NameMap.peek m n of
          NONE => NameMap.insert m (n,c)
        | SOME c' =>
          if Const.equal c c' then m
          else raise Error ("duplicate constant name " ^
                            Name.quotedToString n)
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

fun addTypeOp sym ot =
    addX Term.addTypeOpSharingTypeOps addNothing sym ot
(*OpenTheoryDebug
    handle Error err => raise Error ("Symbol.addTypeOp: " ^ err);
*)

fun addTypeOpSet sym ots =
    addX Term.addTypeOpSetSharingTypeOps addNothing sym ots
(*OpenTheoryDebug
    handle Error err => raise Error ("Symbol.addTypeOpSet: " ^ err);
*)

fun addConst sym c =
    addX addNothing Term.addConstSharingConsts sym c
(*OpenTheoryDebug
    handle Error err => raise Error ("Symbol.addConst: " ^ err);
*)

fun addConstSet sym cs =
    addX addNothing Term.addConstSetSharingConsts sym cs
(*OpenTheoryDebug
    handle Error err => raise Error ("Symbol.addConstSet: " ^ err);
*)

fun addType sym ty =
    addX Term.addTypeSharingTypeOps addNothing sym ty
(*OpenTheoryDebug
    handle Error err => raise Error ("Symbol.addType: " ^ err);
*)

fun addVar sym v =
    addX Term.addTypeSharingTypeOps addNothing sym (Var.typeOf v)
(*OpenTheoryDebug
    handle Error err => raise Error ("Symbol.addVar: " ^ err);
*)

fun addTerm sym tm =
    addX Term.addSharingTypeOps Term.addSharingConsts sym tm
(*OpenTheoryDebug
    handle Error err => raise Error ("Symbol.addTerm: " ^ err);
*)

fun addSequent sym seq =
    addX Sequent.addSharingTypeOps Sequent.addSharingConsts sym seq
(*OpenTheoryDebug
    handle Error err => raise Error ("Symbol.addSequent: " ^ err);
*)

local
  fun add (seq,sym) = addSequent sym seq;
in
  fun addSequentList sym seql =
      List.foldl add sym seql
(*OpenTheoryDebug
      handle Error err => raise Error ("Symbol.addSequentList: " ^ err);
*)

  fun addSequentSet sym seqs =
      SequentSet.foldl add sym seqs
(*OpenTheoryDebug
      handle Error err => raise Error ("Symbol.addSequentSet: " ^ err);
*)
end;

(* ------------------------------------------------------------------------- *)
(* Merging symbol tables.                                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun mergeTypeOps ((n,ot1),(_,ot2)) =
      if TypeOp.equal ot1 ot2 then SOME ot2
      else raise Error ("duplicate type operator name " ^
                        Name.quotedToString n);

  fun mergeConsts ((n,c1),(_,c2)) =
      if Const.equal c1 c2 then SOME c2
      else raise Error ("duplicate constant name " ^
                        Name.quotedToString n);
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
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("Symbol.union: " ^ err);
*)

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
