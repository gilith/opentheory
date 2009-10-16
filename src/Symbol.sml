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
      {opS : TypeOpSet.set,
       opM : TypeOp.typeOp NameMap.map,
       conS : ConstSet.set,
       conM : Const.const NameMap.map};

val empty =
    let
      val opS = TypeOpSet.empty
      val opM = NameMap.new ()
      val conS = ConstSet.empty
      val conM = NameMap.new ()
    in
      Symbol
        {opS = opS,
         opM = opM,
         conS = conS,
         conM = conM}
    end;

fun typeOps (Symbol {opS,...}) = opS;

fun consts (Symbol {conS,...}) = conS;

(* ------------------------------------------------------------------------- *)
(* Looking up entries.                                                       *)
(* ------------------------------------------------------------------------- *)

fun peekTypeOp (Symbol {opM,...}) n = NameMap.peek opM n;

fun peekConst (Symbol {conM,...}) n = NameMap.peek conM n;

fun mkTypeOp syms n =
    let
      fun peek sym = peekTypeOp sym n
    in
      case List.mapPartial peek syms of
        [] => TypeOp.mkUndef n
      | [t] => t
      | _ :: _ :: _ => raise Error "Symbol.mkTypeOp: duplicate type names"
    end;

fun mkConst syms n =
    let
      fun peek sym = peekConst sym n
    in
      case List.mapPartial peek syms of
        [] => Const.mkUndef n
      | [c] => c
      | _ :: _ :: _ => raise Error "Symbol.mkConst: duplicate constant names"
    end;

(* ------------------------------------------------------------------------- *)
(* Adding entries.                                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun add (ot,m) =
      let
        val n = TypeOp.name ot
      in
        case NameMap.peek m n of
          NONE => NameMap.insert m (n,ot)
        | SOME ot' =>
          if TypeOp.equal ot ot' then m
          else raise Error "Symbol.addTypeOps: duplicate name"
      end;
in
  fun addTypeOp sym ot =
      let
        val Symbol {opS,opM,conS,conM} = sym
        val opM = add (ot,opM)
        val opS = TypeOpSet.add opS ot
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;

  fun addTypeOpSet sym s =
      let
        val Symbol {opS,opM,conS,conM} = sym
        val opM = TypeOpSet.foldl add opM s
        val opS = TypeOpSet.union opS s
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

local
  fun add (c,m) =
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
  fun addConst sym c =
      let
        val Symbol {opS,opM,conS,conM} = sym
        val conM = add (c,conM)
        val conS = ConstSet.add conS c
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;

  fun addConstSet sym s =
      let
        val Symbol {opS,opM,conS,conM} = sym
        val conM = ConstSet.foldl add conM s
        val conS = ConstSet.union conS s
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

fun addSequent sym seq =
    let
      val sym = addTypeOpSet sym (Sequent.typeOps seq)
      val sym = addConstSet sym (Sequent.consts seq)
    in
      sym
    end;

fun addSequentSet sym seqs =
    let
      val sym = addTypeOpSet sym (SequentSet.typeOps seqs)
      val sym = addConstSet sym (SequentSet.consts seqs)
    in
      sym
    end;

(* ------------------------------------------------------------------------- *)
(* Merging symbol tables.                                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun mergeTypeOps (ot1,ot2) =
      if TypeOp.equal ot1 ot2 then SOME ot2
      else raise Error "Symbol.union: duplicate type op name";

  fun mergeConsts (c1,c2) =
      if Const.equal c1 c2 then SOME c2
      else raise Error "Symbol.union: duplicate const name";
in
  fun union sym1 sym2 =
      let
        val Symbol {opS = opS1, opM = opM1, conS = conS1, conM = conM1} = sym1
        and Symbol {opS = opS2, opM = opM2, conS = conS2, conM = conM2} = sym2

        val opM = NameMap.union mergeTypeOps opM1 opM2

        val conM = NameMap.union mergeConsts conM1 conM2

        val opS = TypeOpSet.union opS1 opS2

        val conS = ConstSet.union conS1 conS2
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Partition symbol table entries into undefined and defined.                *)
(* ------------------------------------------------------------------------- *)

fun partitionUndef sym =
    let
      val Symbol {opS,conS,...} = sym

      val sym1 = empty
      and sym2 = empty

      val (opS1,opS2) = TypeOpSet.partition TypeOp.isUndef opS

      val sym1 = addTypeOpSet sym1 opS1
      and sym2 = addTypeOpSet sym2 opS2

      val (conS1,conS2) = ConstSet.partition Const.isUndef conS

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