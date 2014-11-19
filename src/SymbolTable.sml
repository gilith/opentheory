(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYMBOL TABLES                                          *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure SymbolTable :> SymbolTable =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of symbol tables.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype table =
    Table of
      {symS : SymbolSet.set,
       opS : Term.sharingTypeOps,
       opM : TypeOp.typeOp NameMap.map,
       conS : Term.sharingConsts,
       conM : Const.const NameMap.map};

val empty =
    let
      val symS = SymbolSet.empty
      and opS = Term.emptySharingTypeOps
      and opM = NameMap.new ()
      and conS = Term.emptySharingConsts
      and conM = NameMap.new ()
    in
      Table
        {symS = symS,
         opS = opS,
         opM = opM,
         conS = conS,
         conM = conM}
    end;

fun symbols (Table {symS,...}) = symS;

fun isEmpty sym = SymbolSet.null (symbols sym);

fun typeOps (Table {opS,...}) = Term.toSetSharingTypeOps opS;

fun consts (Table {conS,...}) = Term.toSetSharingConsts conS;

(* ------------------------------------------------------------------------- *)
(* Looking up entries by name.                                               *)
(* ------------------------------------------------------------------------- *)

fun peekTypeOp (Table {opM,...}) n = NameMap.peek opM n;

fun peekConst (Table {conM,...}) n = NameMap.peek conM n;

fun knownTypeOp (Table {opM,...}) n = NameMap.inDomain n opM;

fun knownConst (Table {conM,...}) n = NameMap.inDomain n conM;

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
  fun addOp (ot,(s,m)) =
      let
        val n = TypeOp.name ot

        val m =
            case NameMap.peek m n of
              NONE => NameMap.insert m (n,ot)
            | SOME ot' =>
              if TypeOp.equal ot ot' then m
              else
                let
                  val err =
                      "different type operators named " ^
                       Name.quotedToString n
                in
                  raise Error err
                end

        val s = SymbolSet.add s (Symbol.TypeOp ot)
      in
        (s,m)
      end;

  fun addCon (c,(s,m)) =
      let
        val n = Const.name c

        val m =
            case NameMap.peek m n of
              NONE => NameMap.insert m (n,c)
            | SOME c' =>
              if Const.equal c c' then m
              else
                let
                  val err =
                      "different constants named " ^
                       Name.quotedToString n
                in
                  raise Error err
                end

        val s = SymbolSet.add s (Symbol.Const c)
      in
        (s,m)
      end;
in
  fun addX addXOp addXCon sym x =
      let
        val Table {symS,opS,opM,conS,conM} = sym

        (* Add type operators in X *)

        val ots' = Term.toSetSharingTypeOps opS

        val opS = addXOp x opS

        val ots = Term.toSetSharingTypeOps opS

        val (symS,opM) =
            if TypeOpSet.size ots = TypeOpSet.size ots' then (symS,opM)
            else
              let
                val ots' = TypeOpSet.difference ots ots'
              in
                TypeOpSet.foldl addOp (symS,opM) ots'
              end

        (* Add constants in X *)

        val cs' = Term.toSetSharingConsts conS

        val conS = addXCon x conS

        val cs = Term.toSetSharingConsts conS

        val (symS,conM) =
            if ConstSet.size cs = ConstSet.size cs' then (symS,conM)
            else
              let
                val cs' = ConstSet.difference cs cs'
              in
                ConstSet.foldl addCon (symS,conM) cs'
              end
      in
        Table
          {symS = symS,
           opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

fun addNothing _ share = share;

fun addTypeOp sym ot =
    addX Term.addTypeOpSharingTypeOps addNothing sym ot
(*OpenTheoryDebug
    handle Error err => raise Error ("SymbolTable.addTypeOp: " ^ err);
*)

fun addTypeOpSet sym ots =
    addX Term.addTypeOpSetSharingTypeOps addNothing sym ots
(*OpenTheoryDebug
    handle Error err => raise Error ("SymbolTable.addTypeOpSet: " ^ err);
*)

fun addConst sym c =
    addX addNothing Term.addConstSharingConsts sym c
(*OpenTheoryDebug
    handle Error err => raise Error ("SymbolTable.addConst: " ^ err);
*)

fun addConstSet sym cs =
    addX addNothing Term.addConstSetSharingConsts sym cs
(*OpenTheoryDebug
    handle Error err => raise Error ("SymbolTable.addConstSet: " ^ err);
*)

local
  fun add (s,sym) =
      case s of
        Symbol.TypeOp t => addTypeOp sym t
      | Symbol.Const c => addConst sym c;
in
  fun addSymbol sym s =
      add (s,sym)
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addSymbol: " ^ err);
*)

  fun addSymbolList sym sl =
      List.foldl add sym sl
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addSymbolList: " ^ err);
*)

  fun addSymbolSet sym ss =
      SymbolSet.foldl add sym ss
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addSymbolSet: " ^ err);
*)
end;

fun addType sym ty =
    addX Term.addTypeSharingTypeOps addNothing sym ty
(*OpenTheoryDebug
    handle Error err => raise Error ("SymbolTable.addType: " ^ err);
*)

local
  fun add (ty,sym) = addType sym ty;
in
  fun addTypeList sym tyl =
      List.foldl add sym tyl
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addTypeList: " ^ err);
*)

  fun addTypeSet sym tys =
      TypeSet.foldl add sym tys
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addTypeSet: " ^ err);
*)
end;

fun addVar sym v =
    addX Term.addTypeSharingTypeOps addNothing sym (Var.typeOf v)
(*OpenTheoryDebug
    handle Error err => raise Error ("SymbolTable.addVar: " ^ err);
*)

local
  fun add (v,sym) = addVar sym v;
in
  fun addVarList sym vl =
      List.foldl add sym vl
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addVarList: " ^ err);
*)

  fun addVarSet sym vs =
      VarSet.foldl add sym vs
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addVarSet: " ^ err);
*)
end;

fun addTerm sym tm =
    addX Term.addSharingTypeOps Term.addSharingConsts sym tm
(*OpenTheoryDebug
    handle Error err => raise Error ("SymbolTable.addTerm: " ^ err);
*)

local
  fun add (tm,sym) = addTerm sym tm;
in
  fun addTermList sym tml =
      List.foldl add sym tml
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addTermList: " ^ err);
*)

  fun addTermSet sym tms =
      TermSet.foldl add sym tms
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addTermSet: " ^ err);
*)

  fun addTermAlphaSet sym tms =
      TermAlphaSet.foldl add sym tms
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addTermAlphaSet: " ^ err);
*)
end;

fun addSequent sym seq =
    addX Sequent.addSharingTypeOps Sequent.addSharingConsts sym seq
(*OpenTheoryDebug
    handle Error err => raise Error ("SymbolTable.addSequent: " ^ err);
*)

local
  fun add (seq,sym) = addSequent sym seq;
in
  fun addSequentList sym seql =
      List.foldl add sym seql
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addSequentList: " ^ err);
*)

  fun addSequentSet sym seqs =
      SequentSet.foldl add sym seqs
(*OpenTheoryDebug
      handle Error err => raise Error ("SymbolTable.addSequentSet: " ^ err);
*)
end;

(* ------------------------------------------------------------------------- *)
(* Merging symbol tables.                                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun mergeTypeOps ((n,ot1),(_,ot2)) =
      if TypeOp.equal ot1 ot2 then SOME ot2
      else
        let
          val err = "different type operators named " ^ Name.quotedToString n
        in
          raise Error err
        end;

  fun mergeConsts ((n,c1),(_,c2)) =
      if Const.equal c1 c2 then SOME c2
      else
        let
          val err = "different constants named " ^ Name.quotedToString n
        in
          raise Error err
        end;
in
  fun union sym1 sym2 =
      let
        val Table
              {symS = symS1,
               opS = opS1,
               opM = opM1,
               conS = conS1,
               conM = conM1} = sym1
        and Table
              {symS = symS2,
               opS = opS2,
               opM = opM2,
               conS = conS2,
               conM = conM2} = sym2

        val symS = SymbolSet.union symS1 symS2
        and opM = NameMap.union mergeTypeOps opM1 opM2
        and conM = NameMap.union mergeConsts conM1 conM2
        and opS = Term.unionSharingTypeOps opS1 opS2
        and conS = Term.unionSharingConsts conS1 conS2
      in
        Table
          {symS = symS,
           opS = opS,
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
      val usym = empty
      and dsym = empty

      val (uopS,dopS) = TypeOpSet.partition TypeOp.isUndef (typeOps sym)

      val usym = addTypeOpSet usym uopS
      and dsym = addTypeOpSet dsym dopS

      val (uconS,dconS) = ConstSet.partition Const.isUndef (consts sym)

      val usym = addConstSet usym uconS
      and dsym = addConstSet dsym dconS
    in
      {undefined = usym, defined = dsym}
    end;

fun undefined table =
    let
      val {undefined = x, defined = _} = partitionUndef table
    in
      x
    end;

fun defined table =
    let
      val {undefined = _, defined = x} = partitionUndef table
    in
      x
    end;

fun allUndefined table = isEmpty (defined table);

fun allDefined table = isEmpty (undefined table);

fun existsUndefined table = not (allDefined table);

fun existsDefined table = not (allUndefined table);

(* ------------------------------------------------------------------------- *)
(* Instantiating undefined type operators and constants with definitions.    *)
(* ------------------------------------------------------------------------- *)

fun instType sym ty' =
    case ty' of
      TypeTerm.VarTy' _ => NONE
    | TypeTerm.OpTy' (ot,tys) =>
      if not (TypeOp.isUndef ot) then NONE
      else
        let
          val n = TypeOp.name ot
        in
          case peekTypeOp sym n of
            NONE => NONE
          | SOME ot => SOME (Type.mkOp (ot,tys))
        end;

fun instTerm sym tm' =
    case tm' of
      TypeTerm.Const' (c,ty) =>
      if not (Const.isUndef c) then NONE
      else
        let
          val n = Const.name c
        in
          case peekConst sym n of
            NONE => NONE
          | SOME c => SOME (Term.mkConst (c,ty))
        end
    | _ => NONE;

fun inst sym =
    let
      val tyRewr = TypeRewrite.new (instType sym)
    in
      TermRewrite.new tyRewr (instTerm sym)
    end;

(* ------------------------------------------------------------------------- *)
(* Primitive symbols.                                                        *)
(* ------------------------------------------------------------------------- *)

val primitives = addSymbolSet empty SymbolSet.primitives;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppNameMap (name,nm) =
      let
        val ns = List.map fst (NameMap.toList nm)
      in
        if List.null ns then Print.skip
        else
          Print.sequence
            (Print.inconsistentBlock 2
               (Print.ppString name ::
                Print.ppString ":" ::
                List.map (Print.sequence Print.break o Name.pp) ns))
            Print.newline
      end;
in
  fun pp sym =
      let
        val Table {opM,conM,...} = sym
      in
        Print.consistentBlock 0
          [ppNameMap ("types",opM),
           ppNameMap ("consts",conM)]
      end;
end;

val toString = Print.toString pp;

end
