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

fun mkTypeOp sym n =
    case peekTypeOp sym n of
      SOME t => t
    | NONE => TypeOp.mkUndef n;

fun mkConst sym n =
    case peekConst sym n of
      SOME c => c
    | NONE => Const.mkUndef n;

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
  fun addTypeOps sym s =
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
  fun addConsts sym s =
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

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppNameMap (name,nm) =
      let
        val ns = map fst (NameMap.toList nm)
      in
        Print.blockProgram Print.Inconsistent 2
          (Print.addString (name ^ ":") ::
           map (Print.sequence (Print.addBreak 1) o Name.pp) ns)
      end;

  fun ppSpacedNameMap isNull space name nm =
      if isNull then Print.skip
      else Print.sequence
             (if space then Print.addNewline else Print.skip)
             (ppNameMap (name,nm))
in
  fun pp (Symbol {opM,conM,...}) =
      let
        val nullOpM = NameMap.null opM
        and nullConM = NameMap.null conM

        val spaceOpM = false
        val spaceConM = not nullOpM
      in
        Print.blockProgram Print.Consistent 0
          [ppSpacedNameMap nullOpM spaceOpM "types" opM,
           ppSpacedNameMap nullConM spaceConM "consts" conM]
      end;
end;

val toString = Print.toString pp;

end
