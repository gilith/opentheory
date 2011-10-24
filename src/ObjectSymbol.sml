(* ========================================================================= *)
(* SYMBOL OBJECTS                                                            *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectSymbol :> ObjectSymbol =
struct

(* ------------------------------------------------------------------------- *)
(* A type of symbol objects.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype symbol =
    Symbol of
      {typeOps : ObjectProv.object TypeOpMap.map,
       consts : ObjectProv.object ConstMap.map};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val typeOps = TypeOpMap.new ()
      and consts = ConstMap.new ()
    in
      Symbol
        {typeOps = typeOps,
         consts = consts}
    end;

(* ------------------------------------------------------------------------- *)
(* Looking up symbols.                                                       *)
(* ------------------------------------------------------------------------- *)

fun peekTypeOp sym ot =
    let
      val Symbol {typeOps,...} = sym
    in
      TypeOpMap.peek typeOps ot
    end;

fun peekConst sym c =
    let
      val Symbol {consts,...} = sym
    in
      ConstMap.peek consts c
    end;

(* ------------------------------------------------------------------------- *)
(* Adding symbols.                                                           *)
(* ------------------------------------------------------------------------- *)

fun addTypeOp sym obj =
    let
      val Symbol {typeOps,consts} = sym

      val ot = ObjectProv.destTypeOp obj

      val typeOps = TypeOpMap.insert typeOps (ot,obj)
    in
      Symbol
        {typeOps = typeOps,
         consts = consts}
    end;

fun addConst sym obj =
    let
      val Symbol {typeOps,consts} = sym

      val c = ObjectProv.destConst obj

      val consts = ConstMap.insert consts (c,obj)
    in
      Symbol
        {typeOps = typeOps,
         consts = consts}
    end;

end
