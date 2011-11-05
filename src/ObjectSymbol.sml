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
      {typeOps : Object.object TypeOpMap.map,
       consts : Object.object ConstMap.map,
       seen : IntSet.set};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val typeOps = TypeOpMap.new ()
      and consts = ConstMap.new ()
      and seen = IntSet.empty
    in
      Symbol
        {typeOps = typeOps,
         consts = consts,
         seen = seen}
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
(* Raw functionality.                                                        *)
(* ------------------------------------------------------------------------- *)

fun addUnseen sym obj =
    let
      val Symbol {typeOps,consts,seen} = sym

      val i = Object.id obj
    in
      if IntSet.member i seen then NONE
      else
        let
          val seen = IntSet.add seen i

          val sym =
              Symbol
                {typeOps = typeOps,
                 consts = consts,
                 seen = seen}
        in
          SOME sym
        end
    end;

fun addTypeOpMap typeOps obj =
    let
      val ot = Object.destTypeOp obj

      val improvement =
          case TypeOpMap.peek typeOps ot of
            NONE => true
          | SOME obj' => Object.id obj < Object.id obj'
    in
      if not improvement then NONE
      else SOME (TypeOpMap.insert typeOps (ot,obj))
    end;

fun addConstMap consts obj =
    let
      val c = Object.destConst obj

      val improvement =
          case ConstMap.peek consts c of
            NONE => true
          | SOME obj' => Object.id obj < Object.id obj'
    in
      if not improvement then NONE
      else SOME (ConstMap.insert consts (c,obj))
    end;

(* ------------------------------------------------------------------------- *)
(* Adding symbols.                                                           *)
(* ------------------------------------------------------------------------- *)

fun addTypeOp sym obj =
    case addUnseen sym obj of
      NONE => sym
    | SOME sym =>
      let
        val Symbol {typeOps,consts,seen} = sym
      in
        case addTypeOpMap typeOps obj of
          NONE => sym
        | SOME typeOps =>
          Symbol
            {typeOps = typeOps,
             consts = consts,
             seen = seen}
      end;

fun addConst sym obj =
    case addUnseen sym obj of
      NONE => sym
    | SOME sym =>
      let
        val Symbol {typeOps,consts,seen} = sym
      in
        case addConstMap consts obj of
          NONE => sym
        | SOME consts =>
          Symbol
            {typeOps = typeOps,
             consts = consts,
             seen = seen}
      end;

(* ------------------------------------------------------------------------- *)
(* Harvesting symbols from objects (and their proofs).                       *)
(* ------------------------------------------------------------------------- *)

local
  fun preDescent obj sym =
      case addUnseen sym obj of
        NONE => {descend = false, result = sym}
      | SOME sym => {descend = true, result = sym};

  fun addSymbolObj obj sym =
      if Object.isTypeOp obj then
        let
          val Symbol {typeOps,consts,seen} = sym
        in
          case addTypeOpMap typeOps obj of
            NONE => sym
          | SOME typeOps =>
            Symbol
              {typeOps = typeOps,
               consts = consts,
               seen = seen}
        end
      else if Object.isConst obj then
        let
          val Symbol {typeOps,consts,seen} = sym
        in
          case addConstMap consts obj of
            NONE => sym
          | SOME consts =>
            Symbol
              {typeOps = typeOps,
               consts = consts,
               seen = seen}
        end
      else
        sym;

  fun addUnseenSymbolObj (obj,sym) =
      case addUnseen sym obj of
        NONE => sym
      | SOME sym => addSymbolObj obj sym;

  fun postDescent obj sym =
      let
        val sym = List.foldl addUnseenSymbolObj sym (Object.definitions obj)

        val sym = addSymbolObj obj sym
      in
        sym
      end;
in
  val addObject =
      Object.foldl
        {preDescent = preDescent,
         postDescent = postDescent};
end;

end
