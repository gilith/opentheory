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

      val improvement =
          case TypeOpMap.peek typeOps ot of
            NONE => true
          | SOME obj' => ObjectProv.id obj < ObjectProv.id obj'
    in
      if not improvement then sym
      else
        let
          val typeOps = TypeOpMap.insert typeOps (ot,obj)
        in
          Symbol
            {typeOps = typeOps,
             consts = consts}
        end
    end;

fun addConst sym obj =
    let
      val Symbol {typeOps,consts} = sym

      val c = ObjectProv.destConst obj

      val improvement =
          case ConstMap.peek consts c of
            NONE => true
          | SOME obj' => ObjectProv.id obj < ObjectProv.id obj'
    in
      if not improvement then sym
      else
        let
          val consts = ConstMap.insert consts (c,obj)
        in
          Symbol
            {typeOps = typeOps,
             consts = consts}
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Harvesting symbols from proofs.                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun addObj seen_sym objs =
      case objs of
        [] => seen_sym
      | obj :: objs =>
        let
          val (seen,sym) = seen_sym

          val i = ObjectProv.id obj
        in
          if IntSet.member i seen then addObj seen_sym objs
          else
            let
              val seen = IntSet.add seen i

              val sym =
                  if ObjectProv.isTypeOp obj then addTypeOp sym obj
                  else if ObjectProv.isConst obj then addConst sym obj
                  else sym

              val objs = ObjectProv.parents obj @ objs
            in
              addObj (seen,sym) objs
            end
        end;

  fun addThm (th,seen_sym) =
      let
        val ObjectThm.Thm {proof = _, hyp, concl} = th
      in
        addObj seen_sym [hyp,concl]
      end;
in
  fun fromExport exp =
      let
        val seen_sym = (IntSet.empty,empty)

        val (_,sym) = ObjectExport.foldl addThm seen_sym exp
      in
        sym
      end;
end;

end
