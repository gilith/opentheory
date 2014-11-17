(* ========================================================================= *)
(* SYMBOL OBJECTS                                                            *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectSymbol :> ObjectSymbol =
struct

(* ------------------------------------------------------------------------- *)
(* A type of symbol objects.                                                 *)
(* ------------------------------------------------------------------------- *)

type symbol = ObjectStore.store;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      fun filter d =
          case d of
            ObjectData.TypeOp _ => true
          | ObjectData.Const _ => true
          | _ => false
    in
      ObjectStore.new {filter = filter}
    end;

(* ------------------------------------------------------------------------- *)
(* Looking up symbols.                                                       *)
(* ------------------------------------------------------------------------- *)

fun peekTypeOp sym ot = ObjectStore.peek sym (ObjectData.TypeOp ot);

fun peekConst sym c = ObjectStore.peek sym (ObjectData.Const c);

(* ------------------------------------------------------------------------- *)
(* Harvesting symbols from objects (and their provenances).                  *)
(* ------------------------------------------------------------------------- *)

val addObject = ObjectStore.add;

(* ------------------------------------------------------------------------- *)
(* Iterating over symbol objects.                                            *)
(* ------------------------------------------------------------------------- *)

val fold = ObjectStore.fold;

end
