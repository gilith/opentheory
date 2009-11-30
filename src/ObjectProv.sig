(* ========================================================================= *)
(* OPENTHEORY OBJECTS THAT TRACK THEIR PROVENANCE                            *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectProv =
sig

(* ------------------------------------------------------------------------- *)
(* A type of objects that track their provenance.                            *)
(* ------------------------------------------------------------------------- *)

type object

(* ------------------------------------------------------------------------- *)
(* A type of inferences.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype inference =
    Ialpha of object
  | Isimulated of object  (* the call object simulated *)
  | Iaxiom

(* ------------------------------------------------------------------------- *)
(* A type of provenances.                                                    *)
(* ------------------------------------------------------------------------- *)

datatype provenance =
    Pnull
  | Pcall of object  (* the argument object for the call *)
  | Pcons of object * object
  | Pref of object
  | Pthm of inference

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

type id = int

val id : object -> id

val equalId : id -> object -> bool

val compare : object * object -> order

(* ------------------------------------------------------------------------- *)
(* Destructors.                                                              *)
(* ------------------------------------------------------------------------- *)

datatype object' =
    Object of
      {id : id,
       object : Object.object,
       provenance : provenance}

val dest : object -> object'

val object : object -> Object.object

val provenance : object -> provenance

val destCall : object -> Name.name * object

val parents : object -> object list

val containsThms : object -> bool

val stackUses : object -> object list

(* ------------------------------------------------------------------------- *)
(* Symbols contained in objects.                                             *)
(* ------------------------------------------------------------------------- *)

val symbol : object -> Symbol.symbol

val symbolList : object list -> Symbol.symbol

val symbolAddList : Symbol.symbol -> object list -> Symbol.symbol

(* ------------------------------------------------------------------------- *)
(* Searching for theorems contained in objects.                              *)
(* ------------------------------------------------------------------------- *)

val search : object -> Sequent.sequent -> (Thm.thm * object) option

val searchList : object list -> Sequent.sequent -> (Thm.thm * object) option

(* ------------------------------------------------------------------------- *)
(* Constructing objects from commands.                                       *)
(* ------------------------------------------------------------------------- *)

val mkNum : int -> object

val mkName : Name.name -> object

val mkError : unit -> object

val mkNil : unit -> object

val mkCons : object -> object -> object

val mkTypeVar : object -> object

val mkTypeOp : TypeOp.typeOp -> object -> object

val mkVar : object -> object -> object

val mkConst : Const.const -> object -> object

val mkApp : object -> object -> object

val mkAbs : object -> object -> object

val mkThm : {savable : bool} -> Thm.thm -> inference -> object

val mkCall : Name.name -> object -> object

val mkReturn : {savable : bool} -> object -> object

val mkRef : {savable : bool} -> object -> object

val mkRemove : {savable : bool} -> object -> object

(* ------------------------------------------------------------------------- *)
(* Building objects.                                                         *)
(* ------------------------------------------------------------------------- *)

val build :
    {savable : bool} -> (Thm.thm -> inference) -> Object.object -> object

(* ------------------------------------------------------------------------- *)
(* Updating provenance (for compression).                                    *)
(* ------------------------------------------------------------------------- *)

val updateProvenance : object -> provenance -> object

(* ------------------------------------------------------------------------- *)
(* Mapping with state over objects.                                          *)
(* ------------------------------------------------------------------------- *)

val maps :
    {preDescent : object -> 's -> {descend : bool, result : object * 's},
     postDescent : object -> 's -> object * 's} ->
    object -> 's -> object  * 's

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : int -> object Print.pp

end
