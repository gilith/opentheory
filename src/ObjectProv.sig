(* ========================================================================= *)
(* OPENTHEORY OBJECTS THAT TRACK THEIR PROVENANCE                            *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature ObjectProv =
sig

(* ------------------------------------------------------------------------- *)
(* Objects that track their provenance.                                      *)
(* ------------------------------------------------------------------------- *)

type id = int

datatype object =
    Object of
      {id : id,
       object : Object.object,
       provenance : provenance,
       call : object option}

and provenance =
    Pnull
  | Pcall of object  (* the argument object for the call *)
  | Preturn of object
  | Pcons of object * object
  | Pref of object
  | Pthm of inference

and inference =
    Isaved of object
  | Isimulated of object  (* the call object simulated *)
  | Istack of object
  | Iknown of object
  | Iaxiom

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk :
    {object : Object.object,
     provenance : provenance,
     call : object option} -> object

val object : object -> Object.object

val provenance : object -> provenance

val call : object -> object option

val callStack : object -> object list

val destCall : object -> Name.name * object

val parents : object -> object list

val containsThms : object -> bool

val stackUses : object -> object list

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

val id : object -> id

val equalId : id -> object -> bool

val compare : object * object -> order

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
