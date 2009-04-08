(* ========================================================================= *)
(* OPENTHEORY OBJECTS THAT TRACK THEIR PROVENANCE                            *)
(* Copyright (c) 2004-2008 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature ObjectProv =
sig

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

type id = int

(* ------------------------------------------------------------------------- *)
(* Objects that track their provenance.                                      *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Object of
      {id : id,
       object : Object.object,
       provenance : provenance,
       call : object option}

and provenance =
    Pnull
  | Pcall of object
  | Preturn of object
  | Pcons of object * object
  | Pref of object
  | Pthm of inference

and inference =
    Isaved of object
  | Isimulated
  | Istack of object
  | Iaxiom

val compare : object * object -> order

val mk :
    {object : Object.object,
     provenance : provenance,
     call : object option} -> object

val id : object -> id

val object : object -> Object.object

val provenance : object -> provenance

val call : object -> object option

val callStack : object -> object list

val destCall : object -> Name.name * object

val parents : object -> object list

val containsThms : object -> bool

(* ------------------------------------------------------------------------- *)
(* Mapping with state over objects.                                          *)
(* ------------------------------------------------------------------------- *)

val maps :
    {preDescent : object -> 's -> {descend : bool, result : object * 's},
     postDescent : object -> 's -> object * 's} ->
    object -> 's -> object  * 's

(* ------------------------------------------------------------------------- *)
(* Generating commands to keep the call stack consistent.                    *)
(* ------------------------------------------------------------------------- *)

val alignCalls :
    {prevCall : object option, call : object option} -> Command.command list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : int -> object Print.pp

end
