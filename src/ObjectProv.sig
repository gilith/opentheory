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
(* A type of provenances.                                                    *)
(* ------------------------------------------------------------------------- *)

datatype provenance =
    Default
  | Special of
      {command : Command.command,
       arguments : object list,
       result : int}

val isDefaultProvenance : provenance -> bool

val parentsProvenance : provenance -> object list

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype object' =
    Object' of
      {object : Object.object,
       provenance : provenance}

val mk : object' -> object

val dest : object -> object'

val object : object -> Object.object

val provenance : object -> provenance

val isDefault : object -> bool

val parents : object -> object list

(* ------------------------------------------------------------------------- *)
(* Constructing objects from commands.                                       *)
(* ------------------------------------------------------------------------- *)

(* Special commands *)

val mkNum : int -> object

val mkName : Name.name -> object

(* Regular commands *)

val mkAbs : {savable : bool} -> object -> object -> object

val mkAbsTerm : {savable : bool} -> object -> object -> object

val mkApp : {savable : bool} -> object -> object -> object

val mkAppTerm : {savable : bool} -> object -> object -> object

val mkAssume : {savable : bool} -> object -> object

val mkAxiom : {savable : bool} -> object -> object -> Sequent.sequent -> object

val mkBetaConv : {savable : bool} -> object -> object

val mkCons : {savable : bool} -> object -> object -> object

val mkConst : Const.const -> object

val mkConstTerm : {savable : bool} -> object -> object -> object

val mkDeductAntisym : {savable : bool} -> object -> object -> object

val mkDefineConst : {savable : bool} -> object -> object -> object * object

val mkDefineTypeOp :
    {savable : bool} -> object -> object -> object -> object -> object ->
    object * object * object * object * object

val mkEqMp : {savable : bool} -> object -> object -> object

val mkNil : object

val mkOpType : {savable : bool} -> object -> object -> object

val mkRefl : {savable : bool} -> object -> object

val mkSubst : {savable : bool} -> object -> object -> object

val mkTypeOp : TypeOp.typeOp -> object

val mkVar : {savable : bool} -> object -> object -> object

val mkVarTerm : {savable : bool} -> object -> object

val mkVarType : object -> object

(***
val mkThm : {savable : bool} -> Thm.thm -> inference -> object

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
***)

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

type id = int

val id : object -> id

val equalId : id -> object -> bool

val compare : object * object -> order

end
