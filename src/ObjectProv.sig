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
       generated : Object.object list,
       result : int}

val isDefaultProvenance : provenance -> bool

val argumentsProvenance : provenance -> object list

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype object' =
    Object' of
      {object : Object.object,
       provenance : provenance}

(***
val mk : object' -> object
***)

val dest : object -> object'

val object : object -> Object.object

val provenance : object -> provenance

val isDefault : object -> bool

val parents : object -> object list

(* Num objects *)

val destNum : object -> int

(* Name objects *)

val destName : object -> Name.name

(* Type operator objects *)

val destTypeOp : object -> TypeOp.typeOp

val equalTypeOp : TypeOp.typeOp -> object -> bool

(* Constant objects *)

val destConst : object -> Const.const

val equalConst : Const.const -> object -> bool

(* Sequent objects *)

val destSequent : object * object -> Sequent.sequent

(* Theorem objects *)

val destThm : object -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Constructing objects from commands.                                       *)
(* ------------------------------------------------------------------------- *)

(* Special commands *)

val mkNum : int -> object

val mkName : Name.name -> object

(* Regular commands *)

val mkAbsTerm : {savable : bool} -> object -> object -> object

val mkAbsThm : {savable : bool} -> object -> object -> object

val mkAppTerm : {savable : bool} -> object -> object -> object

val mkAppThm : {savable : bool} -> object -> object -> object

val mkAssume : {savable : bool} -> object -> object

val mkAxiom : {savable : bool} -> object -> object -> Sequent.sequent -> object

val mkBetaConv : {savable : bool} -> object -> object

val mkCons : {savable : bool} -> object -> object -> object

val mkConst : Name.name -> object

val mkConstTerm : {savable : bool} -> object -> object -> object

val mkDeductAntisym : {savable : bool} -> object -> object -> object

val mkDefineConst : {savable : bool} -> Name.name -> object -> object * object

val mkDefineTypeOp :
    {savable : bool} ->
    Name.name -> Name.name -> Name.name -> object -> object ->
    object * object * object * object * object

val mkEqMp : {savable : bool} -> object -> object -> object

val mkNil : object

val mkOpType : {savable : bool} -> object -> object -> object

val mkRefl : {savable : bool} -> object -> object

val mkSubst : {savable : bool} -> object -> object -> object

val mkTypeOp : Name.name -> object

val mkVar : {savable : bool} -> object -> object -> object

val mkVarTerm : {savable : bool} -> object -> object

val mkVarType : object -> object

(* General commands *)

val mkCommand :
    {savable : bool} -> Command.command -> object list -> object list

(* ------------------------------------------------------------------------- *)
(* Folding over objects.                                                     *)
(* ------------------------------------------------------------------------- *)

val foldl :
    {preDescent : object -> 's -> {descend : bool, result : 's},
     postDescent : object -> 's -> 's} ->
    's -> object -> 's

(* ------------------------------------------------------------------------- *)
(* Mapping with state over objects: return NONE for unchanged.               *)
(* ------------------------------------------------------------------------- *)

val maps :
    {preDescent : object -> 's -> {descend : bool, result : object option * 's},
     postDescent : object -> object option -> 's -> object option * 's,
     savable : bool} ->
    object -> 's -> object option * 's

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : object Print.pp

val ppProvenance : provenance Print.pp

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

type id = int

val id : object -> id

val equalId : id -> object -> bool

val compare : object * object -> order

end
