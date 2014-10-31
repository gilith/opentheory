(* ========================================================================= *)
(* OPENTHEORY OBJECTS THAT TRACK THEIR PROVENANCE                            *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Object =
sig

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory objects that track their provenance.                 *)
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
       definitions : object list,
       generated : ObjectData.data list,
       result : int}

val isDefaultProvenance : provenance -> bool

val argumentsProvenance : provenance -> object list

val definitionsProvenance : provenance -> object list

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype object' =
    Object' of
      {data : ObjectData.data,
       provenance : provenance}

val dest : object -> object'

val data : object -> ObjectData.data

val equalData : ObjectData.data -> object -> bool

val provenance : object -> provenance

val isDefault : object -> bool

val arguments : object -> object list

val definitions : object -> object list

(* List objects *)

val destList : object -> ObjectData.data list

val isList : object -> bool

(* Num objects *)

val destNum : object -> int

val isNum : object -> bool

(* Name objects *)

val destName : object -> Name.name

val isName : object -> bool

(* Name list objects *)

val destNames : object -> Name.name list

val isNames : object -> bool

(* Type operator objects *)

val destTypeOp : object -> TypeOp.typeOp

val isTypeOp : object -> bool

val equalTypeOp : TypeOp.typeOp -> object -> bool

(* Type objects *)

val destType : object -> Type.ty

val isType : object -> bool

(* Type list objects *)

val destTypes : object -> Type.ty list

val isTypes : object -> bool

(* Constant objects *)

val destConst : object -> Const.const

val isConst : object -> bool

val equalConst : Const.const -> object -> bool

(* Term variable objects *)

val destVar : object -> Var.var

val isVar : object -> bool

(* Term objects *)

val destTerm : object -> Term.term

val isTerm : object -> bool

(* Sequent objects *)

val destSequent : object * object -> Sequent.sequent

val isSequent : object * object -> bool

(* Theorem objects *)

val destThm : object -> Thm.thm

val isThm : object -> bool

(* Substitution objects *)

val destSubst : object -> TypeSubst.substMap * TermSubst.substMap

val isSubst : object -> bool

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

val mkDefineTypeOpLegacy :
    {savable : bool} ->
    Name.name -> Name.name -> Name.name -> object -> object ->
    object * object * object * object * object

val mkEqMp : {savable : bool} -> object -> object -> object

val mkNil : object

val mkOpType : {savable : bool} -> object -> object -> object

val mkProveHyp : {savable : bool} -> object -> object -> object

val mkRefl : {savable : bool} -> object -> object

val mkSubst : {savable : bool} -> object -> object -> object

val mkSym : {savable : bool} -> object -> object

val mkTrans : {savable : bool} -> object -> object -> object

val mkTypeOp : Name.name -> object

val mkVar : {savable : bool} -> object -> object -> object

val mkVarTerm : {savable : bool} -> object -> object

val mkVarType : object -> object

(* General commands *)

val mkCommand :
    {savable : bool} -> Command.command -> object list -> object list

(* Derived commands *)

val mkList : {savable : bool} -> object list -> object

(* ------------------------------------------------------------------------- *)
(* Reconstructing the command and arguments used to make an object.          *)
(* ------------------------------------------------------------------------- *)

val unMkAbsTerm : object -> object * object

val unMkAppTerm : object -> object * object

val unMkAxiom : object -> object * object

val unMkVar : object -> object * object

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
(* Bottom-up mapping of objects: return NONE for unchanged.                  *)
(* ------------------------------------------------------------------------- *)

type mapping

val newMapping : (object -> object option) -> mapping

val sharedMap : object -> mapping -> object option * mapping

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

(* ------------------------------------------------------------------------- *)
(* Constructing unsavable objects.                                           *)
(* ------------------------------------------------------------------------- *)

val mkUnsavable : ObjectData.data -> object

end
