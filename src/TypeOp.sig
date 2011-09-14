(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPE OPERATORS                                         *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature TypeOp =
sig

(* ------------------------------------------------------------------------- *)
(* A type of type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

type typeOp = TypeTerm.opTy

type typeOpData =
     {name : Name.name,
      prov : TypeTerm.provOpTy}

val mk : typeOpData -> typeOp

val dest : typeOp -> typeOpData

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name : typeOp -> Name.name

val prov : typeOp -> TypeTerm.provOpTy

val mkUndef : Name.name -> typeOp

val isUndef : typeOp -> bool

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : typeOp * typeOp -> order

val equal : typeOp -> typeOp -> bool

val checkEqual :
    (TypeTerm.term -> TypeTerm.term -> unit) -> typeOp -> typeOp -> unit

(* ------------------------------------------------------------------------- *)
(* Reconstructing the arity from the provenance.                             *)
(* ------------------------------------------------------------------------- *)

val varsDef : typeOp -> Name.name list option

val arityDef : typeOp -> int option

(* ------------------------------------------------------------------------- *)
(* Primitive type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val bool : typeOp

val isBool : typeOp -> bool

(* Function spaces *)

val func : typeOp

val isFun : typeOp -> bool

(* Individuals *)

val ind : typeOp

val isInd : typeOp -> bool

(* ------------------------------------------------------------------------- *)
(* Special syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

val isList : typeOp -> bool

val isPair : typeOp -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppWithShow : Show.show -> typeOp Print.pp

val pp : typeOp Print.pp

val toString : typeOp -> string

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val toHtml : Show.show -> typeOp * Name.name list option -> Html.inline list

end
