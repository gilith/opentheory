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

(* ------------------------------------------------------------------------- *)
(* Reconstructing the arity from the provenance.                             *)
(* ------------------------------------------------------------------------- *)

val varsDef : typeOp -> Name.name list option

val arityDef : typeOp -> int option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppWithShow : Show.show -> typeOp Print.pp

val pp : typeOp Print.pp

val toString : typeOp -> string

val toHtml : (typeOp * Name.name list option) * Name.name -> Html.inline

end
