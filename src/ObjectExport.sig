(* ========================================================================= *)
(* EXPORT SETS OF THEOREM OBJECTS                                            *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectExport =
sig

(* ------------------------------------------------------------------------- *)
(* A type of export sets of theorem objects.                                 *)
(* ------------------------------------------------------------------------- *)

type export

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : {savable : bool} -> export

val singleton : {savable : bool} -> ObjectThm.thm -> export

val savable : export -> bool

val null : export -> bool

val size : export -> int

val toList : export -> ObjectThm.thm list

val toThms : export -> Thms.thms

(* ------------------------------------------------------------------------- *)
(* Adding theorem objects.                                                   *)
(* ------------------------------------------------------------------------- *)

val add : export -> ObjectThm.thm -> export

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

val union : export -> export -> export

val unionList : export list -> export

(* ------------------------------------------------------------------------- *)
(* Looking up theorem objects.                                               *)
(* ------------------------------------------------------------------------- *)

val peek : export -> Sequent.sequent -> ObjectThm.thm option

val member : Sequent.sequent -> export -> bool

(* ------------------------------------------------------------------------- *)
(* Mapping over export sets of theorem objects                               *)
(* ------------------------------------------------------------------------- *)

val fold : (ObjectThm.thm * 's -> 's) -> 's -> export -> 's

val maps :
    (ObjectThm.thm -> 's -> ObjectThm.thm option * 's) ->
    export -> 's -> export option * 's

(* ------------------------------------------------------------------------- *)
(* Symbols.                                                                  *)
(* ------------------------------------------------------------------------- *)

val thmSymbol : export -> ObjectSymbol.symbol

val proofSymbol : export -> ObjectSymbol.symbol

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

val eliminateUnwanted : export -> export option

(* ------------------------------------------------------------------------- *)
(* Convert to a given article version.                                       *)
(* ------------------------------------------------------------------------- *)

val setVersion : ArticleVersion.version -> export -> export option

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

val compress : export -> export option

(* ------------------------------------------------------------------------- *)
(* Warn about symbol definitions with clashing names.                        *)
(* ------------------------------------------------------------------------- *)

val warnClashingSymbols : export -> unit

(* ------------------------------------------------------------------------- *)
(* Branding theorems.                                                        *)
(* ------------------------------------------------------------------------- *)

val brand : Name.name -> Sequents.sequents -> export

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : export Print.pp

end
