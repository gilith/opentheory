(* ========================================================================= *)
(* EXPORT SETS OF THEOREM OBJECTS                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
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

val new : {savable : bool} -> export

val savable : export -> bool

val null : export -> bool

val size : export -> int

val add : export -> ObjectThm.thm -> export

val foldl : (ObjectThm.thm * 's -> 's) -> 's -> export -> 's

val foldr : (ObjectThm.thm * 's -> 's) -> 's -> export -> 's

val toSet : export -> ObjectThmSet.set

val toList : export -> ObjectThm.thm list

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

val union : export -> export -> export

val unionList : export list -> export

(* ------------------------------------------------------------------------- *)
(* Mapping over export sets of theorem objects                               *)
(* ------------------------------------------------------------------------- *)

val maps :
    (ObjectThm.thm -> 's -> ObjectThm.thm option * 's) ->
    export -> 's -> export option * 's

(* ------------------------------------------------------------------------- *)
(* Symbols.                                                                  *)
(* ------------------------------------------------------------------------- *)

val symbol : export -> ObjectSymbol.symbol

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

val eliminateUnwanted : export -> export option

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

val compress : export -> export option

(* ------------------------------------------------------------------------- *)
(* Imprinting theorems.                                                      *)
(* ------------------------------------------------------------------------- *)

val imprint : Name.name -> Thms.thms -> export

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : export Print.pp

end
