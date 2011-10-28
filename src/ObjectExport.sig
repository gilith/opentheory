(* ========================================================================= *)
(* EXPORTED THEOREM OBJECTS                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectExport =
sig

(* ------------------------------------------------------------------------- *)
(* A type of exported theorem objects.                                       *)
(* ------------------------------------------------------------------------- *)

datatype thm =
    Thm of
      {proof : ObjectProv.object,
       hyp : ObjectProv.object,
       concl : ObjectProv.object}

val toThm : thm -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* A type of exported theorem lists.                                         *)
(* ------------------------------------------------------------------------- *)

type export

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val new : {savable : bool} -> export

val savable : export -> bool

val null : export -> bool

val size : export -> int

val add : export -> thm -> export

val foldl : (thm * 's -> 's) -> 's -> export -> 's

val foldr : (thm * 's -> 's) -> 's -> export -> 's

val toList : export -> thm list

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

val eliminateUnwanted : export -> export option

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

val compress : export -> export option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : export Print.pp

end
