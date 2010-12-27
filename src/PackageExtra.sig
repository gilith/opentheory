(* ========================================================================= *)
(* EXTRA PACKAGE FILES                                                       *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageExtra =
sig

(* ------------------------------------------------------------------------- *)
(* A type of extra package files.                                            *)
(* ------------------------------------------------------------------------- *)

type extra

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype extra' =
    Extra of
      {name : string,
       filename : string}

val mk : extra' -> extra

val dest : extra -> extra'

val name : extra -> string

val filename : extra -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* Remove the directory from the filename path.                              *)
(* ------------------------------------------------------------------------- *)

val normalize : extra -> extra

end
