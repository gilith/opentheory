(* ========================================================================= *)
(* PACKAGE AUTHORS                                                           *)
(* Copyright (c) 2012 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageAuthor =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package authors.                                         *)
(* ------------------------------------------------------------------------- *)

type author

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype author' =
    Author' of
      {name : string,
       email : string}

val mk : author' -> author

val dest : author -> author'

val name : author -> {name : string}

val email : author -> {email : string}

val equalName : {name : string} -> author -> bool

val equalEmail : {email : string} -> author -> bool

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : author * author -> order

val equal : author -> author -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : author Print.pp

val toString : author -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,author) Parse.parser

val fromString : string -> author

end
