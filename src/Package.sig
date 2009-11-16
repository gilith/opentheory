(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE DATA                                    *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Package =
sig

(* ------------------------------------------------------------------------- *)
(* Types of theory package data.                                             *)
(* ------------------------------------------------------------------------- *)

datatype package =
    Package of
      {name : PackageName.name option,
       directory : string,
       contents : PackageContents.contents}

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name : package -> PackageName.name option

val directory : package -> {directory : string}

val contents : package -> PackageContents.contents

val tags : package -> Tag.tag list

val requires : package -> PackageRequire.require list

val theory : package -> PackageTheory.theory

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile :
    {name : PackageName.name option,
     filename : string} -> package

end
