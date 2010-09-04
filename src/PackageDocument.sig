(* ========================================================================= *)
(* PACKAGE DOCUMENTS                                                         *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageDocument =
sig

(* ------------------------------------------------------------------------- *)
(* Document filenames.                                                       *)
(* ------------------------------------------------------------------------- *)

val mkFilename : PackageName.name -> {filename : string}

val destFilename : {filename : string} -> PackageName.name option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* A type of package documents.                                              *)
(* ------------------------------------------------------------------------- *)

type document

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype document' =
    Document' of
      {name : PackageName.name,
       package : Package.package,
       summary : Summary.summary,
       files : {theory : string, tarball : string}}

val mk : document' -> document

val dest : document -> document'

(* ------------------------------------------------------------------------- *)
(* Output formats.                                                           *)
(* ------------------------------------------------------------------------- *)

val toHtml : document -> Html.html

val toHtmlFile : {document : document, filename : string} -> unit

end
