(* ========================================================================= *)
(* PACKAGE TARBALLS                                                          *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageTarball =
sig

(* ------------------------------------------------------------------------- *)
(* Tarball filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

val mkFilename : {base : string} -> {filename : string}

val destFilename : {filename : string} -> {base : string} option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* A type of package tarball.                                                *)
(* ------------------------------------------------------------------------- *)

type tarball

val mk :
    {system : RepositorySystem.system,
     filename : string,
     checksum : Checksum.checksum option} -> tarball

val filename : tarball -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* Listing the contents.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype contents =
    Contents of
      {nameVersion : PackageNameVersion.nameVersion,
       theoryFile : {filename : string},
       otherFiles : {filename : string} list}

val contents : tarball -> contents

val nameVersion : tarball -> PackageNameVersion.nameVersion

val theoryFile : tarball -> {filename : string}

val otherFiles : tarball -> {filename : string} list

val allFiles : tarball -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Creating a checksum.                                                      *)
(* ------------------------------------------------------------------------- *)

val checksum : tarball -> Checksum.checksum

(* ------------------------------------------------------------------------- *)
(* Packing a tarball.                                                        *)
(* ------------------------------------------------------------------------- *)

val pack : tarball -> {filename : string} list -> unit

(* ------------------------------------------------------------------------- *)
(* Copying a tarball.                                                        *)
(* ------------------------------------------------------------------------- *)

val copy : {src : tarball} -> {dest : tarball} -> unit

(* ------------------------------------------------------------------------- *)
(* Downloading a tarball.                                                    *)
(* ------------------------------------------------------------------------- *)

val download : tarball -> {url : string} -> unit

(* ------------------------------------------------------------------------- *)
(* Extracting files from a tarball.                                          *)
(* ------------------------------------------------------------------------- *)

val extract : tarball -> {filename : string} list -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading a tarball.                                                      *)
(* ------------------------------------------------------------------------- *)

val upload : tarball -> {url : string, token : string} -> {response : string}

end
