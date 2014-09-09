(* ========================================================================= *)
(* UPLOADING PACKAGES TO A REMOTE REPOSITORY                                 *)
(* Copyright (c) 2014 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature RepositoryUpload =
sig

(* ------------------------------------------------------------------------- *)
(* A type of repository uploads.                                             *)
(* ------------------------------------------------------------------------- *)

type upload

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk :
    {local : Repository.repository,
     remote : RepositoryRemote.remote,
     support : PackageNameVersion.nameVersion list,
     packages : PackageNameVersion.nameVersion list} -> upload

(* ------------------------------------------------------------------------- *)
(* Perform checks on an upload.                                              *)
(* ------------------------------------------------------------------------- *)

val check : upload -> RepositoryError.error list

(* ------------------------------------------------------------------------- *)
(* Execute the upload.                                                       *)
(* ------------------------------------------------------------------------- *)

val upload : upload -> unit

(* ------------------------------------------------------------------------- *)
(* Summarize the upload.                                                     *)
(* ------------------------------------------------------------------------- *)

val pp : upload Print.pp

end
