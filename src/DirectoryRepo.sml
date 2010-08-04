(* ========================================================================= *)
(* PACKAGE DIRECTORY REPOSITORIES                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryRepo :> DirectoryRepo =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of repos.                                                          *)
(* ------------------------------------------------------------------------- *)

type name = string;

datatype repo =
    Repo of
      {name : string,
       url : string,
       checksums : DirectoryChecksums.checksums};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {name,url,filename} =
    let
      val checksums = DirectoryChecksums.mk {filename = filename}
    in
      Repo
        {name = name,
         url = url,
         checksums = checksums}
    end;

fun name (Repo {name = x, ...}) = x;

fun url (Repo {url = x, ...}) = {url = x};

fun checksums (Repo {checksums = x, ...}) = x;

fun filename repo = DirectoryChecksums.filename (checksums repo);

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

fun peek repo n = DirectoryChecksums.peek (checksums repo) n;

fun member n repo = DirectoryChecksums.member n (checksums repo);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap name Print.ppString;

val toString = Print.toString pp;

end
