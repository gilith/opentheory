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
       rootUrl : string,
       checksums : DirectoryChecksums.checksums};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {name, rootDirectory = rootDir, rootUrl} =
    let
      val checksums =
          DirectoryChecksums.mk
            (DirectoryPath.mkRepoFilename {rootDirectory = rootDir} name)
    in
      Repo
        {name = name,
         rootUrl = rootUrl,
         checksums = checksums}
    end;

fun name (Repo {name = x, ...}) = x;

fun rootUrl (Repo {rootUrl = x, ...}) = {rootUrl = x};

fun checksums (Repo {checksums = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

fun peek repo n = DirectoryChecksums.peek (checksums repo) n;

fun member n repo = DirectoryChecksums.member n (checksums repo);

fun first repos n =
    let
      fun pk r =
          case peek r n of
            SOME c => SOME (r,c)
          | NONE => NONE
    in
      Useful.first pk repos
    end;

fun find repos (n,c) =
    let
      fun pred r =
          case peek r n of
            SOME c' => Checksum.equal c' c
          | NONE => false
    in
      List.find pred repos
    end;

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

fun update repo =
    let
      val url = DirectoryPath.mkInstalledUrl (rootUrl repo)
    in
      DirectoryChecksums.update (checksums repo) url
    end;

(* ------------------------------------------------------------------------- *)
(* Downloading packages.                                                     *)
(* ------------------------------------------------------------------------- *)

fun download repo n info =
    let
      val chk =
          case peek repo n of
            SOME c => c
          | NONE =>
            let
              val err =
                  "package " ^ PackageName.toString n ^
                  " does not exist on " ^ name repo ^ " repo"
            in
              raise Error err
            end

      (* Download the tarball *)

      val url = DirectoryPath.mkTarballUrl (rootUrl repo) n

      val () = PackageInfo.downloadTarball info url

      (* Create the checksum *)

      val () = PackageInfo.createChecksum info

      (* Check the checksum *)

      val () =
          let
            val chk' = PackageInfo.readChecksum info
          in
            if Checksum.equal chk' chk then ()
            else
              let
                val err =
                    "tarball for package " ^
                    PackageName.toString n ^
                    " downloaded from " ^ name repo ^
                    " has the wrong checksum"
              in
                raise Error err
              end
          end
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap name Print.ppString;

val toString = Print.toString pp;

end
