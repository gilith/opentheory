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
(* Paths.                                                                    *)
(* ------------------------------------------------------------------------- *)

fun installedUrl repo =
    DirectoryPath.mkInstalledUrl (rootUrl repo);

fun tarballUrl repo n =
    DirectoryPath.mkTarballUrl (rootUrl repo) n;

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

fun update sys repo =
    DirectoryChecksums.update sys (checksums repo) (installedUrl repo);

(* ------------------------------------------------------------------------- *)
(* Downloading packages.                                                     *)
(* ------------------------------------------------------------------------- *)

fun download sys repo info =
    let
      val n = PackageInfo.name info

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

      val () = PackageInfo.downloadTarball sys info (tarballUrl repo n)

      (* Check the checksum *)

      val () =
          let
            val chk' = PackageInfo.checksumTarball sys info
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
(* Uploading packages.                                                       *)
(* ------------------------------------------------------------------------- *)

fun upload repo info = raise Bug "not implemented";

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap name Print.ppString;

val toString = Print.toString pp;

end
