(* ========================================================================= *)
(* PACKAGE DIRECTORY REPOSITORIES                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure DirectoryRepo :> DirectoryRepo =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of repos.                                                          *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name;

datatype repo =
    Repo of
      {system : DirectorySystem.system,
       name : name,
       rootUrl : string,
       checksums : DirectoryChecksums.checksums};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {system = sys, name, rootDirectory = rootDir, rootUrl, upToDate} =
    let
      val checksums =
          let
            val {filename} =
                DirectoryPath.mkRepoFilename {rootDirectory = rootDir} name

            val updateFrom =
                if upToDate then NONE
                else SOME (DirectoryPath.mkInstalledUrl {rootUrl = rootUrl})
          in
            DirectoryChecksums.mk
              {system = sys,
               filename = filename,
               updateFrom = updateFrom}
          end
    in
      Repo
        {system = sys,
         name = name,
         rootUrl = rootUrl,
         checksums = checksums}
    end;

fun system (Repo {system = x, ...}) = x;

fun name (Repo {name = x, ...}) = x;

fun rootUrl (Repo {rootUrl = x, ...}) = {rootUrl = x};

fun checksums (Repo {checksums = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp repo =
    Print.sequence
      (PackageName.pp (name repo))
      (Print.ppString " repo");

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Paths.                                                                    *)
(* ------------------------------------------------------------------------- *)

fun installedUrl repo =
    DirectoryPath.mkInstalledUrl (rootUrl repo);

fun tarballUrl repo n =
    DirectoryPath.mkTarballUrl (rootUrl repo) n;

fun uploadUrl repo =
    DirectoryPath.mkUploadUrl (rootUrl repo);

fun startUploadUrl repo =
    DirectoryPath.mkStartUploadUrl (rootUrl repo);

fun installUploadUrl repo =
    DirectoryPath.mkInstallUploadUrl (rootUrl repo);

fun finishUploadUrl repo =
    DirectoryPath.mkFinishUploadUrl (rootUrl repo);

fun deleteUploadUrl repo =
    DirectoryPath.mkDeleteUploadUrl (rootUrl repo);

fun statusUploadUrl repo token =
    DirectoryPath.mkStatusUploadUrl (rootUrl repo) token;

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
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun previousNameVersion repo nv =
    DirectoryChecksums.previousNameVersion (checksums repo) nv;

fun latestNameVersion repo n =
    DirectoryChecksums.latestNameVersion (checksums repo) n;

fun latestNameVersionList repos name chk' =
    let
      fun matches chk =
          case chk' of
            NONE => true
          | SOME c => Checksum.equal c chk

      fun later nv acc =
          case acc of
            NONE => true
          | SOME (_,nv',_) =>
            let
              val v = PackageNameVersion.version nv
              and v' = PackageNameVersion.version nv'
            in
              case PackageVersion.compare (v',v) of
                LESS => true
              | EQUAL => false
              | GREATER => false
            end

      fun latest (repo,acc) =
          case latestNameVersion repo name of
            NONE => acc
          | SOME (nv,chk) =>
            if not (matches chk andalso later nv acc) then acc
            else SOME (repo,nv,chk)
    in
      List.foldl latest NONE repos
    end;

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

fun update repo =
    DirectoryChecksums.update (checksums repo) (installedUrl repo);

(* ------------------------------------------------------------------------- *)
(* Downloading packages.                                                     *)
(* ------------------------------------------------------------------------- *)

fun download repo info =
    let
      val nv = PackageInfo.nameVersion info

      val chk =
          case peek repo nv of
            SOME c => c
          | NONE =>
            let
              val err =
                  "package " ^ PackageNameVersion.toString nv ^
                  " does not exist on " ^ toString repo
            in
              raise Error err
            end

      (* Download the tarball *)

      val () = PackageInfo.downloadTarball info (tarballUrl repo nv)

      (* Check the checksum *)

      val () =
          let
            val chk' = PackageInfo.checksumTarball info
          in
            if Checksum.equal chk' chk then ()
            else
              let
                val err =
                    "tarball for package " ^
                    PackageNameVersion.toString nv ^
                    " downloaded from " ^ toString repo ^
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

datatype upload =
    Upload of
      {repo : repo,
       token : Checksum.checksum,
       repoName : PackageName.name};

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val repoNameParser =
      (PackageName.parser ++
       exactString " repo: ") >> fst;

  val newUploadParser =
      (exactString "new upload = " ++
       Checksum.parser) >> snd;

  val supportUploadParser =
      (exactString "successfully installed package " ++
       PackageNameVersion.parser) >> snd;

  val packageUploadParser =
      (exactString "successfully uploaded package " ++
       PackageNameVersion.parser) >> snd;

  val finishUploadParser =
      exactString "successfully finished package upload";

  val deleteUploadParser =
      exactString "successfully deleted package upload";
in
  val parserStartUpload =
      (repoNameParser ++
       newUploadParser);

  val parserSupportUpload =
      (repoNameParser ++
       supportUploadParser);

  val parserPackageUpload =
      (repoNameParser ++
       packageUploadParser);

  val parserFinishUpload =
      (repoNameParser ++
       finishUploadParser) >> fst;

  val parserDeleteUpload =
      (repoNameParser ++
       deleteUploadParser) >> fst;
end;

fun fromStringStartUpload s =
    Parse.fromString parserStartUpload s
    handle Parse.NoParse => raise Error "fromStringStartUpload";

fun fromStringSupportUpload s =
    Parse.fromString parserSupportUpload s
    handle Parse.NoParse => raise Error "fromStringSupportUpload";

fun fromStringPackageUpload s =
    Parse.fromString parserPackageUpload s
    handle Parse.NoParse => raise Error "fromStringPackageUpload";

fun fromStringFinishUpload s =
    Parse.fromString parserFinishUpload s
    handle Parse.NoParse => raise Error "fromStringFinishUpload";

fun fromStringDeleteUpload s =
    Parse.fromString parserDeleteUpload s
    handle Parse.NoParse => raise Error "fromStringDeleteUpload";

fun startUpload repo =
    let
      val sys = system repo

      val {url} = startUploadUrl repo

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = DirectorySystem.curl sys

      val cmd =
          cmd ^ " " ^ url ^
          " --output " ^ tmpFile

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "starting the upload failed"

      val lines = Stream.toList (Stream.fromTextFile {filename = tmpFile})

      val () = OS.FileSys.remove tmpFile
    in
      if List.null lines then raise Error "no response from repo"
      else
        let
          (* Check the repo response *)

          val response = chomp (String.concat lines)
        in
          case total fromStringStartUpload response of
            NONE => raise Error ("error response from repo:\n" ^ response)
          | SOME (repoName,token) =>
            Upload {repo = repo, token = token, repoName = repoName}
        end
    end;

fun supportUpload upl namever chk =
    let
      val Upload {repo,token,repoName} = upl

      val sys = system repo

      (* Send the install request *)

      val {url} = installUploadUrl repo

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = DirectorySystem.curl sys

      val cmd =
          cmd ^ " " ^ url ^
          " --form \"u=" ^ Checksum.toString token ^ "\"" ^
          " --form \"p=" ^ PackageNameVersion.toString namever ^ "\"" ^
          " --form \"c=" ^ Checksum.toString chk ^ "\"" ^
          " --form \"x=install support package\"" ^
          " --output " ^ tmpFile

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "upload install request failed"

      val lines = Stream.toList (Stream.fromTextFile {filename = tmpFile})

      val () = OS.FileSys.remove tmpFile
    in
      if List.null lines then raise Error "no response from repo"
      else
        let
          (* Check the repo response *)

          val response = chomp (String.concat lines)
        in
          case total fromStringSupportUpload response of
            NONE => raise Error ("error response from repo:\n" ^ response)
          | SOME (repoName',namever') =>
            if not (PackageName.equal repoName' repoName) then
              let
                val err =
                    "repo name " ^ PackageName.toString repoName ^
                    " changed since start of upload:\n" ^ response
              in
                raise Error err
              end
            else if not (PackageNameVersion.equal namever' namever) then
              let
                val err =
                    "uploaded package " ^ PackageNameVersion.toString namever ^
                    " has a different name:\n" ^ response
              in
                raise Error err
              end
            else
              ()
        end
    end;

fun packageUpload upl info chk =
    let
      val Upload {repo,token,repoName} = upl

      val {url} = uploadUrl repo

      val token = Checksum.toString token

      (* Upload the tarball *)

      val {response} =
          PackageInfo.uploadTarball info chk {url = url, token = token}

      (* Check the repo response *)

      val namever = PackageInfo.nameVersion info
    in
      case total fromStringPackageUpload response of
        NONE => raise Error ("error response from repo:\n" ^ response)
      | SOME (repoName',namever') =>
        if not (PackageName.equal repoName' repoName) then
          let
            val err =
                "repo name " ^ PackageName.toString repoName ^
                " changed since start of upload:\n" ^ response
          in
            raise Error err
          end
        else if not (PackageNameVersion.equal namever' namever) then
          let
            val err =
                "uploaded package " ^ PackageNameVersion.toString namever ^
                " has a different name:\n" ^ response
          in
            raise Error err
          end
        else
          ()
    end;

fun finishUpload upl =
    let
      val Upload {repo,token,repoName} = upl

      val sys = system repo

      (* Send the finish request *)

      val {url} = finishUploadUrl repo

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = DirectorySystem.curl sys

      val cmd =
          cmd ^ " " ^ url ^
          " --form \"u=" ^ Checksum.toString token ^ "\"" ^
          " --output " ^ tmpFile

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "upload finish request failed"

      val lines = Stream.toList (Stream.fromTextFile {filename = tmpFile})

      val () = OS.FileSys.remove tmpFile
    in
      if List.null lines then raise Error "no response from repo"
      else
        let
          (* Check the repo response *)

          val response = chomp (String.concat lines)
        in
          case total fromStringFinishUpload response of
            NONE => raise Error ("error response from repo:\n" ^ response)
          | SOME repoName' =>
            if not (PackageName.equal repoName' repoName) then
              let
                val err =
                    "repo name " ^ PackageName.toString repoName ^
                    " changed since start of upload:\n" ^ response
              in
                raise Error err
              end
            else
              ()
        end
    end;

fun deleteUpload upl =
    let
      val Upload {repo,token,repoName} = upl

      val sys = system repo

      (* Send the delete request *)

      val {url} = deleteUploadUrl repo

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = DirectorySystem.curl sys

      val cmd =
          cmd ^ " " ^ url ^
          " --form \"u=" ^ Checksum.toString token ^ "\"" ^
          " --output " ^ tmpFile

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "upload delete request failed"

      val lines = Stream.toList (Stream.fromTextFile {filename = tmpFile})

      val () = OS.FileSys.remove tmpFile
    in
      if List.null lines then raise Error "no response from repo"
      else
        let
          (* Check the repo response *)

          val response = chomp (String.concat lines)
        in
          case total fromStringDeleteUpload response of
            NONE => raise Error ("error response from repo:\n" ^ response)
          | SOME repoName' =>
            if not (PackageName.equal repoName' repoName) then
              let
                val err =
                    "repo name " ^ PackageName.toString repoName ^
                    " changed since start of upload:\n" ^ response
              in
                raise Error err
              end
            else
              ()
        end
    end;

fun urlUpload upl =
    let
      val Upload {repo,token,...} = upl
    in
      statusUploadUrl repo token
    end;

end
