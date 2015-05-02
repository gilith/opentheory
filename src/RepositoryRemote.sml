(* ========================================================================= *)
(* REMOTE REPOSITORIES                                                       *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure RepositoryRemote :> RepositoryRemote =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of remote repositories.                                            *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name;

datatype remote =
    Remote of
      {system : RepositorySystem.system,
       name : name,
       rootUrl : string,
       checksums : RepositoryChecksums.checksums};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {system = sys, name, rootDirectory = rootDir, rootUrl, upToDate} =
    let
      val checksums =
          let
            val {filename} =
                RepositoryPath.mkRemoteRepositoryChecksumsFilename
                  {rootDirectory = rootDir} name

            val updateFrom =
                if upToDate then NONE
                else SOME (RepositoryPath.mkInstalledUrl {rootUrl = rootUrl})
          in
            RepositoryChecksums.mk
              {system = sys,
               filename = filename,
               updateFrom = updateFrom}
          end
    in
      Remote
        {system = sys,
         name = name,
         rootUrl = rootUrl,
         checksums = checksums}
    end;

fun system (Remote {system = x, ...}) = x;

fun name (Remote {name = x, ...}) = x;

fun rootUrl (Remote {rootUrl = x, ...}) = {rootUrl = x};

fun checksums (Remote {checksums = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp remote =
    Print.sequence
      (PackageName.pp (name remote))
      (Print.ppString " repo");

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Paths.                                                                    *)
(* ------------------------------------------------------------------------- *)

fun installedUrl remote =
    RepositoryPath.mkInstalledUrl (rootUrl remote);

fun tarballUrl remote nv =
    RepositoryPath.mkTarballUrl (rootUrl remote) nv;

fun uploadUrl remote =
    RepositoryPath.mkUploadUrl (rootUrl remote);

fun startUploadUrl remote =
    RepositoryPath.mkStartUploadUrl (rootUrl remote);

fun installUploadUrl remote =
    RepositoryPath.mkInstallUploadUrl (rootUrl remote);

fun finishUploadUrl remote =
    RepositoryPath.mkFinishUploadUrl (rootUrl remote);

fun deleteUploadUrl remote =
    RepositoryPath.mkDeleteUploadUrl (rootUrl remote);

fun uploadStatusUrl remote token =
    RepositoryPath.mkUploadStatusUrl (rootUrl remote) token;

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

fun peek remote n = RepositoryChecksums.peek (checksums remote) n;

fun member n remote = RepositoryChecksums.member n (checksums remote);

fun first remotes n =
    let
      fun pk r =
          case peek r n of
            SOME c => SOME (r,c)
          | NONE => NONE
    in
      Useful.first pk remotes
    end;

fun find remotes (n,c) =
    let
      fun pred r =
          case peek r n of
            SOME c' => Checksum.equal c' c
          | NONE => false
    in
      List.find pred remotes
    end;

(* ------------------------------------------------------------------------- *)
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun previousNameVersion remote nv =
    RepositoryChecksums.previousNameVersion (checksums remote) nv;

local
  fun previous nv remote =
      case previousNameVersion remote nv of
        NONE => NONE
      | SOME (nv,chk) => SOME (remote,nv,chk);

  fun latest (p2,p1) =
      let
        val (_,nv1,_) = p1
        and (_,nv2,_) = p2
      in
        case PackageNameVersion.compareVersion (nv1,nv2) of
          LESS => p2
        | _ => p1
      end;
in
  fun previousNameVersionList remotes nv =
      case List.mapPartial (previous nv) remotes of
        [] => NONE
      | h :: t => SOME (List.foldl latest h t);
end;

fun latestNameVersion remote n =
    RepositoryChecksums.latestNameVersion (checksums remote) n;

fun latestNameVersionList remotes name chk' =
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

      fun latest (remote,acc) =
          case latestNameVersion remote name of
            NONE => acc
          | SOME (nv,chk) =>
            if not (matches chk andalso later nv acc) then acc
            else SOME (remote,nv,chk)
    in
      List.foldl latest NONE remotes
    end;

fun earlierThanLatestNameVersion remote namever =
    let
      val PackageNameVersion.NameVersion' {name,version} =
          PackageNameVersion.dest namever
    in
      case latestNameVersion remote name of
        NONE => false
      | SOME (nv,_) =>
        case PackageVersion.compare (version, PackageNameVersion.version nv) of
          LESS => true
        | EQUAL => false
        | GREATER => false
    end;

fun laterThanLatestNameVersion remote namever =
    let
      val PackageNameVersion.NameVersion' {name,version} =
          PackageNameVersion.dest namever
    in
      case latestNameVersion remote name of
        NONE => false
      | SOME (nv,_) =>
        case PackageVersion.compare (version, PackageNameVersion.version nv) of
          LESS => false
        | EQUAL => false
        | GREATER => true
    end;

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

fun update remote =
    RepositoryChecksums.update (checksums remote) (installedUrl remote);

(* ------------------------------------------------------------------------- *)
(* Downloading packages.                                                     *)
(* ------------------------------------------------------------------------- *)

fun download remote pkg =
    let
      val nv = Package.nameVersion pkg

      val chk =
          case peek remote nv of
            SOME c => c
          | NONE =>
            let
              val err =
                  "package " ^ PackageNameVersion.toString nv ^
                  " does not exist on " ^ toString remote
            in
              raise Error err
            end

      (* Download the tarball *)

      val () = Package.downloadTarball pkg (tarballUrl remote nv)

      (* Check the checksum *)

      val () =
          let
            val chk' = Package.checksum pkg
          in
            if Checksum.equal chk' chk then ()
            else
              let
                val err =
                    "tarball for package " ^
                    PackageNameVersion.toString nv ^
                    " downloaded from " ^ toString remote ^
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
      {remote : remote,
       token : Checksum.checksum,
       remoteName : name};

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val remoteNameParser =
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
      (remoteNameParser ++
       newUploadParser);

  val parserSupportUpload =
      (remoteNameParser ++
       supportUploadParser);

  val parserPackageUpload =
      (remoteNameParser ++
       packageUploadParser);

  val parserFinishUpload =
      (remoteNameParser ++
       finishUploadParser) >> fst;

  val parserDeleteUpload =
      (remoteNameParser ++
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

fun startUpload remote =
    let
      val sys = system remote

      val {url} = startUploadUrl remote

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = RepositorySystem.curl sys

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
          (* Check the remote repository response *)

          val response = chomp (String.concat lines)
        in
          case total fromStringStartUpload response of
            NONE => raise Error ("error response from repo:\n" ^ response)
          | SOME (name,token) =>
            Upload {remote = remote, token = token, remoteName = name}
        end
    end;

fun supportUpload upl namever chk =
    let
      val Upload {remote,token,remoteName} = upl

      val sys = system remote

      (* Send the install request *)

      val {url} = installUploadUrl remote

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = RepositorySystem.curl sys

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
          (* Check the remote repository response *)

          val response = chomp (String.concat lines)
        in
          case total fromStringSupportUpload response of
            NONE => raise Error ("error response from repo:\n" ^ response)
          | SOME (remoteName',namever') =>
            if not (PackageName.equal remoteName' remoteName) then
              let
                val err =
                    "repo name " ^ PackageName.toString remoteName ^
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

fun packageUpload upl pkg =
    let
      val Upload {remote,token,remoteName} = upl

      val {url} = uploadUrl remote

      val token = Checksum.toString token

      (* Upload the tarball *)

      val {response} = Package.upload pkg {url = url, token = token}

      (* Check the remote repository response *)

      val namever = Package.nameVersion pkg
    in
      case total fromStringPackageUpload response of
        NONE => raise Error ("error response from repo:\n" ^ response)
      | SOME (remoteName',namever') =>
        if not (PackageName.equal remoteName' remoteName) then
          let
            val err =
                "repo name " ^ PackageName.toString remoteName ^
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
      val Upload {remote,token,remoteName} = upl

      val sys = system remote

      (* Send the finish request *)

      val {url} = finishUploadUrl remote

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = RepositorySystem.curl sys

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
          (* Check the remote repository response *)

          val response = chomp (String.concat lines)
        in
          case total fromStringFinishUpload response of
            NONE => raise Error ("error response from repo:\n" ^ response)
          | SOME remoteName' =>
            if not (PackageName.equal remoteName' remoteName) then
              let
                val err =
                    "repo name " ^ PackageName.toString remoteName ^
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
      val Upload {remote,token,remoteName} = upl

      val sys = system remote

      (* Send the delete request *)

      val {url} = deleteUploadUrl remote

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = RepositorySystem.curl sys

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
          (* Check the remote repository response *)

          val response = chomp (String.concat lines)
        in
          case total fromStringDeleteUpload response of
            NONE => raise Error ("error response from repo:\n" ^ response)
          | SOME remoteName' =>
            if not (PackageName.equal remoteName' remoteName) then
              let
                val err =
                    "repo name " ^ PackageName.toString remoteName ^
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
      val Upload {remote,token,...} = upl
    in
      uploadStatusUrl remote token
    end;

end
