(* ========================================================================= *)
(* PACKAGE TARBALLS                                                          *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageTarball :> PackageTarball =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "tgz";

(* ------------------------------------------------------------------------- *)
(* Tarball filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

fun mkFilename namever =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = PackageNameVersion.toString namever,
             ext = SOME fileExtension}
    in
      {filename = filename}
    end;

fun destFilename {filename} =
    let
      val {base,ext} = OS.Path.splitBaseExt (OS.Path.file filename)
    in
      case ext of
        NONE => NONE
      | SOME x =>
        if x <> fileExtension then NONE
        else total PackageNameVersion.fromString base
    end;

fun isFilename file = Option.isSome (destFilename file);

(* ------------------------------------------------------------------------- *)
(* Listing the contents.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype contents =
    Contents of
      {nameVersion : PackageNameVersion.nameVersion,
       theoryFile : {filename : string},
       otherFiles : {filename : string} list}

fun rawContents sys {filename} =
    let
      val tmpFile = OS.FileSys.tmpName ()

      val {tar = cmd} = RepositorySystem.tar sys

      val cmd = cmd ^ " tzf " ^ filename ^ " > " ^ tmpFile

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "listing tarball failed"

      val files = Stream.fromTextFile {filename = tmpFile}

      val files = List.map (fn f => {filename = chomp f}) (Stream.toList files)

      val () = OS.FileSys.remove tmpFile
    in
      files
    end;

local
  fun split {filename} =
      let
        val {dir,file} = OS.Path.splitDirFile filename
      in
        (dir, {filename = file})
      end;
in
  fun listContents sys tarFile =
      let
        val files = rawContents sys tarFile

        val (dirs,files) = unzip (List.map split files)

        val dir =
            case dirs of
              [] => raise Error "empty tarball"
            | d :: ds =>
              case List.find (not o equal d) ds of
                NONE => d
              | SOME d' =>
                raise Error ("inconsistent directories in tarball:\n" ^
                             "  \"" ^ d ^ "\"\n" ^
                             "  \"" ^ d' ^ "\"")

        val namever = PackageNameVersion.fromString dir

        val theoryFile =
            PackageInformation.mkFilename (PackageNameVersion.name namever)

        val otherFiles =
            case List.partition (equal theoryFile) files of
              ([],_) => raise Error "no theory file in tarball"
            | ([_],l) => l
            | (_ :: _ :: _, _) =>
              raise Error "multiple theory files in tarball"
      in
        Contents
          {nameVersion = namever,
           theoryFile = theoryFile,
           otherFiles = otherFiles}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Creating a checksum.                                                      *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val separatorParser = exactString " *";
in
  fun parserChecksum {filename} =
      (Checksum.parser ++
       separatorParser ++
       exactString filename ++
       exactChar #"\n" ++
       finished) >> (fn (c,((),((),((),())))) => c);
end;

fun readChecksum tarFile tmpFile =
    let
      val strm = Stream.fromTextFile tmpFile

      val strm = Stream.listConcat (Stream.map String.explode strm)
    in
      Parse.fromStream (parserChecksum tarFile) strm
    end
    handle Parse.NoParse => raise Error "bad checksum format";

fun createChecksum sys {filename = tarFile} =
    let
      val tmpFile = OS.FileSys.tmpName ()

      val {sha = cmd} = RepositorySystem.sha sys

      val cmd = cmd ^ " " ^ tarFile ^ " > " ^ tmpFile

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "creating tarball checksum failed"

      val chk = readChecksum {filename = tarFile} {filename = tmpFile}

      val () = OS.FileSys.remove tmpFile
    in
      chk
    end;

(* ------------------------------------------------------------------------- *)
(* Packing a tarball.                                                        *)
(* ------------------------------------------------------------------------- *)

fun joinTarFiles {filename} files =
    let
      val {dir = pkgDir, file = tarFile} = OS.Path.splitDirFile filename

      val {dir = baseDir, file = pkgDir} = OS.Path.splitDirFile pkgDir

      fun joinDir {filename} = OS.Path.concat (pkgDir,filename)

      val tarFile = joinDir {filename = tarFile}

      val files = List.map joinDir files
    in
      ({directory = baseDir}, join " " (tarFile :: files))
    end;

fun packTarball sys tar files =
    let
      val ({directory = baseDir}, tarFiles) = joinTarFiles tar files

      val {tar} = RepositorySystem.tar sys

      val cmd = tar ^ " czf " ^ tarFiles

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)
      val workingDir = OS.FileSys.getDir ()
    in
      let
        val () = OS.FileSys.chDir baseDir

        val () =
            if OS.Process.isSuccess (OS.Process.system cmd) then ()
            else raise Error "creating tarball failed"

        val () = OS.FileSys.chDir workingDir
      in
        ()
      end
      handle e => let val () = OS.FileSys.chDir workingDir in raise e end
    end;

(* ------------------------------------------------------------------------- *)
(* Copying a tarball from a file.                                            *)
(* ------------------------------------------------------------------------- *)

fun copyTarball sys {filename = src} {filename = dest} =
    let
      val {cp} = RepositorySystem.cp sys

      val cmd = cp ^ " " ^ src ^ " " ^ dest

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)
      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "copying package tarball failed"

      val {chmod} = RepositorySystem.chmod sys

      val cmd = chmod ^ " 644 " ^ dest

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)
      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "changing mode of package tarball failed"
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Downloading a tarball.                                                    *)
(* ------------------------------------------------------------------------- *)

fun downloadTarball sys {url} {filename} =
    let
      val {curl} = RepositorySystem.curl sys

      val cmd = curl ^ " " ^ url ^ " --output " ^ filename

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "downloading the package tarball failed"
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Extracting files from a tarball.                                          *)
(* ------------------------------------------------------------------------- *)

fun extractTarball sys tar files =
    if List.null files then ()
    else
      let
        val ({directory = baseDir}, tarFiles) = joinTarFiles tar files

        val {tar} = RepositorySystem.tar sys

        val cmd = tar ^ " xzf " ^ tarFiles

(*OpenTheoryTrace1
        val () = trace (cmd ^ "\n")
*)
        val workingDir = OS.FileSys.getDir ()
      in
        let
          val () = OS.FileSys.chDir baseDir

          val () =
              if OS.Process.isSuccess (OS.Process.system cmd) then ()
              else raise Error "unpacking tarball failed"

          val () = OS.FileSys.chDir workingDir
        in
          ()
        end
        handle e => let val () = OS.FileSys.chDir workingDir in raise e end
      end;

(* ------------------------------------------------------------------------- *)
(* Uploading a tarball.                                                      *)
(* ------------------------------------------------------------------------- *)

fun uploadTarball sys {filename} chk {url,token} =
    let
      val tmpFile = OS.FileSys.tmpName ()

      val {curl} = RepositorySystem.curl sys

      val cmd =
          curl ^ " " ^ url ^
          " --form \"u=" ^ token ^ "\"" ^
          " --form \"t=@" ^ filename ^ "\"" ^
          " --form \"c=" ^ Checksum.toString chk ^ "\"" ^
          " --form \"x=upload package\"" ^
          " --output " ^ tmpFile

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "uploading the package tarball failed"

      val lines = Stream.toList (Stream.fromTextFile {filename = tmpFile})

      val () = OS.FileSys.remove tmpFile

      val response = chomp (String.concat lines)
    in
      if size response = 0 then raise Error "no response from repo"
      else {response = response}
    end;

(* ------------------------------------------------------------------------- *)
(* A type of package tarball.                                                *)
(* ------------------------------------------------------------------------- *)

datatype tarball =
    Tarball of
      {system : RepositorySystem.system,
       nameVersion : PackageNameVersion.nameVersion,
       filename : string,
       contents : contents option ref,
       checksum : Checksum.checksum option ref};

fun invalidateContents tar =
    let
      val Tarball {contents = cntr, ...} = tar

      val () = cntr := NONE
    in
      ()
    end;

fun invalidateChecksum tar =
    let
      val Tarball {checksum = chkr, ...} = tar

      val () = chkr := NONE
    in
      ()
    end;

fun invalidate tar =
    let
      val () = invalidateContents tar
      and () = invalidateChecksum tar
    in
      ()
    end;

fun mk {system,nameVersion,checksum,filename} =
    let
      val cntr = ref NONE
      and chkr = ref checksum
    in
      Tarball
        {system = system,
         nameVersion = nameVersion,
         filename = filename,
         contents = cntr,
         checksum = chkr}
    end;

fun filename (Tarball {filename = x, ...}) = {filename = x};

fun contents tar =
    let
      val Tarball
        {system = sys,
         nameVersion = namever,
         filename,
         contents = cntr,
         ...} = tar
    in
      case !cntr of
        SOME cnt => cnt
      | NONE =>
        let
          val cnt = listContents sys {filename = filename}

          val Contents {nameVersion = nv, ...} = cnt

          val () =
              if PackageNameVersion.equal nv namever then ()
              else raise Error "package tarball has unexpected contents"

          val () = cntr := SOME cnt
        in
          cnt
        end
    end;

fun checksum tar =
    let
      val Tarball {system = sys, filename, checksum = chkr, ...} = tar
    in
      case !chkr of
        SOME chk => chk
      | NONE =>
        let
          val chk = createChecksum sys {filename = filename}

          val () = chkr := SOME chk
        in
          chk
        end
    end;

fun pack tar files =
    let
      val Tarball {system = sys, filename, ...} = tar

      val () = invalidate tar

      val () = packTarball sys {filename = filename} files
    in
      ()
    end;

fun copy tar {filename = src} =
    let
      val Tarball {system = sys, filename = dest, ...} = tar

      val () = invalidate tar

      val () = copyTarball sys {filename = src} {filename = dest}
    in
      ()
    end;

fun download tar url =
    let
      val Tarball {system = sys, filename, ...} = tar

      val () = invalidate tar

      val () = downloadTarball sys url {filename = filename}
    in
      ()
    end;

fun extract tar files =
    let
      val Tarball {system = sys, filename, ...} = tar

      val () = extractTarball sys {filename = filename} files
    in
      ()
    end;

fun upload tar url =
    let
      val Tarball {system = sys, filename, ...} = tar

      val chk = checksum tar
    in
      uploadTarball sys {filename = filename} chk url
    end;

end
