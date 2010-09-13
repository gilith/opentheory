(* ========================================================================= *)
(* THEORY PACKAGE META-DATA                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageInfo :> PackageInfo =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val uploadSuccessString = "successfully uploaded";

(* ------------------------------------------------------------------------- *)
(* A type of theory package meta-data.                                       *)
(* ------------------------------------------------------------------------- *)

datatype info =
    Info of
      {system : DirectorySystem.system,
       name : PackageName.name,
       directory : string,
       package : Package.package option ref};

fun mk {system,name,directory} =
    let
      val package = ref NONE
    in
      Info
        {system = system,
         name = name,
         directory = directory,
         package = package}
    end;

fun system (Info {system = x, ...}) = x;

fun name (Info {name = x, ...}) = x;

fun base info = PackageName.base (name info);

fun version info = PackageName.version (name info);

fun directory (Info {directory = x, ...}) = {directory = x};

(* ------------------------------------------------------------------------- *)
(* Package directory operations.                                             *)
(* ------------------------------------------------------------------------- *)

fun joinDirectory info =
    let
      val {directory = dir} = directory info
    in
      fn {filename} => {filename = OS.Path.concat (dir,filename)}
    end;

fun existsDirectory info =
    let
      val {directory = dir} = directory info
    in
      OS.FileSys.isDir dir
      handle OS.SysErr _ => false
    end;

fun createDirectory info =
    let
      val {directory = dir} = directory info
    in
      OS.FileSys.mkDir dir
    end;

local
  fun delete {filename} = OS.FileSys.remove filename
in
  fun nukeDirectory info =
      let
        val Info {directory = dir, package = pkg, ...} = info

        val filenames = readDirectory {directory = dir}

        val () = app delete filenames

        val () = pkg := NONE

        val () = OS.FileSys.rmDir dir
      in
        ()
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Is the package installed?                                                 *)
(* ------------------------------------------------------------------------- *)

fun isInstalled info = existsDirectory info;

(* ------------------------------------------------------------------------- *)
(* The package theory file.                                                  *)
(* ------------------------------------------------------------------------- *)

fun theoryFile info = Package.mkFilename (base info);

(* ------------------------------------------------------------------------- *)
(* Read the package.                                                         *)
(* ------------------------------------------------------------------------- *)

fun package info =
    let
      val Info {package = pkg, ...} = info
    in
      case !pkg of
        SOME p => p
      | NONE =>
        let
          val filename = joinDirectory info (theoryFile info)

          val p = Package.fromTextFile filename

(*OpenTheoryDebug
          val _ = PackageName.equal (name info) (Package.name p) orelse
                  raise Bug "PackageInfo.package: different name"
*)

          val () = pkg := SOME p
        in
          p
        end
    end;

(* ------------------------------------------------------------------------- *)
(* The files needed by the package.                                          *)
(* ------------------------------------------------------------------------- *)

fun articleFiles info = Package.articles (package info);

fun extraFiles info = Package.extraFiles (package info);

fun allFiles info =
    theoryFile info ::
    articleFiles info @
    map Package.filenameExtraFile (extraFiles info);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun packages info =
    let
      val pkg = package info
    in
      PackageNameSet.fromList (Package.packages pkg)
    end;

(* ------------------------------------------------------------------------- *)
(* Package tarball.                                                          *)
(* ------------------------------------------------------------------------- *)

fun tarball info = PackageTarball.mkFilename (name info);

fun createTarball info =
    let
      val sys = system info
      and {directory = dir} = directory info

      val {dir = baseDir, file = pkgDir} = OS.Path.splitDirFile dir

      fun joinDir {filename} =
          {filename = OS.Path.concat (pkgDir,filename)}

      val {filename = tarFile} = joinDir (tarball info)

      val pkgFiles = map joinDir (allFiles info)

      val {tar = cmd} = DirectorySystem.tar sys

      val cmd =
          cmd ^ " czf " ^ tarFile ^
          String.concat (map (fn {filename = f} => " " ^ f) pkgFiles)

(*OpenTheoryTrace1
      val () = print (cmd ^ "\n")
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

fun copyTarball info {filename = src} =
    let
      val sys = system info

      val {filename = dest} = joinDirectory info (tarball info)

      val {cp = cmd} = DirectorySystem.cp sys

      val cmd = cmd ^ " " ^ src ^ " " ^ dest

(*OpenTheoryTrace1
      val () = print (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "copying the package tarball failed"

      val {chmod = cmd} = DirectorySystem.chmod sys

      val cmd = cmd ^ " 644 " ^ dest

(*OpenTheoryTrace1
      val () = print (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "changing mode of the package tarball failed"
    in
      ()
    end;

fun downloadTarball info {url} =
    let
      val sys = system info

      val {filename = f} = joinDirectory info (tarball info)

      val {curl = cmd} = DirectorySystem.curl sys

      val cmd = cmd ^ " " ^ url ^ " --output " ^ f

(*OpenTheoryTrace1
      val () = print (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "downloading the package tarball failed"
    in
      ()
    end;

fun checksumTarball info =
    let
      val sys = system info

      val tarFile = joinDirectory info (tarball info)
    in
      PackageTarball.checksum sys tarFile
    end;

fun contentsTarball info =
    let
      val sys = system info

      val tarFile = joinDirectory info (tarball info)
    in
      PackageTarball.contents sys tarFile
    end;

fun extractTarball info files =
    if null files then ()
    else
      let
        val sys = system info
        and {directory = dir} = directory info

        val {dir = baseDir, file = pkgDir} = OS.Path.splitDirFile dir

        fun joinDir {filename} =
            {filename = OS.Path.concat (pkgDir,filename)}

        fun mkArg file =
            let
              val {filename} = joinDir file
            in
              " " ^ filename
            end

        val {filename = tarFile} = joinDir (tarball info)

        val {tar = cmd} = DirectorySystem.tar sys

        val cmd =
            cmd ^ " xzf " ^ tarFile ^ String.concat (map mkArg files)

(*OpenTheoryTrace1
        val () = print (cmd ^ "\n")
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

fun unpackTarball info contents {minimal} =
    let
      val PackageTarball.Contents {name = n, theoryFile, otherFiles} = contents

(*OpenTheoryDebug
        val () = if PackageName.equal (name info) n then ()
                 else raise Bug "PackageInfo.unpackTarball: name clash"
*)

      val () = extractTarball info [theoryFile]

      val pkg = package info

      val arts = Package.articles pkg

      val exts = List.map Package.filenameExtraFile (Package.extraFiles pkg)

      val () =
          let
            fun add ({filename},set) = StringSet.add set filename

            val filel = arts @ exts

            val files = List.foldl add StringSet.empty filel

            val () =
                if length filel = StringSet.size files then ()
                else raise Error "filename clash in package"

            val files' = List.foldl add StringSet.empty otherFiles

            val () =
                if StringSet.subset files files' then ()
                else raise Error "extra files in tarball"

            val () =
                if StringSet.subset files' files then ()
                else raise Error "missing package files in tarball"
          in
            ()
          end

      val files = if minimal then arts else arts @ exts

      val () = extractTarball info files
    in
      ()
    end;

fun uploadTarball info chk {url} =
    let
      val sys = system info

      val {filename = f} = joinDirectory info (tarball info)

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = DirectorySystem.curl sys

      val cmd =
          cmd ^ " " ^ url ^
          " --form \"t=@" ^ f ^ "\"" ^
          " --form \"c=" ^ Checksum.toString chk ^ "\"" ^
          " --form \"s=upload package\"" ^
          " --output " ^ tmpFile

(*OpenTheoryTrace1
      val () = print (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "uploading the package tarball failed"

      val lines = Stream.toList (Stream.fromTextFile {filename = tmpFile})

      val () = OS.FileSys.remove tmpFile
    in
      case lines of
        [] => raise Error "no response from repo"
      | line :: _ =>
        let
          val response = chomp (String.concat lines)
        in
          if String.isSubstring uploadSuccessString line then
            {response = response}
          else
            raise Error ("error response from repo:\n" ^ response)
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Package document.                                                         *)
(* ------------------------------------------------------------------------- *)

fun document info = PackageDocument.mkFilename (name info);

fun writeDocument info doc =
    let
      val {filename = f} = joinDirectory info (document info)
    in
      PackageDocument.toHtmlFile {document = doc, filename = f}
    end;

end
