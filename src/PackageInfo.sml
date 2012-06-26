(* ========================================================================= *)
(* THEORY PACKAGE META-DATA                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure PackageInfo :> PackageInfo =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory package meta-data.                                       *)
(* ------------------------------------------------------------------------- *)

datatype info =
    Info of
      {system : DirectorySystem.system,
       nameVersion : PackageNameVersion.nameVersion,
       directory : string,
       package : Package.package option ref,
       theorems : PackageTheorems.theorems option ref};

fun mk {system,nameVersion,directory} =
    let
      val package = ref NONE
      and theorems = ref NONE
    in
      Info
        {system = system,
         nameVersion = nameVersion,
         directory = directory,
         package = package,
         theorems = theorems}
    end;

fun system (Info {system = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

fun nameVersion (Info {nameVersion = x, ...}) = x;

fun name info = PackageNameVersion.name (nameVersion info);

fun version info = PackageNameVersion.version (nameVersion info);

(* ------------------------------------------------------------------------- *)
(* Package directory.                                                        *)
(* ------------------------------------------------------------------------- *)

fun directory (Info {directory = x, ...}) = {directory = x};

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

fun theoryFile info = Package.mkFilename (name info);

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
          val () =
              let
                val n1 = nameVersion info
                and n2 = Package.nameVersion p
              in
                if PackageNameVersion.equal n1 n2 then ()
                else  raise Bug "PackageInfo.package: different name"
              end
*)

          val () = pkg := SOME p
        in
          p
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

fun description info = Package.description (package info);

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

fun author info = Package.author (package info);

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

fun license info = Package.license (package info);

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

fun requires info = Package.requires (package info);

(* ------------------------------------------------------------------------- *)
(* The files needed by the package.                                          *)
(* ------------------------------------------------------------------------- *)

fun articleFiles info = Package.articles (package info);

fun extraFiles info = Package.extraFiles (package info);

fun allFiles info =
    theoryFile info ::
    articleFiles info @
    List.map PackageExtra.filename (extraFiles info);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun includes info =
    let
      val pkg = package info
    in
      PackageNameVersionSet.fromList (Package.includes pkg)
    end;

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

fun show info = Package.show (package info);

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

fun theory info = Package.theory (package info);

fun emptyTheory info = Package.emptyTheory (package info);

(* ------------------------------------------------------------------------- *)
(* Package tarball.                                                          *)
(* ------------------------------------------------------------------------- *)

fun tarball info = PackageTarball.mkFilename (nameVersion info);

fun createTarball info =
    let
      val sys = system info
      and {directory = dir} = directory info

      val {dir = baseDir, file = pkgDir} = OS.Path.splitDirFile dir

      fun joinDir {filename} =
          {filename = OS.Path.concat (pkgDir,filename)}

      val {filename = tarFile} = joinDir (tarball info)

      val pkgFiles = List.map joinDir (allFiles info)

      val {tar = cmd} = DirectorySystem.tar sys

      val cmd =
          cmd ^ " czf " ^ tarFile ^
          String.concat (List.map (fn {filename = f} => " " ^ f) pkgFiles)

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

fun copyTarball info {filename = src} =
    let
      val sys = system info

      val {filename = dest} = joinDirectory info (tarball info)

      val {cp = cmd} = DirectorySystem.cp sys

      val cmd = cmd ^ " " ^ src ^ " " ^ dest

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "copying the package tarball failed"

      val {chmod = cmd} = DirectorySystem.chmod sys

      val cmd = cmd ^ " 644 " ^ dest

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
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
      val () = trace (cmd ^ "\n")
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
    if List.null files then ()
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
            cmd ^ " xzf " ^ tarFile ^ String.concat (List.map mkArg files)

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

fun unpackTarball info contents {minimal} =
    let
      val PackageTarball.Contents
            {nameVersion = nv, theoryFile, otherFiles} = contents

(*OpenTheoryDebug
        val () = if PackageNameVersion.equal (nameVersion info) nv then ()
                 else raise Bug "PackageInfo.unpackTarball: name clash"
*)

      val () = extractTarball info [theoryFile]

      val pkg = package info

      val arts = Package.articles pkg

      val exts = List.map PackageExtra.filename (Package.extraFiles pkg)

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

fun uploadTarball info chk {url,token} =
    let
      val sys = system info

      val {filename = file} = joinDirectory info (tarball info)

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = DirectorySystem.curl sys

      val cmd =
          cmd ^ " " ^ url ^
          " --form \"u=" ^ token ^ "\"" ^
          " --form \"t=@" ^ file ^ "\"" ^
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
(* Package theorems.                                                         *)
(* ------------------------------------------------------------------------- *)

fun theoremsFile info = PackageTheorems.mkFilename (nameVersion info);

fun theorems info =
    let
      val Info {theorems = ths, ...} = info
    in
      case !ths of
        SOME t => t
      | NONE =>
        let
          val nv = nameVersion info

          val {filename} = joinDirectory info (theoremsFile info)

          val t =
              PackageTheorems.fromTextFile
                {package = nv,
                 filename = filename}

          val () = ths := SOME t
        in
          t
        end
    end;

fun writeTheorems info t =
    let
      val Info {theorems = ths, ...} = info

      val {filename} = joinDirectory info (theoremsFile info)

      val () =
          PackageTheorems.toTextFile {theorems = t, filename = filename}

      val () = ths := SOME t
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Package document.                                                         *)
(* ------------------------------------------------------------------------- *)

fun documentFile info = PackageDocument.mkFilename (nameVersion info);

fun writeDocument info doc =
    let
      val {filename} = joinDirectory info (documentFile info)
    in
      PackageDocument.toHtmlFile {document = doc, filename = filename}
    end;

end
