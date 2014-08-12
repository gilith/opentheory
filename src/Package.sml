(* ========================================================================= *)
(* THEORY PACKAGES                                                           *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Package :> Package =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

datatype package =
    Package of
      {system : RepositorySystem.system,
       nameVersion : PackageNameVersion.nameVersion,
       directory : string,
       information : PackageInformation.information option ref,
       tarball : PackageTarball.tarball option ref,
       theorems : PackageTheorems.theorems option ref};

fun flushInformation pkg =
    let
      val Package {information = info, ...} = pkg

      val () = info := NONE
    in
      ()
    end;

fun flushTarball pkg =
    let
      val Package {tarball = tarr, ...} = pkg

      val () = tarr := NONE
    in
      ()
    end;

fun flushTheorems pkg =
    let
      val Package {theorems = thms, ...} = pkg

      val () = thms := NONE
    in
      ()
    end;

fun flush pkg =
    let
      val () = flushInformation pkg
      and () = flushTarball pkg
      and () = flushTheorems pkg
    in
      ()
    end;

fun mk {system,nameVersion,directory} =
    let
      val information = ref NONE
      and tarball = ref NONE
      and theorems = ref NONE
    in
      Package
        {system = system,
         nameVersion = nameVersion,
         directory = directory,
         information = information,
         tarball = tarball,
         theorems = theorems}
    end;

fun system (Package {system = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Package name and version.                                                 *)
(* ------------------------------------------------------------------------- *)

fun nameVersion (Package {nameVersion = x, ...}) = x;

fun name pkg = PackageNameVersion.name (nameVersion pkg);

fun version pkg = PackageNameVersion.version (nameVersion pkg);

(* ------------------------------------------------------------------------- *)
(* Package directory.                                                        *)
(* ------------------------------------------------------------------------- *)

fun directory (Package {directory = x, ...}) = {directory = x};

fun joinDirectory pkg =
    let
      val {directory = dir} = directory pkg
    in
      fn {filename} => {filename = OS.Path.concat (dir,filename)}
    end;

fun existsDirectory pkg =
    let
      val {directory = dir} = directory pkg
    in
      OS.FileSys.isDir dir
      handle OS.SysErr _ => false
    end;

fun createDirectory pkg =
    let
      val {directory = dir} = directory pkg
    in
      OS.FileSys.mkDir dir
    end;

local
  fun delete {filename} = OS.FileSys.remove filename
in
  fun nukeDirectory pkg =
      let
        val () = flush pkg

        val {directory = dir} = directory pkg

        val filenames = readDirectory {directory = dir}

        val () = app delete filenames

        val () = OS.FileSys.rmDir dir
      in
        ()
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Is the package installed?                                                 *)
(* ------------------------------------------------------------------------- *)

fun isInstalled pkg = existsDirectory pkg;

(* ------------------------------------------------------------------------- *)
(* The package theory file.                                                  *)
(* ------------------------------------------------------------------------- *)

fun theoryFile pkg = PackageInformation.mkFilename (name pkg);

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

fun information pkg =
    let
      val Package {information = info, ...} = pkg
    in
      case !info of
        SOME inf => inf
      | NONE =>
        let
          val filename = joinDirectory pkg (theoryFile pkg)

          val inf = PackageInformation.fromTextFile filename

(*OpenTheoryDebug
          val () =
              let
                val n1 = nameVersion pkg
                and n2 = PackageInformation.nameVersion inf
              in
                if PackageNameVersion.equal n1 n2 then ()
                else  raise Bug "Package.information: different name"
              end
*)

          val () = info := SOME inf
        in
          inf
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

fun description pkg = PackageInformation.description (information pkg);

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

fun author pkg = PackageInformation.author (information pkg);

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

fun license pkg = PackageInformation.license (information pkg);

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

fun requires pkg = PackageInformation.requires (information pkg);

(* ------------------------------------------------------------------------- *)
(* The files needed by the package.                                          *)
(* ------------------------------------------------------------------------- *)

fun articleFiles pkg = PackageInformation.articles (information pkg);

fun extraFiles pkg = PackageInformation.extraFiles (information pkg);

fun allFiles pkg =
    theoryFile pkg ::
    articleFiles pkg @
    List.map PackageExtra.filename (extraFiles pkg);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun includes pkg = PackageInformation.includes (information pkg);

fun nameVersionIncludes pkg =
    PackageInformation.nameVersionIncludes (information pkg);

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

fun show pkg = PackageInformation.show (information pkg);

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

fun theory pkg = PackageInformation.theory (information pkg);

fun emptyTheory pkg = PackageInformation.emptyTheory (information pkg);

(* ------------------------------------------------------------------------- *)
(* Package tarball.                                                          *)
(* ------------------------------------------------------------------------- *)

fun tarballFile pkg = PackageTarball.mkFilename (nameVersion pkg);

fun tarball pkg =
    let
      val Package {system = sys, tarball = tarr, ...} = pkg
    in
      case !tarr of
        SOME tar => tar
      | NONE =>
        let
          val {filename = file} = joinDirectory pkg (tarballFile pkg)

          val tar = PackageTarball.mk {system = sys, filename = file}

          val () = tarr := SOME tar
        in
          tar
        end
    end;

fun packTarball pkg =
    let
      val () = flushTarball pkg

      val sys = system pkg
      and {directory = dir} = directory pkg

      val {dir = baseDir, file = pkgDir} = OS.Path.splitDirFile dir

      fun joinDir {filename} =
          {filename = OS.Path.concat (pkgDir,filename)}

      val {filename = tarFile} = joinDir (tarballFile pkg)

      val pkgFiles = List.map joinDir (allFiles pkg)

      val {tar = cmd} = RepositorySystem.tar sys

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

fun copyTarball pkg {filename = src} =
    let
      val () = flushTarball pkg

      val sys = system pkg

      val {filename = dest} = joinDirectory pkg (tarballFile pkg)

      val {cp = cmd} = RepositorySystem.cp sys

      val cmd = cmd ^ " " ^ src ^ " " ^ dest

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "copying the package tarball failed"

      val {chmod = cmd} = RepositorySystem.chmod sys

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

fun downloadTarball pkg {url} =
    let
      val () = flushTarball pkg

      val sys = system pkg

      val {filename = f} = joinDirectory pkg (tarballFile pkg)

      val {curl = cmd} = RepositorySystem.curl sys

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

fun checksumTarball pkg = PackageTarball.checksum (tarball pkg);

fun contentsTarball pkg = PackageTarball.contents (tarball pkg);

fun extractTarball pkg files =
    if List.null files then ()
    else
      let
        val sys = system pkg
        and {directory = dir} = directory pkg

        val {dir = baseDir, file = pkgDir} = OS.Path.splitDirFile dir

        fun joinDir {filename} =
            {filename = OS.Path.concat (pkgDir,filename)}

        fun mkArg file =
            let
              val {filename} = joinDir file
            in
              " " ^ filename
            end

        val {filename = tarFile} = joinDir (tarballFile pkg)

        val {tar = cmd} = RepositorySystem.tar sys

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

fun unpackTarball pkg contents {minimal} =
    let
      val PackageTarball.Contents
            {nameVersion = nv, theoryFile, otherFiles} = contents

(*OpenTheoryDebug
      val () = if PackageNameVersion.equal (nameVersion pkg) nv then ()
               else raise Bug "Package.unpackTarball: name clash"
*)

      val () = extractTarball pkg [theoryFile]

      val pkg = information pkg

      val arts = PackageInformation.articles pkg

      val exts =
          List.map PackageExtra.filename (PackageInformation.extraFiles pkg)

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

      val () = extractTarball pkg files
    in
      ()
    end;

fun uploadTarball pkg chk {url,token} =
    let
      val sys = system pkg

      val {filename = file} = joinDirectory pkg (tarballFile pkg)

      val tmpFile = OS.FileSys.tmpName ()

      val {curl = cmd} = RepositorySystem.curl sys

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

fun theoremsFile pkg = PackageTheorems.mkFilename (nameVersion pkg);

fun theorems pkg =
    let
      val Info {theorems = ths, ...} = pkg
    in
      case !ths of
        SOME t => t
      | NONE =>
        let
          val nv = nameVersion pkg

          val {filename} = joinDirectory pkg (theoremsFile pkg)

          val t =
              PackageTheorems.fromTextFile
                {package = nv,
                 filename = filename}

          val () = ths := SOME t
        in
          t
        end
    end;

fun writeTheorems pkg t =
    let
      val Info {theorems = ths, ...} = pkg

      val {filename} = joinDirectory pkg (theoremsFile pkg)

      val () =
          PackageTheorems.toTextFile {theorems = t, filename = filename}

      val () = ths := SOME t
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Package document.                                                         *)
(* ------------------------------------------------------------------------- *)

fun documentFile pkg = PackageDocument.mkFilename (nameVersion pkg);

fun writeDocument pkg doc =
    let
      val {filename} = joinDirectory pkg (documentFile pkg)
    in
      PackageDocument.toHtmlFile {document = doc, filename = filename}
    end;

end
