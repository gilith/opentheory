(* ========================================================================= *)
(* THEORY PACKAGES                                                           *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Package :> Package =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun joinDirectoryFilename {directory} {filename} =
    {filename = OS.Path.concat (directory,filename)};

(* ------------------------------------------------------------------------- *)
(* A type of theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

datatype package =
    Package of
      {system : RepositorySystem.system,
       nameVersion : PackageNameVersion.nameVersion,
       directory : string,
       tarball : PackageTarball.tarball,
       information : PackageInformation.information option ref,
       theorems : PackageTheorems.theorems option ref};

(* ------------------------------------------------------------------------- *)
(* Destructors.                                                              *)
(* ------------------------------------------------------------------------- *)

fun invalidateInformation pkg =
    let
      val Package {information = info, ...} = pkg

      val () = info := NONE
    in
      ()
    end;

fun invalidateTheorems pkg =
    let
      val Package {theorems = thmsr, ...} = pkg

      val () = thmsr := NONE
    in
      ()
    end;

fun invalidate pkg =
    let
      val () = invalidateInformation pkg
      and () = invalidateTheorems pkg
    in
      ()
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

fun joinDirectory pkg = joinDirectoryFilename (directory pkg);

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
        val () = invalidate pkg

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

fun articleFiles pkg = PackageInformation.articleFiles (information pkg);

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

fun tarball (Package {tarball = x, ...}) = x;

fun packTarball pkg = PackageTarball.pack (tarball pkg) (allFiles pkg);

fun copyTarball pkg src = PackageTarball.copy (tarball pkg) src;

fun downloadTarball pkg url = PackageTarball.download (tarball pkg) url;

fun checksum pkg = PackageTarball.checksum (tarball pkg);

fun unpackTarball pkg {minimal} =
    let
      val () = invalidate pkg

      val tar = tarball pkg

      val PackageTarball.Contents {nameVersion = _, theoryFile, otherFiles} =
          PackageTarball.contents tar

      val () = PackageTarball.extract tar [theoryFile]

      val arts = articleFiles pkg

      val exts = List.map PackageExtra.filename (extraFiles pkg)

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

      val () = PackageTarball.extract tar files
    in
      ()
    end;

fun upload pkg url = PackageTarball.upload (tarball pkg) url

(* ------------------------------------------------------------------------- *)
(* Package theorems.                                                         *)
(* ------------------------------------------------------------------------- *)

fun theoremsFile pkg = PackageTheorems.mkFilename (nameVersion pkg);

fun theorems pkg =
    let
      val Package {theorems = thmsr, ...} = pkg
    in
      case !thmsr of
        SOME thms => thms
      | NONE =>
        let
          val nv = nameVersion pkg

          val {filename} = joinDirectory pkg (theoremsFile pkg)

          val thms =
              PackageTheorems.fromTextFile
                {package = nv,
                 filename = filename}

          val () = thmsr := SOME thms
        in
          thms
        end
    end;

fun writeTheorems pkg thms =
    let
      val Package {theorems = thmsr, ...} = pkg

      val {filename} = joinDirectory pkg (theoremsFile pkg)

      val () =
          PackageTheorems.toTextFile {theorems = thms, filename = filename}

      val () = thmsr := SOME thms
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

(* ------------------------------------------------------------------------- *)
(* Constructor.                                                              *)
(* ------------------------------------------------------------------------- *)

fun mk {system,nameVersion,checksum,directory} =
    let
      val tarball =
          let
            val {filename} =
                joinDirectoryFilename {directory = directory}
                  (PackageTarball.mkFilename nameVersion)
          in
            PackageTarball.mk
              {system = system,
               nameVersion = nameVersion,
               checksum = checksum,
               filename = filename}
          end

      val information = ref NONE
      and theorems = ref NONE
    in
      Package
        {system = system,
         nameVersion = nameVersion,
         directory = directory,
         tarball = tarball,
         information = information,
         theorems = theorems}
    end;

end
