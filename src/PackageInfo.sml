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

val checksumFilename = "checksum.txt"
and tarballFileExtension = "tgz";

(* ------------------------------------------------------------------------- *)
(* Tarball filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

fun mkTarball name =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = PackageName.toString name,
             ext = SOME tarballFileExtension}
    in
      {filename = filename}
    end;

fun destTarball {filename} =
    let
      val {base,ext} = OS.Path.splitBaseExt (OS.Path.file filename)
    in
      case ext of
        NONE => NONE
      | SOME x =>
        if x <> tarballFileExtension then NONE
        else total PackageName.fromString base
    end;

fun isTarball file = Option.isSome (destTarball file);

(* ------------------------------------------------------------------------- *)
(* A type of theory package meta-data.                                       *)
(* ------------------------------------------------------------------------- *)

datatype info =
    Info of
      {name : PackageName.name,
       directory : string,
       package : Package.package option ref};

fun mk {name,directory} =
    let
      val package = ref NONE
    in
      Info
        {name = name,
         directory = directory,
         package = package}
    end;

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
(* Package tarball.                                                          *)
(* ------------------------------------------------------------------------- *)

fun tarball info = mkTarball (name info);

fun createTarball info =
    let
      val {directory = dir} = directory info

      val {dir = baseDir, file = pkgDir} = OS.Path.splitDirFile dir

      fun joinDir {filename} =
          {filename = OS.Path.concat (pkgDir,filename)}

      val {filename = tarFile} = joinDir (tarball info)

      val pkgFiles = map joinDir (allFiles info)

      val cmd =
          "tar czf " ^ tarFile ^
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

(* ------------------------------------------------------------------------- *)
(* Package checksum.                                                         *)
(* ------------------------------------------------------------------------- *)

fun checksum (_ : info) = {filename = checksumFilename};

fun createChecksum info =
    let
      val {directory = dir} = directory info

      val {filename = tarFile} = tarball info

      val {filename = chkFile} = checksum info

      val cmd = "sha1sum --binary " ^ tarFile ^ " > " ^ chkFile

(*OpenTheoryTrace1
      val () = print (cmd ^ "\n")
*)

      val workingDir = OS.FileSys.getDir ()
    in
      let
        val () = OS.FileSys.chDir dir

        val () =
            if OS.Process.isSuccess (OS.Process.system cmd) then ()
            else raise Error "creating checksum failed"

        val () = OS.FileSys.chDir workingDir
      in
        ()
      end
      handle e => let val () = OS.FileSys.chDir workingDir in raise e end
    end;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val separatorParser = exactString " *";
in
  fun parserChecksumTarball info =
      let
        val {filename = tarFile} = tarball info
      in
        (Checksum.parser ++
         separatorParser ++
         exactString tarFile ++
         exactChar #"\n" ++
         finished) >> (fn (c,((),((),((),())))) => c)
      end
end;

fun readChecksum info =
    let
      val file = joinDirectory info (checksum info)

      val strm = Stream.fromTextFile file

      val strm = Stream.listConcat (Stream.map explode strm)
    in
      Parse.fromStream (parserChecksumTarball info) strm
    end
    handle Parse.NoParse => raise Error "bad checksum format";

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun packages info =
    let
      val pkg = package info
    in
      PackageNameSet.fromList (Package.packages pkg)
    end;

end
