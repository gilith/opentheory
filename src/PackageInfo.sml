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
and tarballFileExtension = "tgz"
and theoryFileExtension = "thy";

(* ------------------------------------------------------------------------- *)
(* Package directory name.                                                   *)
(* ------------------------------------------------------------------------- *)

fun packageDirectory name = {directory = PackageName.toString name};

fun joinPackageDirectory name =
    let
      val {directory = dir} = packageDirectory name
    in
      fn {filename} => {filename = OS.Path.concat (dir,filename)}
    end;

(* ------------------------------------------------------------------------- *)
(* Theory filenames.                                                         *)
(* ------------------------------------------------------------------------- *)

fun mkTheoryFile name =
    let
      val base = PackageName.base name

      val filename =
          OS.Path.joinBaseExt
            {base = PackageBase.toString base,
             ext = SOME theoryFileExtension}
    in
      {filename = filename}
    end;

fun isTheoryFile {filename} =
    case OS.Path.ext (OS.Path.file filename) of
      SOME ext => ext = theoryFileExtension
    | NONE => false;

(* ------------------------------------------------------------------------- *)
(* Tarball filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

fun mkTarball name =
    let
      val base = PackageName.base name

      val filename =
          OS.Path.joinBaseExt
            {base = PackageBase.toString base,
             ext = SOME tarballFileExtension}
    in
      {filename = filename}
    end;

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

fun theoryFile info = mkTheoryFile (name info);

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

          val () = pkg := SOME p
        in
          p
        end
    end;

(* ------------------------------------------------------------------------- *)
(* The files needed by the package.                                          *)
(* ------------------------------------------------------------------------- *)

fun articles info = Package.articles (package info);

fun extraFiles info = Package.extraFiles (package info);

fun allFiles info =
    theoryFile info ::
    articles info @
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

fun createChecksum info =
    let
      val {directory = dir} = directory info

      val {filename = tarFile} = tarball info

      val cmd = "sha1sum --binary " ^ tarFile ^ " > " ^ checksumFilename

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

  fun isChecksumChar c =
      Char.isDigit c orelse
      c = #"a" orelse c = #"b" orelse c = #"c" orelse
      c = #"d" orelse c = #"e" orelse c = #"f";

  val checksumCharParser = some isChecksumChar;

  val checksumStringParser = atLeastOne checksumCharParser >> implode;

  val separatorParser = exactString " *";
in
  fun parserChecksum info =
      let
        val {filename = tarFile} = tarball info
      in
        (checksumStringParser ++
         separatorParser ++
         exactString tarFile ++
         exactChar #"\n" ++
         finished) >> (fn (s,((),((),((),())))) => s)
      end
end;

fun readChecksum info =
    let
      val file = joinDirectory info {filename = checksumFilename}

      val strm = Stream.fromTextFile file

      val strm = Stream.listConcat (Stream.map explode strm)
    in
      Parse.fromStream (parserChecksum info) strm
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
