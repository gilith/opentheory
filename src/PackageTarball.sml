(* ========================================================================= *)
(* PACKAGE TARBALLS                                                          *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
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

      val {tar = cmd} = DirectorySystem.tar sys

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
  fun contents sys tarFile =
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

        val theoryFile = Package.mkFilename (PackageNameVersion.base namever)

        val otherFiles =
            case List.partition (equal theoryFile) files of
              ([],_) => raise Error "no theory file in tarball"
            | ([_],l) => l
            | (_ :: _ :: _, _) => raise Error "multiple theory files in tarball"
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

fun checksum sys {filename = tarFile} =
    let
      val tmpFile = OS.FileSys.tmpName ()

      val {sha = cmd} = DirectorySystem.sha sys

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

end
