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

val tarballFileExtension = "tgz";

(* ------------------------------------------------------------------------- *)
(* Tarball filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

fun mkFilename name =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = PackageName.toString name,
             ext = SOME tarballFileExtension}
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
        if x <> tarballFileExtension then NONE
        else total PackageName.fromString base
    end;

fun isFilename file = Option.isSome (destFilename file);

(* ------------------------------------------------------------------------- *)
(* Listing the contents.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype contents =
    Contents of
      {name : PackageName.name,
       theoryFile : {filename : string},
       otherFiles : {filename : string} list}

fun rawContents sys {filename} =
    let
      val tmpFile = OS.FileSys.tmpName ()

      val {tar = cmd} = DirectoryConfig.tarSystem sys

      val cmd = cmd ^ " tzf " ^ filename ^ " > " ^ tmpFile

(*OpenTheoryTrace1
      val () = print (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "listing tarball failed"

      val files = Stream.fromTextFile {filename = tmpFile}

      val files = map (fn f => {filename = chomp f}) (Stream.toList files)

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

        val (dirs,files) = unzip (map split files)

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

        val name = PackageName.fromString dir

        val theoryFile = Package.mkFilename (PackageName.base name)

        val otherFiles =
            case List.partition (equal theoryFile) files of
              ([],_) => raise Error "no theory file in tarball"
            | ([_],l) => l
            | (_ :: _ :: _, _) => raise Error "multiple theory files in tarball"
      in
        Contents
          {name = name,
           theoryFile = theoryFile,
           otherFiles = otherFiles}
      end;
end;

end
