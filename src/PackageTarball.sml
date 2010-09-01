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

fun contents sys {filename} =
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

end
