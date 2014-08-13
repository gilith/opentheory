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
(* Copying a tarball.                                                        *)
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
(* A type of package tarball.                                                *)
(* ------------------------------------------------------------------------- *)

datatype tarball =
    Tarball of
      {system : RepositorySystem.system,
       filename : string,
       contents : contents option ref,
       checksum : Checksum.checksum option ref};

fun mk {system,filename} =
    let
      val contents = ref NONE
      and checksum = ref NONE
    in
      Tarball
        {system = system,
         filename = filename,
         contents = contents,
         checksum = checksum}
    end;

fun filename (Tarball {filename = x, ...}) = {filename = x};

fun contents tar =
    let
      val Tarball {system = sys, filename, contents = cntr, ...} = tar
    in
      case !cntr of
        SOME cnt => cnt
      | NONE =>
        let
          val cnt = listContents sys {filename = filename}

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

fun copy tar {filename = dest} =
    let
      val Tarball
            {system = sys,
             filename = src,
             contents = ref cnt,
             checksum = ref chk} = tar

      val () = copyTarball sys {filename = src} {filename = dest}
    in
      Tarball
        {system = sys,
         filename = dest,
         contents = ref cnt,
         checksum = ref chk}
    end;

end
