(* ========================================================================= *)
(* REPOSITORY PACKAGE CHECKSUMS                                              *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure RepositoryChecksums :> RepositoryChecksums =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "pkg";

(* ------------------------------------------------------------------------- *)
(* Checksums filenames.                                                      *)
(* ------------------------------------------------------------------------- *)

fun mkFilename name =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = PackageName.toString name,
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
        else total PackageName.fromString base
    end;

fun isFilename file = Option.isSome (destFilename file);

(* ------------------------------------------------------------------------- *)
(* A pure type of package checksums.                                         *)
(* ------------------------------------------------------------------------- *)

datatype pureChecksums =
    PureChecksums of Checksum.checksum PackageNameVersionMap.map;

val emptyPure = PureChecksums (PackageNameVersionMap.new ());

fun peekPure (PureChecksums m) = PackageNameVersionMap.peek m;

fun memberPure nv (PureChecksums m) = PackageNameVersionMap.inDomain nv m;

fun previousNameVersionPure (PureChecksums m) nv =
    PackageNameVersionMap.previousNameVersion m nv;

fun latestNameVersionPure (PureChecksums m) n =
    PackageNameVersionMap.latestNameVersion m n;

fun insertPure pc (nv,c) =
    if memberPure nv pc then
      let
        val err =
            "multiple entries for package " ^ PackageNameVersion.toString nv
      in
        raise Error err
      end
    else
      let
        val PureChecksums m = pc

        val m = PackageNameVersionMap.insert m (nv,c)
      in
        PureChecksums m
      end;

fun uncurriedInsertPure (nv_c,pc) = insertPure pc nv_c;

fun deletePure (PureChecksums m) nv =
    PureChecksums (PackageNameVersionMap.delete m nv);

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;
in
  val parserPackageChecksum =
      (PackageNameVersion.parser ++
       exactChar #" " ++
       Checksum.parser ++
       exactChar #"\n") >>
      (fn (n,((),(c,()))) => [(n,c)]);
end;

fun isCommentLine l =
    case l of
      [] => true
    | [#"\n"] => true
    | _ => false;

fun fromTextFilePure {filename} =
    let
      (* Estimating parse error line numbers *)

      val lines = Stream.fromTextFile {filename = filename}

      val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
    in
      (let
         (* The character stream *)

         val chars = Stream.filter (not o isCommentLine) chars

         val chars = Parse.everything Parse.any chars

         (* The package stream *)

         val pkgs = Parse.everything parserPackageChecksum chars
       in
         Stream.foldl uncurriedInsertPure emptyPure pkgs
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in package list \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

local
  fun toLinePackageChecksum (n,c) =
      PackageNameVersion.toString n ^ " " ^ Checksum.toString c ^ "\n";
in
  fun toTextFilePure {checksums = PureChecksums m, filename = f} =
      let
        val strm = PackageNameVersionMap.toStream m

        val strm = Stream.map toLinePackageChecksum strm

        val strm = if Stream.null strm then Stream.singleton "\n" else strm
      in
        Stream.toTextFile {filename = f} strm
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Creating a new package checksums file.                                    *)
(* ------------------------------------------------------------------------- *)

fun create {filename} =
    let
      val chks = emptyPure
    in
      toTextFilePure {checksums = chks, filename = filename}
    end;

(* ------------------------------------------------------------------------- *)
(* A type of repository package checkums.                                    *)
(* ------------------------------------------------------------------------- *)

datatype checksumsState =
    UpdateFrom of {url : string}
  | UpToDate
  | Ready of pureChecksums;

datatype checksums =
    Checksums of
      {system : RepositorySystem.system,
       filename : string,
       checksums : checksumsState ref};

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

fun update chks {url} =
    let
      val Checksums {system = sys, filename = f, checksums = rox} = chks

      val () = rox := UpToDate

      val {curl = cmd} = RepositorySystem.curl sys

      val cmd = cmd ^ " " ^ url ^ " --output " ^ f

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "updating repo package list failed"
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {system,filename,updateFrom} =
    let
      val state =
          case updateFrom of
            SOME url => UpdateFrom url
          | NONE => UpToDate

      val checksums = ref state
    in
      Checksums
        {system = system,
         filename = filename,
         checksums = checksums}
    end;

fun filename (Checksums {filename = x, ...}) = {filename = x};

fun checksums chks =
    let
      val Checksums {checksums = rox, ...} = chks

      val ref ox = rox
    in
      case ox of
        Ready x => x
      | UpdateFrom url =>
        let
          val () = update chks url

          val x = fromTextFilePure (filename chks)

          val () = rox := Ready x
        in
          x
        end
      | UpToDate =>
        let
          val x = fromTextFilePure (filename chks)

          val () = rox := Ready x
        in
          x
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

fun peek chks n = peekPure (checksums chks) n;

fun member n chks = memberPure n (checksums chks);

(* ------------------------------------------------------------------------- *)
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun previousNameVersion chks nv =
    previousNameVersionPure (checksums chks) nv;

fun latestNameVersion chks n =
    latestNameVersionPure (checksums chks) n;

(* ------------------------------------------------------------------------- *)
(* Adding a new package.                                                     *)
(* ------------------------------------------------------------------------- *)

fun add chks (n,c) =
    let
      val Checksums {system = sys, filename = f, checksums = rox} = chks

(*OpenTheoryDebug
      val () =
          case !rox of
            UpdateFrom _ => raise Bug "RepositoryChecksums.add: UpdateFrom"
          | _ => ()
*)

      val () =
          case !rox of
            Ready x => rox := Ready (insertPure x (n,c))
          | _ => ()

      val {echo = cmd} = RepositorySystem.echo sys

      val cmd =
          cmd ^ " \"" ^ PackageNameVersion.toString n ^ " " ^
          Checksum.toString c ^ "\"" ^ " >> " ^ f

(*OpenTheoryTrace1
      val () = trace (cmd ^ "\n")
*)

      val () =
          if OS.Process.isSuccess (OS.Process.system cmd) then ()
          else raise Error "adding to the installed package list failed"
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Deleting a package.                                                       *)
(* ------------------------------------------------------------------------- *)

fun delete chks n =
    let
(*OpenTheoryDebug
      val () =
          let
            val Checksums {system = _, filename = _, checksums = rox} = chks
          in
            case !rox of
              UpdateFrom _ => raise Bug "RepositoryChecksums.delete: UpdateFrom"
            | _ => ()
          end
*)

      val x = deletePure (checksums chks) n

      val Checksums {system = _, filename = f, checksums = rox} = chks

      val () = rox := Ready x

      val () = toTextFilePure {checksums = x, filename = f}
    in
      ()
    end;

end
