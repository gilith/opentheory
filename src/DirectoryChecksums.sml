(* ========================================================================= *)
(* PACKAGE DIRECTORY CHECKSUMS                                               *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryChecksums :> DirectoryChecksums =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "pkg";

(* ------------------------------------------------------------------------- *)
(* Checksums filenames.                                                      *)
(* ------------------------------------------------------------------------- *)

fun mkFilename base =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = base,
             ext = SOME fileExtension}
    in
      {filename = filename}
    end;

(* ------------------------------------------------------------------------- *)
(* The pure package checksums.                                               *)
(* ------------------------------------------------------------------------- *)

datatype packages = Packages of Checksum.checksum PackageNameMap.map;

val emptyPackages = Packages (PackageNameMap.new ());

fun peekPackages (Packages m) = PackageNameMap.peek m;

fun memberPackages n (Packages m) = PackageNameMap.inDomain n m;

fun insertPackages pc (n,c) =
    if memberPackages n pc then
      raise Error ("multiple entries for package " ^ PackageName.toString n)
    else
      let
        val Packages m = pc

        val m = PackageNameMap.insert m (n,c)
      in
        Packages m
      end;

fun uncurriedInsertPackages (n_c,pc) = insertPackages pc n_c;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;
in
  val parserPackageChecksum =
      (PackageName.parser ++
       exactChar #" " ++
       Checksum.parser ++
       exactChar #"\n") >>
      (fn (n,((),(c,()))) => [(n,c)]);
end;

fun fromTextFilePackages {filename} =
    let
      (* Estimating parse error line numbers *)

      val lines = Stream.fromTextFile {filename = filename}

      val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
    in
      (let
         (* The character stream *)

         val chars = Parse.everything Parse.any chars

         (* The package stream *)

         val pkgs = Parse.everything parserPackageChecksum chars
       in
         Stream.foldl uncurriedInsertPackages emptyPackages pkgs
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in repo file \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

(* ------------------------------------------------------------------------- *)
(* A type of package directory checkums.                                     *)
(* ------------------------------------------------------------------------- *)

datatype checksums =
    Checksums of
      {filename : string,
       packages : packages option ref};

fun mk {filename} =
    let
      val packages = ref NONE
    in
      Checksums
        {filename = filename,
         packages = packages}
    end;

fun filename (Checksums {filename = x, ...}) = {filename = x};

fun packages chks =
    let
      val Checksums {packages, ...} = chks

      val ref pkgs = packages
    in
      case pkgs of
        SOME pc => pc
      | NONE =>
        let
          val pc = fromTextFilePackages (filename chks)

          val () = packages := SOME pc
        in
          pc
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

fun peek chks n = peekPackages (packages chks) n;

fun member chks n = memberPackages n (packages chks);

end
