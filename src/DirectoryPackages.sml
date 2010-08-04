(* ========================================================================= *)
(* INSTALLED PACKAGE DIRECTORY                                               *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryPackages :> DirectoryPackages =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A pure type of installed packages.                                        *)
(* ------------------------------------------------------------------------- *)

datatype purePackages =
    PurePackages of PackageInfo.info PackageNameMap.map;

val emptyPure = PurePackages (PackageNameMap.new ());

fun sizePure (PurePackages pkgs) = PackageNameMap.size pkgs;

fun peekPure (PurePackages pkgs) name = PackageNameMap.peek pkgs name;

fun memberPure name (PurePackages pkgs) = PackageNameMap.inDomain name pkgs;

local
  fun add (name,_,acc) = PackageNameSet.add acc name;
in
  fun listPure (PurePackages pkgs) =
      PackageNameMap.foldl add PackageNameSet.empty pkgs;
end;

fun appPure f =
    let
      fun f' (_,info) = f info
    in
      fn PurePackages pkgs => PackageNameMap.app f' pkgs
    end;

fun foldlPure f =
    let
      fun f' (_,info,acc) = f (info,acc)
    in
      fn acc => fn PurePackages pkgs => PackageNameMap.foldl f' acc pkgs
    end;

fun addPure (PurePackages pkgs) info =
    let
      val name = PackageInfo.name info

      val pkgs = PackageNameMap.insert pkgs (name,info)
    in
      PurePackages pkgs
    end;

fun deletePure (PurePackages pkgs) name =
    let
      val pkgs = PackageNameMap.delete pkgs name
    in
      PurePackages pkgs
    end;

fun removePure (PurePackages pkgs) name =
    let
      val pkgs = PackageNameMap.remove pkgs name
    in
      PurePackages pkgs
    end;

local
  fun add ({filename},pkgs) =
      let
        val name = PackageName.fromString (OS.Path.file filename)

        val info = PackageInfo.mk {name = name, directory = filename}
      in
        addPure pkgs info
      end;
in
  fun fromDirectoryPure dir =
      let
        val dirs = readDirectory dir
      in
        List.foldl add emptyPure dirs
      end;
end;

(* ------------------------------------------------------------------------- *)
(* A type of installed packages.                                             *)
(* ------------------------------------------------------------------------- *)

datatype packages =
    Packages of
      {directory : string,
       packages : purePackages option ref,
       checksums : DirectoryChecksums.checksums};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {directory,filename} =
    let
      val packages = ref NONE

      val checksums = DirectoryChecksums.mk {filename = filename}
    in
      Packages
        {directory = directory,
         packages = packages,
         checksums = checksums}
    end;

fun directory (Packages {directory = x, ...}) = {directory = x};

fun packages pkgs =
    let
      val Packages {packages = rox, ...} = pkgs

      val ref ox = rox
    in
      case ox of
        SOME x => x
      | NONE =>
        let
          val x = fromDirectoryPure (directory pkgs)

          val () = rox := SOME x
        in
          x
        end
    end;

fun checksums (Packages {checksums = x, ...}) = x;

fun filename pkgs = DirectoryChecksums.filename (checksums pkgs);

fun size pkgs = sizePure (packages pkgs);

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

fun peek pkgs name = peekPure (packages pkgs) name;

fun member name pkgs = memberPure name (packages pkgs);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap size (Print.ppBracket "{" "}" Print.ppInt);

val toString = Print.toString pp;

end
