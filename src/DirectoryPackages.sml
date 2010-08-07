(* ========================================================================= *)
(* INSTALLED PACKAGE DIRECTORY                                               *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryPackages :> DirectoryPackages =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of package dependency graphs.                                      *)
(* ------------------------------------------------------------------------- *)

datatype packageDeps =
    PackageDeps of
      {parents : PackageNameSet.set PackageNameMap.map,
       children : PackageNameSet.set PackageNameMap.map};

val emptyPackageDeps =
    let
      val parents = PackageNameMap.new ()
      and children = PackageNameMap.new ()
    in
      PackageDeps
        {parents = parents,
         children = children}
    end;

fun parentsPackageDeps (PackageDeps {parents,...}) name =
    case PackageNameMap.peek parents name of
      SOME ps => ps
    | NONE => PackageNameSet.empty;

fun childrenPackageDeps (PackageDeps {children,...}) name =
    case PackageNameMap.peek children name of
      SOME cs => cs
    | NONE => PackageNameSet.empty;

fun ancestorsSetPackageDeps deps =
    PackageNameSet.close (parentsPackageDeps deps);

fun ancestorsPackageDeps deps name =
    ancestorsSetPackageDeps deps (parentsPackageDeps deps name);

fun descendentsSetPackageDeps deps =
    PackageNameSet.close (childrenPackageDeps deps);

fun descendentsPackageDeps deps name =
    descendentsSetPackageDeps deps (childrenPackageDeps deps name);

fun addPackageDeps deps (p,c) =
    let
      val ps = parentsPackageDeps deps c
      and cs = childrenPackageDeps deps p

      val PackageDeps {parents,children} = deps

      val parents =
          if PackageNameSet.member p ps then parents
          else PackageNameMap.insert parents (c, PackageNameSet.add ps p)

      val children =
          if PackageNameSet.member c cs then children
          else PackageNameMap.insert children (p, PackageNameSet.add cs c)
    in
      PackageDeps
        {parents = parents,
         children = children}
    end;

fun addInfoPackageDeps deps info =
    let
      val name = PackageInfo.name info
      and pars = PackageInfo.packages info
    in
      PackageNameSet.foldl (fn (p,d) => addPackageDeps d (p,name)) deps pars
    end;

fun installOrderPackageDeps deps =
    PackageNameSet.postOrder (parentsPackageDeps deps);

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
  fun toNameSetPure (PurePackages pkgs) =
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

val packageDepsPure =
    let
      fun add (info,deps) = addInfoPackageDeps deps info
    in
      foldlPure add emptyPackageDeps
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
       dependencies : packageDeps option ref,
       checksums : DirectoryChecksums.checksums};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {directory,filename} =
    let
      val packages = ref NONE

      val dependencies = ref NONE

      val checksums = DirectoryChecksums.mk {filename = filename}
    in
      Packages
        {directory = directory,
         packages = packages,
         dependencies = dependencies,
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

fun dependencies pkgs =
    let
      val Packages {dependencies = rox, ...} = pkgs

      val ref ox = rox
    in
      case ox of
        SOME x => x
      | NONE =>
        let
          val x = packageDepsPure (packages pkgs)

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

fun get pkgs name =
    case peek pkgs name of
      SOME info => info
    | NONE => raise Error "DirectoryPackages.get";

fun member name pkgs = memberPure name (packages pkgs);

(* ------------------------------------------------------------------------- *)
(* All installed packages.                                                   *)
(* ------------------------------------------------------------------------- *)

fun list pkgs = toNameSetPure (packages pkgs);

(* ------------------------------------------------------------------------- *)
(* Dependencies in the installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

fun parents' pkgs =
    let
      val Packages {dependencies = ref odeps, ...} = pkgs
    in
      case odeps of
        SOME deps => parentsPackageDeps deps
      | NONE => fn name => PackageInfo.packages (get pkgs name)
    end;

fun parents pkgs name =
    let
(*OpenTheoryDebug
      val _ = member name pkgs orelse
              raise Bug "DirectoryPackages.parents: unknown package"
*)
    in
      parents' pkgs name
    end;

fun children' pkgs = childrenPackageDeps (dependencies pkgs);

fun children pkgs name =
    let
(*OpenTheoryDebug
      val _ = member name pkgs orelse
              raise Bug "DirectoryPackages.children: unknown package"
*)
    in
      children' pkgs name
    end;

fun ancestorsSet pkgs = PackageNameSet.close (parents' pkgs);

fun ancestors pkgs name =
    let
(*OpenTheoryDebug
      val _ = member name pkgs orelse
              raise Bug "DirectoryPackages.ancestors: unknown package"
*)
    in
      ancestorsSet pkgs (parents' pkgs name)
    end;

fun descendentsSet pkgs = PackageNameSet.close (children' pkgs);

fun descendents pkgs name =
    let
(*OpenTheoryDebug
      val _ = member name pkgs orelse
              raise Bug "DirectoryPackages.descendents: unknown package"
*)
    in
      descendentsSet pkgs (children' pkgs name)
    end;

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

fun installOrder pkgs = PackageNameSet.postOrder (parents' pkgs);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap size (Print.ppBracket "{" "}" Print.ppInt);

val toString = Print.toString pp;

end
