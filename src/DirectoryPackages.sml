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
      {parents : PackageNameVersionSet.set PackageNameVersionMap.map,
       children : PackageNameVersionSet.set PackageNameVersionMap.map};

val emptyPackageDeps =
    let
      val parents = PackageNameVersionMap.new ()
      and children = PackageNameVersionMap.new ()
    in
      PackageDeps
        {parents = parents,
         children = children}
    end;

fun parentsPackageDeps (PackageDeps {parents,...}) namever =
    case PackageNameVersionMap.peek parents namever of
      SOME ps => ps
    | NONE => PackageNameVersionSet.empty;

fun childrenPackageDeps (PackageDeps {children,...}) namever =
    case PackageNameVersionMap.peek children namever of
      SOME cs => cs
    | NONE => PackageNameVersionSet.empty;

fun ancestorsSetPackageDeps deps =
    PackageNameVersionSet.close (parentsPackageDeps deps);

fun ancestorsPackageDeps deps namever =
    ancestorsSetPackageDeps deps (parentsPackageDeps deps namever);

fun descendentsSetPackageDeps deps =
    PackageNameVersionSet.close (childrenPackageDeps deps);

fun descendentsPackageDeps deps namever =
    descendentsSetPackageDeps deps (childrenPackageDeps deps namever);

fun addPackageDeps deps (p,c) =
    let
      val ps = parentsPackageDeps deps c
      and cs = childrenPackageDeps deps p

      val PackageDeps {parents,children} = deps

      val parents =
          if PackageNameVersionSet.member p ps then parents
          else
            let
              val ps = PackageNameVersionSet.add ps p
            in
              PackageNameVersionMap.insert parents (c,ps)
            end

      val children =
          if PackageNameVersionSet.member c cs then children
          else
            let
              val cs = PackageNameVersionSet.add cs c
            in
              PackageNameVersionMap.insert children (p,cs)
            end
    in
      PackageDeps
        {parents = parents,
         children = children}
    end;

fun addInfoPackageDeps deps info =
    let
      val namever = PackageInfo.nameVersion info
      and pars = PackageInfo.packages info

      fun add (p,d) = addPackageDeps d (p,namever)
    in
      PackageNameVersionSet.foldl add deps pars
    end;

fun installOrderPackageDeps deps =
    PackageNameVersionSet.postOrder (parentsPackageDeps deps);

(* ------------------------------------------------------------------------- *)
(* A pure type of installed packages.                                        *)
(* ------------------------------------------------------------------------- *)

datatype purePackages =
    PurePackages of PackageInfo.info PackageNameVersionMap.map;

val emptyPure = PurePackages (PackageNameVersionMap.new ());

fun sizePure (PurePackages pkgs) = PackageNameVersionMap.size pkgs;

fun peekPure (PurePackages pkgs) namever =
    PackageNameVersionMap.peek pkgs namever;

fun memberPure namever (PurePackages pkgs) =
    PackageNameVersionMap.inDomain namever pkgs;

local
  fun add (namever,_,acc) = PackageNameVersionSet.add acc namever;
in
  fun toNameSetPure (PurePackages pkgs) =
      PackageNameVersionMap.foldl add PackageNameVersionSet.empty pkgs;
end;

fun appPure f =
    let
      fun f' (_,info) = f info
    in
      fn PurePackages pkgs => PackageNameVersionMap.app f' pkgs
    end;

fun foldlPure f =
    let
      fun f' (_,info,acc) = f (info,acc)
    in
      fn acc => fn PurePackages pkgs => PackageNameVersionMap.foldl f' acc pkgs
    end;

fun addPure (PurePackages pkgs) info =
    let
      val namever = PackageInfo.nameVersion info

      val pkgs = PackageNameVersionMap.insert pkgs (namever,info)
    in
      PurePackages pkgs
    end;

fun deletePure (PurePackages pkgs) namever =
    let
      val pkgs = PackageNameVersionMap.delete pkgs namever
    in
      PurePackages pkgs
    end;

fun removePure (PurePackages pkgs) namever =
    let
      val pkgs = PackageNameVersionMap.remove pkgs namever
    in
      PurePackages pkgs
    end;

val packageDepsPure =
    let
      fun add (info,deps) = addInfoPackageDeps deps info
    in
      foldlPure add emptyPackageDeps
    end;

fun fromDirectoryPure sys =
    let
      fun add ({filename},pkgs) =
          let
            val namever = PackageNameVersion.fromString (OS.Path.file filename)

            val info =
                PackageInfo.mk
                  {system = sys,
                   nameVersion = namever,
                   directory = filename}
          in
            addPure pkgs info
          end
    in
      fn dir => List.foldl add emptyPure (readDirectory dir)
    end;

(* ------------------------------------------------------------------------- *)
(* A type of installed packages.                                             *)
(* ------------------------------------------------------------------------- *)

datatype packages =
    Packages of
      {system : DirectorySystem.system,
       directory : string,
       packages : purePackages option ref,
       dependencies : packageDeps option ref,
       checksums : DirectoryChecksums.checksums};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {system = sys, rootDirectory = rootDir} =
    let
      val {directory} =
          DirectoryPath.mkPackagesDirectory {rootDirectory = rootDir}

      val packages = ref NONE

      val dependencies = ref NONE

      val checksums =
          let
            val {filename} =
                DirectoryPath.mkInstalledFilename {rootDirectory = rootDir}
          in
            DirectoryChecksums.mk
              {system = sys,
               filename = filename,
               updateFrom = NONE}
          end
    in
      Packages
        {system = sys,
         directory = directory,
         packages = packages,
         dependencies = dependencies,
         checksums = checksums}
    end;

fun directory (Packages {directory = x, ...}) = {directory = x};

fun packages pkgs =
    let
      val Packages {system = sys, directory = dir, packages = rox, ...} = pkgs

      val ref ox = rox
    in
      case ox of
        SOME x => x
      | NONE =>
        let
          val x = fromDirectoryPure sys {directory = dir}

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

fun size pkgs = sizePure (packages pkgs);

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

fun peek pkgs namever = peekPure (packages pkgs) namever;

fun get pkgs namever =
    case peek pkgs namever of
      SOME info => info
    | NONE => raise Error "DirectoryPackages.get";

fun member namever pkgs = memberPure namever (packages pkgs);

fun checksum pkgs namever = DirectoryChecksums.peek (checksums pkgs) namever;

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
      | NONE => fn namever => PackageInfo.packages (get pkgs namever)
    end;

fun parents pkgs namever =
    let
(*OpenTheoryDebug
      val _ = member namever pkgs orelse
              raise Bug "DirectoryPackages.parents: unknown package"
*)
    in
      parents' pkgs namever
    end;

fun children' pkgs = childrenPackageDeps (dependencies pkgs);

fun children pkgs namever =
    let
(*OpenTheoryDebug
      val _ = member namever pkgs orelse
              raise Bug "DirectoryPackages.children: unknown package"
*)
    in
      children' pkgs namever
    end;

fun ancestorsSet pkgs = PackageNameVersionSet.close (parents' pkgs);

fun ancestors pkgs namever =
    let
(*OpenTheoryDebug
      val _ = member namever pkgs orelse
              raise Bug "DirectoryPackages.ancestors: unknown package"
*)
    in
      ancestorsSet pkgs (parents' pkgs namever)
    end;

fun descendentsSet pkgs = PackageNameVersionSet.close (children' pkgs);

fun descendents pkgs namever =
    let
(*OpenTheoryDebug
      val _ = member namever pkgs orelse
              raise Bug "DirectoryPackages.descendents: unknown package"
*)
    in
      descendentsSet pkgs (children' pkgs namever)
    end;

(* ------------------------------------------------------------------------- *)
(* Generate a valid installation order.                                      *)
(* ------------------------------------------------------------------------- *)

fun installOrder pkgs = PackageNameVersionSet.postOrder (parents' pkgs);

(* ------------------------------------------------------------------------- *)
(* Adding a new package.                                                     *)
(* ------------------------------------------------------------------------- *)

fun add pkgs info chk =
    let
      val Packages
            {system = _,
             directory = _,
             packages = rop,
             dependencies = rod,
             checksums = chks} = pkgs

      val () =
          case !rop of
            NONE => ()
          | SOME p => rop := SOME (addPure p info)

      val () =
          case !rod of
            NONE => ()
          | SOME d => rod := SOME (addInfoPackageDeps d info)

      val () =
          let
            val nv = PackageInfo.nameVersion info
          in
            DirectoryChecksums.add chks (nv,chk)
          end
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Deleting a package.                                                       *)
(* ------------------------------------------------------------------------- *)

fun delete pkgs namever =
    let
      val Packages
            {system = _,
             directory = _,
             packages = rop,
             dependencies = rod,
             checksums = chks} = pkgs

      val () =
          case !rop of
            NONE => ()
          | SOME p => rop := SOME (deletePure p namever)

      val () =
          case !rod of
            NONE => ()
          | SOME _ => rod := NONE;

      val () = DirectoryChecksums.delete chks namever
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap size (Print.ppBracket "{" "}" Print.ppInt);

val toString = Print.toString pp;

end
