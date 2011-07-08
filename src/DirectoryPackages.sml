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
    PurePackages of PackageInfo.info PackageNameVersionMap.map;

val emptyPure = PurePackages (PackageNameVersionMap.new ());

fun sizePure (PurePackages pkgs) = PackageNameVersionMap.size pkgs;

fun peekPure (PurePackages pkgs) namever =
    PackageNameVersionMap.peek pkgs namever;

fun memberPure namever (PurePackages pkgs) =
    PackageNameVersionMap.inDomain namever pkgs;

fun toNameVersionSetPure (PurePackages pkgs) =
    PackageNameVersionSet.domain pkgs;

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

val dependencyPure =
    let
      fun add (info,dep) = PackageDependency.addInfo dep info
    in
      foldlPure add PackageDependency.empty
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
       dependency : PackageDependency.dependency option ref,
       checksums : DirectoryChecksums.checksums};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk {system = sys, rootDirectory = rootDir} =
    let
      val {directory} =
          DirectoryPath.mkPackagesDirectory {rootDirectory = rootDir}

      val packages = ref NONE

      val dependency = ref NONE

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
         dependency = dependency,
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

fun dependency pkgs =
    let
      val Packages {dependency = rox, ...} = pkgs

      val ref ox = rox
    in
      case ox of
        SOME x => x
      | NONE =>
        let
          val x = dependencyPure (packages pkgs)

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

fun list pkgs = toNameVersionSetPure (packages pkgs);

(* ------------------------------------------------------------------------- *)
(* Looking up the latest version of packages.                                *)
(* ------------------------------------------------------------------------- *)

fun latestVersion pkgs nv =
    let
      val n = PackageNameVersion.name nv
    in
      case PackageNameVersionSet.latestVersion (list pkgs) n of
        SOME nv => PackageNameVersionSet.singleton nv
      | NONE => PackageNameVersionSet.empty
    end;

fun isLatestVersion pkgs nv =
    let
      val nvs = latestVersion pkgs nv
    in
      PackageNameVersionSet.member nv nvs
    end;

(* ------------------------------------------------------------------------- *)
(* Dependencies in the installed packages.                                   *)
(* ------------------------------------------------------------------------- *)

fun parents pkgs namever =
    let
(*OpenTheoryDebug
      val _ = member namever pkgs orelse
              raise Bug "DirectoryPackages.parents: unknown package"
*)
      val Packages {dependency = ref odep, ...} = pkgs
    in
      case odep of
        SOME dep => PackageDependency.parents dep namever
      | NONE => PackageInfo.packages (get pkgs namever)
    end;

fun children pkgs namever =
    let
(*OpenTheoryDebug
      val _ = member namever pkgs orelse
              raise Bug "DirectoryPackages.children: unknown package"
*)
    in
      PackageDependency.children (dependency pkgs) namever
    end;

fun ancestorsSet pkgs =
    PackageNameVersionSet.close (parents pkgs);

fun ancestors pkgs namever =
    ancestorsSet pkgs (parents pkgs namever);

fun descendentsSet pkgs =
    PackageNameVersionSet.close (children pkgs);

fun descendents pkgs namever =
    descendentsSet pkgs (children pkgs namever);

(* Auxiliary packages *)

fun auxiliaryParents pkgs namever =
    let
      fun isAuxiliary par =
          PackageNameVersion.isStrictPrefixName namever par

      val pars = parents pkgs namever
    in
      PackageNameVersionSet.filter isAuxiliary pars
    end;

fun auxiliaryChildren pkgs namever =
    let
      fun isAuxiliary chil =
          PackageNameVersion.isStrictPrefixName chil namever

      val chils = children pkgs namever
    in
      PackageNameVersionSet.filter isAuxiliary chils
    end;

fun auxiliaryAncestorsSet pkgs =
    PackageNameVersionSet.close (auxiliaryParents pkgs);

fun auxiliaryAncestors pkgs namever =
    auxiliaryAncestorsSet pkgs (auxiliaryParents pkgs namever);

fun auxiliaryDescendentsSet pkgs =
    PackageNameVersionSet.close (auxiliaryChildren pkgs);

fun auxiliaryDescendents pkgs namever =
    auxiliaryDescendentsSet pkgs (auxiliaryChildren pkgs namever);

fun isAuxiliary pkgs namever =
    not (PackageNameVersionSet.null (auxiliaryChildren pkgs namever));

(* ------------------------------------------------------------------------- *)
(* Arranging packages in installation order.                                 *)
(* ------------------------------------------------------------------------- *)

fun installOrder pkgs = PackageNameVersionSet.postOrder (parents pkgs);

fun installOrdered pkgs = PackageNameVersionSet.postOrdered (parents pkgs);

(* ------------------------------------------------------------------------- *)
(* Adding a new package.                                                     *)
(* ------------------------------------------------------------------------- *)

fun add pkgs info chk =
    let
      val Packages
            {system = _,
             directory = _,
             packages = rop,
             dependency = rod,
             checksums = chks} = pkgs

      val () =
          case !rop of
            NONE => ()
          | SOME p => rop := SOME (addPure p info)

      val () =
          case !rod of
            NONE => ()
          | SOME d => rod := SOME (PackageDependency.addInfo d info)

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
             dependency = rod,
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
