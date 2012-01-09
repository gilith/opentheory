(* ========================================================================= *)
(* INSTALLED PACKAGE DIRECTORY                                               *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryPackages :> DirectoryPackages =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of installed packages.                                             *)
(* ------------------------------------------------------------------------- *)

datatype installed =
    Installed of PackageInfo.info PackageNameVersionMap.map;

val emptyInstalled = Installed (PackageNameVersionMap.new ());

fun sizeInstalled (Installed pkgs) = PackageNameVersionMap.size pkgs;

fun peekInstalled (Installed pkgs) namever =
    PackageNameVersionMap.peek pkgs namever;

fun memberInstalled namever (Installed pkgs) =
    PackageNameVersionMap.inDomain namever pkgs;

fun toNameVersionSetInstalled (Installed pkgs) =
    PackageNameVersionSet.domain pkgs;

fun appInstalled f =
    let
      fun f' (_,info) = f info
    in
      fn Installed pkgs => PackageNameVersionMap.app f' pkgs
    end;

fun foldlInstalled f =
    let
      fun f' (_,info,acc) = f (info,acc)
    in
      fn acc => fn Installed pkgs => PackageNameVersionMap.foldl f' acc pkgs
    end;

fun addInstalled (Installed pkgs) namever_info =
    let
      val pkgs = PackageNameVersionMap.insert pkgs namever_info
    in
      Installed pkgs
    end;

fun deleteInstalled (Installed pkgs) namever =
    let
      val pkgs = PackageNameVersionMap.delete pkgs namever
    in
      Installed pkgs
    end;

(* ------------------------------------------------------------------------- *)
(* A type of package versions.                                               *)
(* ------------------------------------------------------------------------- *)

datatype versions =
    Versions of PackageVersionSet.set PackageNameMap.map;

val emptyVersions =
    let
      val vermap = PackageNameMap.new ()
    in
      Versions vermap
    end;

local
  fun totalPeek vermap name =
      case PackageNameMap.peek vermap name of
        SOME vers => vers
      | NONE => PackageVersionSet.empty;
in
  fun peekVersions (Versions vermap) name = totalPeek vermap name;

  fun addVersions (Versions vermap) namever =
      let
        val PackageNameVersion.NameVersion' {name,version} =
            PackageNameVersion.dest namever

        val vers = totalPeek vermap name

        val vers = PackageVersionSet.add vers version

        val vermap = PackageNameMap.insert vermap (name,vers)
      in
        Versions vermap
      end;
end;

fun deleteVersions (Versions vermap) namever =
    let
      val PackageNameVersion.NameVersion' {name,version} =
          PackageNameVersion.dest namever

      val vers =
          case PackageNameMap.peek vermap name of
            SOME vs => vs
          | NONE => raise Error "DirectoryPackages.deleteVersions"

      val vers = PackageVersionSet.delete vers version

      val vermap =
          if PackageVersionSet.null vers then
            PackageNameMap.delete vermap name
          else
            PackageNameMap.insert vermap (name,vers)
    in
      Versions vermap
    end;

(* ------------------------------------------------------------------------- *)
(* A pure type of installed packages.                                        *)
(* ------------------------------------------------------------------------- *)

datatype purePackages =
    PurePackages of
      {installed : installed,
       versions : versions};

fun installedPure (PurePackages {installed = x, ...}) = x;

fun versionsPure (PurePackages {versions = x, ...}) = x;

val emptyPure =
    let
      val installed = emptyInstalled
      and versions = emptyVersions
    in
      PurePackages
        {installed = installed,
         versions = versions}
    end;

fun sizePure pkgs = sizeInstalled (installedPure pkgs);

fun peekPure pkgs namever = peekInstalled (installedPure pkgs) namever;

fun memberPure namever pkgs = memberInstalled namever (installedPure pkgs);

fun toNameVersionSetPure pkgs = toNameVersionSetInstalled (installedPure pkgs);

fun appPure f pkgs = appInstalled f (installedPure pkgs);

fun foldlPure f b pkgs = foldlInstalled f b (installedPure pkgs);

fun addPure pkgs info =
    let
      val PurePackages {installed,versions} = pkgs

      val namever = PackageInfo.nameVersion info

      val installed = addInstalled installed (namever,info)

      val versions = addVersions versions namever
    in
      PurePackages
        {installed = installed,
         versions = versions}
    end;

fun deletePure pkgs namever =
    let
      val PurePackages {installed,versions} = pkgs

      val installed = deleteInstalled installed namever

      val versions = deleteVersions versions namever
    in
      PurePackages
        {installed = installed,
         versions = versions}
    end;

fun removePure pkgs namever =
    if memberPure namever pkgs then deletePure pkgs namever else pkgs;

val dependencyPure =
    let
      fun add (info,dep) = PackageDependency.addInfo dep info
    in
      foldlPure add PackageDependency.empty
    end;

fun nameVersionsPure pkgs name = peekVersions (versionsPure pkgs) name;

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
(* Package versions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun nameVersions pkgs name = nameVersionsPure (packages pkgs) name;

fun latestVersion pkgs name =
    PackageVersionSet.latestVersion (nameVersions pkgs name);

fun latestNameVersion pkgs name =
    case latestVersion pkgs name of
      NONE => NONE
    | SOME version =>
      let
        val namever' =
            PackageNameVersion.NameVersion'
              {name = name,
               version = version}
      in
        SOME (PackageNameVersion.mk namever')
      end;

fun isLatestNameVersion pkgs namever =
    let
      val PackageNameVersion.NameVersion' {name,version} =
          PackageNameVersion.dest namever
    in
      case latestVersion pkgs name of
        NONE => false
      | SOME ver => PackageVersion.equal ver version
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
