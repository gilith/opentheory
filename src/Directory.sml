(* ========================================================================= *)
(* THEORY PACKAGE DIRECTORIES                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Directory :> Directory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Repos.                                                                    *)
(* ------------------------------------------------------------------------- *)

datatype repo =
    Repo of
      {name : string,
       pkgs : {filename : string} list option PackageNameMap.map ref};

fun mkRepo name =
    let
      val pkgs = ref (PackageNameMap.new ())
    in
      Repo
        {name = name,
         pkgs = pkgs}
    end;

fun filesRepo repo pkg =

(***
type repo

val containsRepo : repo -> PackageName.name -> bool

val filesRepo : repo -> PackageName.name -> {filename : string} list option
***)

(* ------------------------------------------------------------------------- *)
(* Configuration.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype config =
    Config of
      {repos : repo list};

(***
type config

val mkConfig : {filename : string} -> config

val reposConfig : config -> repo list
***)

(* ------------------------------------------------------------------------- *)
(* Packages.                                                                 *)
(* ------------------------------------------------------------------------- *)

(***
type package

val directoryPackage : package -> {directory : string}

val filenamePackage : package -> {filename : string}

val contentsPackage : package -> Package.package
***)

(* ------------------------------------------------------------------------- *)
(* A type of theory package directories.                                     *)
(* ------------------------------------------------------------------------- *)

datatype directory =
    Directory of
      {root : {directory : string},
       config : config Lazy.lazy};

(***
val mk : {root : {directory : string}} -> directory

val root : directory -> {directory : string}

val config : directory -> config

val repos : directory -> repo list

val lookup : directory -> PackageName.name -> package option
***)

end
