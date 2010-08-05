(* ========================================================================= *)
(* PACKAGE DIRECTORY CONFIG FILE                                             *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure DirectoryConfig :> DirectoryConfig =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val nameRepoSectionKey = "name"
and openTheoryRepoName = "gilith"
and openTheoryRepoUrl = "http://opentheory.gilith.com/"
and repoConfigSection = "repo"
and urlRepoSectionKey = "url";

(* ------------------------------------------------------------------------- *)
(* Repo configuration.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype repo =
    Repo of
      {name : string,
       url : string};

val openTheoryRepo =
    Repo
      {name = openTheoryRepoName,
       url = openTheoryRepoUrl};

(* ------------------------------------------------------------------------- *)
(* A type of configuration data.                                             *)
(* ------------------------------------------------------------------------- *)

datatype config =
    Config of
      {repos : repo list};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val repos = []
    in
      Config {repos = repos}
    end;

fun repos (Config {repos = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Basic operations.                                                         *)
(* ------------------------------------------------------------------------- *)

fun addRepo cfg r =
    let
      val Config {repos = rs} = cfg

      val rs = rs @ [r]
    in
      Config {repos = rs}
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun toRepoSection repo =
      let
        val Repo {name,url} = repo
      in
        Config.Section
          {name = repoConfigSection,
           keyValues =
             [Config.KeyValue
                {key = nameRepoSectionKey,
                 value = name},
              Config.KeyValue
                {key = urlRepoSectionKey,
                 value = url}]}
      end;

  fun toSections config =
      let
        val Config {repos = rs} = config

        val sections = map toRepoSection rs
      in
        Config.Config {sections = sections}
      end;
in
  val pp = Print.ppMap toSections Config.pp;
end;

(* ------------------------------------------------------------------------- *)
(* I/O.                                                                      *)
(* ------------------------------------------------------------------------- *)

local
  local
    datatype repoSectionState =
        RepoSectionState of
          {name : string option,
           url : string option};

    val initialRepoSectionState =
        let
          val name = NONE
          and url = NONE
        in
          RepoSectionState
            {name = name,
             url = url}
        end;

    fun addNameRepoSectionState n state =
        let
          val RepoSectionState {name,url} = state

          val name =
              case name of
                NONE => SOME n
              | SOME n' =>
                raise Error ("duplicate \"" ^ nameRepoSectionKey ^
                             "\" keys: " ^ n ^ " and " ^ n')
        in
          RepoSectionState
            {name = name,
             url = url}
        end;

    fun addUrlRepoSectionState u state =
        let
          val RepoSectionState {name,url} = state

          val url =
              case url of
                NONE => SOME u
              | SOME u' =>
                raise Error ("duplicate \"" ^ urlRepoSectionKey ^
                             "\" keys: " ^ u ^ " and " ^ u')
        in
          RepoSectionState
            {name = name,
             url = url}
        end;

    fun processRepoSectionState (kv,state) =
        let
          val Config.KeyValue {key,value} = kv
        in
          if key = nameRepoSectionKey then
            addNameRepoSectionState value state
          else if key = urlRepoSectionKey then
            addUrlRepoSectionState value state
          else
            raise Error ("unknown key \"" ^ key ^ "\"")
        end;

    fun finalRepoSectionState state =
        let
          val RepoSectionState {name,url} = state

          val name =
              case name of
                SOME n => n
              | NONE =>
                raise Error ("missing \"" ^ nameRepoSectionKey ^ "\" key")

          val url =
              case url of
                SOME u => u
              | NONE =>
                raise Error ("missing \"" ^ urlRepoSectionKey ^ "\" key")
        in
          Repo
            {name = name,
             url = url}
        end;
  in
    fun fromRepoSection kvs =
        let
          val state = initialRepoSectionState

          val state = List.foldl processRepoSectionState state kvs
        in
          finalRepoSectionState state
        end
        handle Error err =>
          raise Error ("in \"" ^ repoConfigSection ^
                       "\" section of config file:\n" ^ err);
  end;

  fun addSection (sect,conf) =
      let
        val Config.Section {name,keyValues} = sect
      in
        if name = repoConfigSection then
          let
            val r = fromRepoSection keyValues
          in
            addRepo conf r
          end
        else
          raise Error ("unknown config section \"" ^ name ^ "\"")
      end;

  fun fromSections conf =
      let
        val Config.Config {sections} = conf
      in
        List.foldl addSection empty sections
      end;
in
  fun fromTextFile filename =
      let
        val conf =
            Config.fromTextFile filename
            handle IO.Io _ => Config.empty
      in
        fromSections conf
      end
(*OpenTheoryDebug
      handle Error err => raise Bug ("DirectoryConfig.fromTextFile: " ^ err);
*)
end;

fun toTextFile {config,filename} =
    let
      val s = Print.toStream pp config
    in
      Stream.toTextFile {filename = filename} s
    end;

(* ------------------------------------------------------------------------- *)
(* The default configuration.                                                *)
(* ------------------------------------------------------------------------- *)

val default =
    let
      val repos = [openTheoryRepo]
    in
      Config {repos = repos}
    end;

end
