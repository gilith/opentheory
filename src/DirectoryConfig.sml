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

val cpSystemDefault = "cp"
and cpSystemKey = "cp"
and curlSystemDefault = "curl --silent --show-error --user-agent opentheory"
and curlSystemKey = "curl"
and echoSystemDefault = "echo"
and echoSystemKey = "echo"
and nameRepoKey = "name"
and nameRepoOpenTheory = "gilith"
and repoSection = "repo"
and shaSystemDefault = "sha1sum --binary"
and shaSystemKey = "sha"
and systemSection = "system"
and tarSystemDefault = "tar"
and tarSystemKey = "tar"
and touchSystemDefault = "touch"
and touchSystemKey = "touch"
and urlRepoKey = "url"
and urlRepoOpenTheory = "http://opentheory.gilith.com/";

(* ------------------------------------------------------------------------- *)
(* A type of repo configuration data.                                        *)
(* ------------------------------------------------------------------------- *)

datatype repo =
    Repo of
      {name : string,
       url : string};

fun nameRepo (Repo {name = x, ...}) = {name = x};

fun urlRepo (Repo {url = x, ...}) = {url = x};

fun findRepo repos {name = n} =
    let
      fun pred repo =
          let
            val {name = n'} = nameRepo repo
          in
            n' = n
          end
    in
      case List.filter pred repos of
        [] => NONE
      | [repo] => SOME repo
      | _ :: _ :: _ => raise Bug "multiple repos with the same name"
    end;

fun toSectionRepo repo =
    let
      val Repo {name,url} = repo
    in
      Config.Section
        {name = repoSection,
         keyValues =
           [Config.KeyValue
              {key = nameRepoKey,
               value = name},
            Config.KeyValue
              {key = urlRepoKey,
               value = url}]}
    end;

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
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = nameRepoKey} ^
                    " keys: " ^ n ^ " and " ^ n'
              in
                raise Error err
              end
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
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = urlRepoKey} ^
                    " keys: " ^ u ^ " and " ^ u'
              in
                raise Error err
              end
      in
        RepoSectionState
          {name = name,
           url = url}
      end;

  fun processRepoSectionState (kv,state) =
      let
        val Config.KeyValue {key,value} = kv
      in
        if key = nameRepoKey then addNameRepoSectionState value state
        else if key = urlRepoKey then addUrlRepoSectionState value state
        else raise Error ("unknown key: " ^ Config.toStringKey {key = key})
      end;

  fun finalRepoSectionState state =
      let
        val RepoSectionState {name,url} = state

        val name =
            case name of
              SOME n => n
            | NONE =>
              let
                val err =
                    "missing " ^ Config.toStringKey {key = nameRepoKey} ^ " key"
              in
                raise Error err
              end

        val url =
            case url of
              SOME u => u
            | NONE =>
              let
                val err =
                    "missing " ^ Config.toStringKey {key = urlRepoKey} ^ " key"
              in
                raise Error err
              end
      in
        Repo
          {name = name,
           url = url}
      end;
in
  fun fromSectionRepo kvs =
      let
        val state = initialRepoSectionState

        val state = List.foldl processRepoSectionState state kvs
      in
        finalRepoSectionState state
      end
      handle Error err =>
        let
          val err =
              "in section " ^
              Config.toStringSectionName {name = repoSection} ^
              " of config file:\n" ^ err
        in
          raise Error err
        end;
end;

val openTheoryRepo =
    Repo
      {name = nameRepoOpenTheory,
       url = urlRepoOpenTheory};

val defaultRepos = [openTheoryRepo];

(* ------------------------------------------------------------------------- *)
(* A type of system configuration data.                                      *)
(* ------------------------------------------------------------------------- *)

fun toSectionSystem sys =
    let
      val {cp,curl,echo,sha,tar,touch} = DirectorySystem.dest sys
    in
      Config.Section
        {name = systemSection,
         keyValues =
           [Config.KeyValue
              {key = cpSystemKey,
               value = cp},
            Config.KeyValue
              {key = curlSystemKey,
               value = curl},
            Config.KeyValue
              {key = echoSystemKey,
               value = echo},
            Config.KeyValue
              {key = shaSystemKey,
               value = sha},
            Config.KeyValue
              {key = tarSystemKey,
               value = tar},
            Config.KeyValue
              {key = touchSystemKey,
               value = touch}]}
    end;

local
  datatype systemSectionState =
      SystemSectionState of
        {cp : string option,
         curl : string option,
         echo : string option,
         sha : string option,
         tar : string option,
         touch : string option};

  val initialSystemSectionState =
      let
        val cp = NONE
        and curl = NONE
        and echo = NONE
        and sha = NONE
        and tar = NONE
        and touch = NONE
      in
        SystemSectionState
          {cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar,
           touch = touch}
      end;

  fun addCpSystemSectionState x state =
      let
        val SystemSectionState {cp,curl,echo,sha,tar,touch} = state

        val cp =
            case cp of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = cpSystemKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        SystemSectionState
          {cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar,
           touch = touch}
      end;

  fun addCurlSystemSectionState x state =
      let
        val SystemSectionState {cp,curl,echo,sha,tar,touch} = state

        val curl =
            case curl of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = curlSystemKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        SystemSectionState
          {cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar,
           touch = touch}
      end;

  fun addEchoSystemSectionState x state =
      let
        val SystemSectionState {cp,curl,echo,sha,tar,touch} = state

        val echo =
            case echo of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = echoSystemKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        SystemSectionState
          {cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar,
           touch = touch}
      end;

  fun addShaSystemSectionState x state =
      let
        val SystemSectionState {cp,curl,echo,sha,tar,touch} = state

        val sha =
            case sha of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = shaSystemKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        SystemSectionState
          {cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar,
           touch = touch}
      end;

  fun addTarSystemSectionState x state =
      let
        val SystemSectionState {cp,curl,echo,sha,tar,touch} = state

        val tar =
            case tar of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = tarSystemKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        SystemSectionState
          {cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar,
           touch = touch}
      end;

  fun addTouchSystemSectionState x state =
      let
        val SystemSectionState {cp,curl,echo,sha,tar,touch} = state

        val touch =
            case touch of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = touchSystemKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        SystemSectionState
          {cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar,
           touch = touch}
      end;

  fun processSystemSectionState (kv,state) =
      let
        val Config.KeyValue {key,value} = kv
      in
        if key = cpSystemKey then addCpSystemSectionState value state
        else if key = curlSystemKey then addCurlSystemSectionState value state
        else if key = echoSystemKey then addEchoSystemSectionState value state
        else if key = shaSystemKey then addShaSystemSectionState value state
        else if key = tarSystemKey then addTarSystemSectionState value state
        else if key = touchSystemKey then addTouchSystemSectionState value state
        else raise Error ("unknown key: " ^ Config.toStringKey {key = key})
      end;

  fun finalSystemSectionState sys state =
      let
        val SystemSectionState {cp,curl,echo,sha,tar,touch} = state

        val cp =
            case cp of
              SOME x => x
            | NONE => let val {cp = x} = DirectorySystem.cp sys in x end

        val curl =
            case curl of
              SOME x => x
            | NONE => let val {curl = x} = DirectorySystem.curl sys in x end

        val echo =
            case echo of
              SOME x => x
            | NONE => let val {echo = x} = DirectorySystem.echo sys in x end

        val sha =
            case sha of
              SOME x => x
            | NONE => let val {sha = x} = DirectorySystem.sha sys in x end

        val tar =
            case tar of
              SOME x => x
            | NONE => let val {tar = x} = DirectorySystem.tar sys in x end

        val touch =
            case touch of
              SOME x => x
            | NONE => let val {touch = x} = DirectorySystem.touch sys in x end
      in
        DirectorySystem.mk
          {cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar,
           touch = touch}
      end;
in
  fun fromSectionSystem sys kvs =
      let
        val state = initialSystemSectionState

        val state = List.foldl processSystemSectionState state kvs
      in
        finalSystemSectionState sys state
      end
      handle Error err =>
        let
          val err =
              "in section " ^
              Config.toStringSectionName {name = systemSection} ^
              " of config file:\n" ^ err
        in
          raise Error err
        end;
end;

val defaultSystem =
    DirectorySystem.mk
      {cp = cpSystemDefault,
       curl = curlSystemDefault,
       echo = echoSystemDefault,
       sha = shaSystemDefault,
       tar = tarSystemDefault,
       touch = touchSystemDefault};

(* ------------------------------------------------------------------------- *)
(* A type of configuration data.                                             *)
(* ------------------------------------------------------------------------- *)

datatype config =
    Config of
      {repos : repo list,
       system : DirectorySystem.system};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val rs = []
      and sys = defaultSystem
    in
      Config {repos = rs, system = sys}
    end;

fun repos (Config {repos = x, ...}) = x;

fun system (Config {system = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Basic operations.                                                         *)
(* ------------------------------------------------------------------------- *)

fun addRepo cfg r =
    let
      val Config {repos = rs, system = sys} = cfg

      val rs = rs @ [r]
    in
      Config {repos = rs, system = sys}
    end;

fun replaceSystem cfg sys =
    let
      val Config {repos = rs, system = _} = cfg
    in
      Config {repos = rs, system = sys}
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun toSections config =
    let
      val Config {repos = rs, system = sys} = config

      val sections = map toSectionRepo rs @ [toSectionSystem sys]
    in
      Config.Config {sections = sections}
    end;

val pp = Print.ppMap toSections Config.pp;

(* ------------------------------------------------------------------------- *)
(* I/O.                                                                      *)
(* ------------------------------------------------------------------------- *)

local
  fun addSection (sect,conf) =
      let
        val Config.Section {name,keyValues} = sect
      in
        if name = repoSection then
          let
            val r = fromSectionRepo keyValues
          in
            addRepo conf r
          end
        else if name = systemSection then
          let
            val sys = system conf

            val sys = fromSectionSystem sys keyValues
          in
            replaceSystem conf sys
          end
        else
          let
            val err =
                "unknown config section: " ^
                Config.toStringSectionName {name = name}
          in
            raise Error err
          end
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
      val rs = defaultRepos
      and sys = defaultSystem
    in
      Config {repos = rs, system = sys}
    end;

end
