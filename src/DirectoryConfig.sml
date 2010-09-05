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

val chmodSystemDefault = "chmod"
and chmodSystemKey = "chmod"
and cleanupInstallDefault = Time.fromSeconds 3600  (* 1 hour *)
and cleanupInstallKey = "cleanup"
and cpSystemDefault = "cp"
and cpSystemKey = "cp"
and curlSystemDefault = "curl --silent --show-error --user-agent opentheory"
and curlSystemKey = "curl"
and echoSystemDefault = "echo"
and echoSystemKey = "echo"
and installSection = "install"
and nameRepoKey = "name"
and refreshRepoDefault = Time.fromSeconds 604800  (* 1 week *)
and refreshRepoKey = "refresh"
and repoSection = "repo"
and shaSystemDefault = "sha1sum --binary"
and shaSystemKey = "sha"
and systemSection = "system"
and tarSystemDefault = "tar"
and tarSystemKey = "tar"
and urlRepoKey = "url";

val nameRepoGilith = "gilith"
and refreshRepoGilith = refreshRepoDefault
and urlRepoGilith = "http://opentheory.gilith.com/";

(* ------------------------------------------------------------------------- *)
(* Time interval functions.                                                  *)
(* ------------------------------------------------------------------------- *)

fun toStringInterval t = Int.toString (Real.round (Time.toReal t));

fun fromStringInterval s =
    (case Int.fromString s of
       NONE => raise Error "not an integer"
     | SOME i =>
       if i >= 0 then Time.fromReal (Real.fromInt i)
       else raise Error "negative number")
    handle Error err => raise Error ("bad time interval format: " ^ err);

(* ------------------------------------------------------------------------- *)
(* A type of repo configuration data.                                        *)
(* ------------------------------------------------------------------------- *)

datatype repo =
    Repo of
      {name : string,
       url : string,
       refresh : Time.time};

fun nameRepo (Repo {name = x, ...}) = {name = x};

fun urlRepo (Repo {url = x, ...}) = {url = x};

fun refreshRepo (Repo {refresh = x, ...}) = x;

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
      val Repo {name,url,refresh} = repo
    in
      Config.Section
        {name = repoSection,
         keyValues =
           [Config.KeyValue
              {key = nameRepoKey,
               value = name},
            Config.KeyValue
              {key = urlRepoKey,
               value = url},
            Config.KeyValue
              {key = refreshRepoKey,
               value = toStringInterval refresh}]}
    end;

local
  datatype repoSectionState =
      RepoSectionState of
        {name : string option,
         url : string option,
         refresh : string option};

  val initialRepoSectionState =
      let
        val name = NONE
        and url = NONE
        and refresh = NONE
      in
        RepoSectionState
          {name = name,
           url = url,
           refresh = refresh}
      end;

  fun addNameRepoSectionState x state =
      let
        val RepoSectionState {name,url,refresh} = state

        val name =
            case name of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = nameRepoKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        RepoSectionState
          {name = name,
           url = url,
           refresh = refresh}
      end;

  fun addUrlRepoSectionState x state =
      let
        val RepoSectionState {name,url,refresh} = state

        val url =
            case url of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = urlRepoKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        RepoSectionState
          {name = name,
           url = url,
           refresh = refresh}
      end;

  fun addRefreshRepoSectionState x state =
      let
        val RepoSectionState {name,url,refresh} = state

        val refresh =
            case refresh of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = refreshRepoKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        RepoSectionState
          {name = name,
           url = url,
           refresh = refresh}
      end;

  fun processRepoSectionState (kv,state) =
      let
        val Config.KeyValue {key,value} = kv
      in
        if key = nameRepoKey then addNameRepoSectionState value state
        else if key = urlRepoKey then addUrlRepoSectionState value state
        else if key = refreshRepoKey then addRefreshRepoSectionState value state
        else raise Error ("unknown key: " ^ Config.toStringKey {key = key})
      end;

  fun finalRepoSectionState state =
      let
        val RepoSectionState {name,url,refresh} = state

        val name =
            case name of
              SOME x => x
            | NONE =>
              let
                val err =
                    "missing " ^ Config.toStringKey {key = nameRepoKey} ^ " key"
              in
                raise Error err
              end

        val url =
            case url of
              SOME x => x
            | NONE =>
              let
                val err =
                    "missing " ^ Config.toStringKey {key = urlRepoKey} ^ " key"
              in
                raise Error err
              end

        val refresh =
            case refresh of
              SOME x => fromStringInterval x
            | NONE => refreshRepoDefault
      in
        Repo
          {name = name,
           url = url,
           refresh = refresh}
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

val defaultRepo =
    Repo
      {name = nameRepoGilith,
       url = urlRepoGilith,
       refresh = refreshRepoGilith};

val defaultRepos = [defaultRepo];

(* ------------------------------------------------------------------------- *)
(* A type of system configuration data.                                      *)
(* ------------------------------------------------------------------------- *)

datatype install =
    Install of
      {cleanup : Time.time};

fun cleanupInstall (Install {cleanup = x, ...}) = x;

fun toSectionInstall ins =
    let
      val Install {cleanup} = ins
    in
      Config.Section
        {name = installSection,
         keyValues =
           [Config.KeyValue
              {key = cleanupInstallKey,
               value = toStringInterval cleanup}]}
    end;

local
  datatype installSectionState =
      InstallSectionState of
        {cleanup : string option};

  val initialInstallSectionState =
      let
        val cleanup = NONE
      in
        InstallSectionState
          {cleanup = cleanup}
      end;

  fun addCleanupInstallSectionState x state =
      let
        val InstallSectionState {cleanup} = state

        val cleanup =
            case cleanup of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = cleanupInstallKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        InstallSectionState
          {cleanup = cleanup}
      end;

  fun processInstallSectionState (kv,state) =
      let
        val Config.KeyValue {key,value} = kv
      in
        if key = cleanupInstallKey then
          addCleanupInstallSectionState value state
        else
          raise Error ("unknown key: " ^ Config.toStringKey {key = key})
      end;

  fun finalInstallSectionState ins state =
      let
        val InstallSectionState {cleanup} = state

        val cleanup =
            case cleanup of
              SOME x => fromStringInterval x
            | NONE => cleanupInstall ins
      in
        Install
          {cleanup = cleanup}
      end;
in
  fun fromSectionInstall sys kvs =
      let
        val state = initialInstallSectionState

        val state = List.foldl processInstallSectionState state kvs
      in
        finalInstallSectionState sys state
      end
      handle Error err =>
        let
          val err =
              "in section " ^
              Config.toStringSectionName {name = installSection} ^
              " of config file:\n" ^ err
        in
          raise Error err
        end;
end;

val defaultInstall =
    Install
      {cleanup = cleanupInstallDefault};

(* ------------------------------------------------------------------------- *)
(* A type of system configuration data.                                      *)
(* ------------------------------------------------------------------------- *)

fun toSectionSystem sys =
    let
      val {chmod,cp,curl,echo,sha,tar} = DirectorySystem.dest sys
    in
      Config.Section
        {name = systemSection,
         keyValues =
           [Config.KeyValue
              {key = chmodSystemKey,
               value = chmod},
            Config.KeyValue
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
               value = tar}]}
    end;

local
  datatype systemSectionState =
      SystemSectionState of
        {chmod : string option,
         cp : string option,
         curl : string option,
         echo : string option,
         sha : string option,
         tar : string option};

  val initialSystemSectionState =
      let
        val chmod = NONE
        and cp = NONE
        and curl = NONE
        and echo = NONE
        and sha = NONE
        and tar = NONE
      in
        SystemSectionState
          {chmod = chmod,
           cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar}
      end;

  fun addChmodSystemSectionState x state =
      let
        val SystemSectionState {chmod,cp,curl,echo,sha,tar} = state

        val chmod =
            case chmod of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = chmodSystemKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        SystemSectionState
          {chmod = chmod,
           cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar}
      end;

  fun addCpSystemSectionState x state =
      let
        val SystemSectionState {chmod,cp,curl,echo,sha,tar} = state

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
          {chmod = chmod,
           cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar}
      end;

  fun addCurlSystemSectionState x state =
      let
        val SystemSectionState {chmod,cp,curl,echo,sha,tar} = state

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
          {chmod = chmod,
           cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar}
      end;

  fun addEchoSystemSectionState x state =
      let
        val SystemSectionState {chmod,cp,curl,echo,sha,tar} = state

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
          {chmod = chmod,
           cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar}
      end;

  fun addShaSystemSectionState x state =
      let
        val SystemSectionState {chmod,cp,curl,echo,sha,tar} = state

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
          {chmod = chmod,
           cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar}
      end;

  fun addTarSystemSectionState x state =
      let
        val SystemSectionState {chmod,cp,curl,echo,sha,tar} = state

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
          {chmod = chmod,
           cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar}
      end;

  fun processSystemSectionState (kv,state) =
      let
        val Config.KeyValue {key,value} = kv
      in
        if key = chmodSystemKey then addChmodSystemSectionState value state
        else if key = cpSystemKey then addCpSystemSectionState value state
        else if key = curlSystemKey then addCurlSystemSectionState value state
        else if key = echoSystemKey then addEchoSystemSectionState value state
        else if key = shaSystemKey then addShaSystemSectionState value state
        else if key = tarSystemKey then addTarSystemSectionState value state
        else raise Error ("unknown key: " ^ Config.toStringKey {key = key})
      end;

  fun finalSystemSectionState sys state =
      let
        val SystemSectionState {chmod,cp,curl,echo,sha,tar} = state

        val chmod =
            case chmod of
              SOME x => x
            | NONE => let val {chmod = x} = DirectorySystem.chmod sys in x end

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
      in
        DirectorySystem.mk
          {chmod = chmod,
           cp = cp,
           curl = curl,
           echo = echo,
           sha = sha,
           tar = tar}
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
      {chmod = chmodSystemDefault,
       cp = cpSystemDefault,
       curl = curlSystemDefault,
       echo = echoSystemDefault,
       sha = shaSystemDefault,
       tar = tarSystemDefault};

(* ------------------------------------------------------------------------- *)
(* A type of configuration data.                                             *)
(* ------------------------------------------------------------------------- *)

datatype config =
    Config of
      {repos : repo list,
       install : install,
       system : DirectorySystem.system};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val rs = []
      and ins = defaultInstall
      and sys = defaultSystem
    in
      Config
        {repos = rs,
         install = ins,
         system = sys}
    end;

fun repos (Config {repos = x, ...}) = x;

fun install (Config {install = x, ...}) = x;

fun system (Config {system = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Basic operations.                                                         *)
(* ------------------------------------------------------------------------- *)

fun addRepo cfg r =
    let
      val Config {repos = rs, install = ins, system = sys} = cfg

      val rs = rs @ [r]
    in
      Config {repos = rs, install = ins, system = sys}
    end;

fun replaceInstall cfg ins =
    let
      val Config {repos = rs, install = _, system = sys} = cfg
    in
      Config {repos = rs, install = ins, system = sys}
    end;

fun replaceSystem cfg sys =
    let
      val Config {repos = rs, install = ins, system = _} = cfg
    in
      Config {repos = rs, install = ins, system = sys}
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun toSections config =
    let
      val Config {repos = rs, install = ins, system = sys} = config

      val sections =
          map toSectionRepo rs @
          [toSectionInstall ins] @
          [toSectionSystem sys]
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
        else if name = installSection then
          let
            val ins = install conf

            val ins = fromSectionInstall ins keyValues
          in
            replaceInstall conf ins
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
      and ins = defaultInstall
      and sys = defaultSystem
    in
      Config {repos = rs, install = ins, system = sys}
    end;

end
