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

val chmodSystemKey = "chmod"
and cleanupInstallKey = "cleanup"
and cpSystemKey = "cp"
and curlSystemKey = "curl"
and echoSystemKey = "echo"
and installSection = "install"
and licenseSection = "license"
and nameLicenseKey = "name"
and nameRepoKey = "name"
and refreshRepoKey = "refresh"
and repoSection = "repo"
and shaSystemKey = "sha"
and systemSection = "system"
and tarSystemKey = "tar"
and urlLicenseKey = "url"
and urlRepoKey = "url";

val defaultRepoRefresh = Time.fromSeconds 604800;  (* 1 week *)

val gilithRepoName = "gilith"
and gilithRepoRefresh = defaultRepoRefresh
and gilithRepoUrl = "http://opentheory.gilith.com/";

val mitLicenseName = "MIT"
and mitLicenseUrl = "url";

val holLightLicenseName = "HOLLight"
and holLightLicenseUrl = "url";

val defaultInstallCleanup = Time.fromSeconds 3600;  (* 1 hour *)

val defaultSystemChmod = "chmod"
and defaultSystemCp = "cp"
and defaultSystemCurl = "curl --silent --show-error --user-agent opentheory"
and defaultSystemEcho = "echo"
and defaultSystemSha = "sha1sum --binary"
and defaultSystemTar = "tar";

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
            | NONE => defaultRepoRefresh
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
      {name = gilithRepoName,
       url = gilithRepoUrl,
       refresh = gilithRepoRefresh};

val defaultRepos = [defaultRepo];

(* ------------------------------------------------------------------------- *)
(* A type of license configuration data.                                     *)
(* ------------------------------------------------------------------------- *)

datatype license =
    License of
      {name : string,
       url : string};

fun nameLicense (License {name = x, ...}) = {name = x};

fun urlLicense (License {url = x, ...}) = {url = x};

fun findLicense licenses {name = n} =
    let
      fun pred license =
          let
            val {name = n'} = nameLicense license
          in
            n' = n
          end
    in
      case List.filter pred licenses of
        [] => NONE
      | [license] => SOME license
      | _ :: _ :: _ => raise Bug "multiple licenses with the same name"
    end;

fun toSectionLicense license =
    let
      val License {name,url} = license
    in
      Config.Section
        {name = licenseSection,
         keyValues =
           [Config.KeyValue
              {key = nameLicenseKey,
               value = name},
            Config.KeyValue
              {key = urlLicenseKey,
               value = url}]}
    end;

local
  datatype licenseSectionState =
      LicenseSectionState of
        {name : string option,
         url : string option};

  val initialLicenseSectionState =
      let
        val name = NONE
        and url = NONE
      in
        LicenseSectionState
          {name = name,
           url = url}
      end;

  fun addNameLicenseSectionState x state =
      let
        val LicenseSectionState {name,url} = state

        val name =
            case name of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = nameLicenseKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        LicenseSectionState
          {name = name,
           url = url}
      end;

  fun addUrlLicenseSectionState x state =
      let
        val LicenseSectionState {name,url} = state

        val url =
            case url of
              NONE => SOME x
            | SOME x' =>
              let
                val err =
                    "duplicate " ^
                    Config.toStringKey {key = urlLicenseKey} ^
                    " keys: " ^ x ^ " and " ^ x'
              in
                raise Error err
              end
      in
        LicenseSectionState
          {name = name,
           url = url}
      end;

  fun processLicenseSectionState (kv,state) =
      let
        val Config.KeyValue {key,value} = kv
      in
        if key = nameLicenseKey then addNameLicenseSectionState value state
        else if key = urlLicenseKey then addUrlLicenseSectionState value state
        else raise Error ("unknown key: " ^ Config.toStringKey {key = key})
      end;

  fun finalLicenseSectionState state =
      let
        val LicenseSectionState {name,url} = state

        val name =
            case name of
              SOME x => x
            | NONE =>
              let
                val err =
                    "missing " ^
                    Config.toStringKey {key = nameLicenseKey} ^ " key"
              in
                raise Error err
              end

        val url =
            case url of
              SOME x => x
            | NONE =>
              let
                val err =
                    "missing " ^
                    Config.toStringKey {key = urlLicenseKey} ^ " key"
              in
                raise Error err
              end
      in
        License
          {name = name,
           url = url}
      end;
in
  fun fromSectionLicense kvs =
      let
        val state = initialLicenseSectionState

        val state = List.foldl processLicenseSectionState state kvs
      in
        finalLicenseSectionState state
      end
      handle Error err =>
        let
          val err =
              "in section " ^
              Config.toStringSectionName {name = licenseSection} ^
              " of config file:\n" ^ err
        in
          raise Error err
        end;
end;

val defaultLicense =
    License
      {name = mitLicenseName,
       url = mitLicenseUrl};

val holLightLicense =
    License
      {name = holLightLicenseName,
       url = holLightLicenseUrl};

val defaultLicenses =
    [defaultLicense,
     holLightLicense];

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
      {cleanup = defaultInstallCleanup};

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
      {chmod = defaultSystemChmod,
       cp = defaultSystemCp,
       curl = defaultSystemCurl,
       echo = defaultSystemEcho,
       sha = defaultSystemSha,
       tar = defaultSystemTar};

(* ------------------------------------------------------------------------- *)
(* A type of configuration data.                                             *)
(* ------------------------------------------------------------------------- *)

datatype config =
    Config of
      {repos : repo list,
       licenses : license list,
       install : install,
       system : DirectorySystem.system};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val repos = []
      and licenses = []
      and install = defaultInstall
      and system = defaultSystem
    in
      Config
        {repos = repos,
         licenses = licenses,
         install = install,
         system = system}
    end;

fun repos (Config {repos = x, ...}) = x;

fun licenses (Config {licenses = x, ...}) = x;

fun install (Config {install = x, ...}) = x;

fun system (Config {system = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Basic operations.                                                         *)
(* ------------------------------------------------------------------------- *)

fun addRepo cfg repo =
    let
      val Config
            {repos,
             licenses,
             install,
             system} = cfg

      val repos = repos @ [repo]
    in
      Config
        {repos = repos,
         licenses = licenses,
         install = install,
         system = system}
    end;

fun addLicense cfg license =
    let
      val Config
            {repos,
             licenses,
             install,
             system} = cfg

      val licenses = licenses @ [license]
    in
      Config
        {repos = repos,
         licenses = licenses,
         install = install,
         system = system}
    end;

fun replaceInstall cfg install =
    let
      val Config
            {repos,
             licenses,
             install = _,
             system} = cfg
    in
      Config
        {repos = repos,
         licenses = licenses,
         install = install,
         system = system}
    end;

fun replaceSystem cfg system =
    let
      val Config
            {repos,
             licenses,
             install,
             system = _} = cfg
    in
      Config
        {repos = repos,
         licenses = licenses,
         install = install,
         system = system}
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun toSections cfg =
    let
      val Config
            {repos,
             licenses,
             install,
             system} = cfg

      val sections =
          List.map toSectionRepo repos @
          List.map toSectionLicense licenses @
          [toSectionInstall install] @
          [toSectionSystem system]
    in
      Config.Config {sections = sections}
    end;

val pp = Print.ppMap toSections Config.pp;

(* ------------------------------------------------------------------------- *)
(* I/O.                                                                      *)
(* ------------------------------------------------------------------------- *)

local
  fun addSection (sect,cfg) =
      let
        val Config.Section {name,keyValues} = sect
      in
        if name = repoSection then
          let
            val repo = fromSectionRepo keyValues
          in
            addRepo cfg repo
          end
        else if name = licenseSection then
          let
            val license = fromSectionLicense keyValues
          in
            addLicense cfg license
          end
        else if name = installSection then
          let
            val ins = install cfg

            val ins = fromSectionInstall ins keyValues
          in
            replaceInstall cfg ins
          end
        else if name = systemSection then
          let
            val sys = system cfg

            val sys = fromSectionSystem sys keyValues
          in
            replaceSystem cfg sys
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

  fun fromSections cfg =
      let
        val Config.Config {sections} = cfg
      in
        List.foldl addSection empty sections
      end;
in
  fun fromTextFile filename =
      let
        val cfg =
            Config.fromTextFile filename
            handle IO.Io _ => Config.empty
      in
        fromSections cfg
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
      val repos = defaultRepos
      and licenses = defaultLicenses
      and install = defaultInstall
      and system = defaultSystem
    in
      Config
        {repos = repos,
         licenses = licenses,
         install = install,
         system = system}
    end;

end
