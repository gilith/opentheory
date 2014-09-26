(* ========================================================================= *)
(* UPLOADING PACKAGES TO A REMOTE REPOSITORY                                 *)
(* Copyright (c) 2014 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure RepositoryUpload :> RepositoryUpload =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of repository uploads.                                             *)
(* ------------------------------------------------------------------------- *)

datatype upload =
    Upload of
      {repository : Repository.repository,
       remote : RepositoryRemote.remote,
       support : PackageNameVersion.nameVersion list,
       packages : PackageNameVersion.nameVersion list};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk = Upload;

(* ------------------------------------------------------------------------- *)
(* Perform checks on an upload.                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun collectAuthors repo =
      let
        fun add (nv,acc) =
            let
              val pkg = Repository.get repo nv

              val auth = Package.author pkg

              val nvs =
                  case PackageAuthorMap.peek acc auth of
                    NONE => PackageNameVersionSet.empty
                  | SOME s => s

              val nvs = PackageNameVersionSet.add nvs nv
            in
              PackageAuthorMap.insert acc (auth,nvs)
            end

        fun flip (auth,nvs) = (nvs,auth)
      in
        fn nvs =>
           let
             val auths = List.foldl add (PackageAuthorMap.new ()) nvs
           in
             List.map flip (PackageAuthorMap.toList auths)
           end
      end;

  fun allInstalled repo nvs =
      List.all (fn nv => Repository.member nv repo) nvs;

  fun checkInstalled repo =
      let
        fun isKnown nv = Repository.member nv repo

        fun add (nv,errs) =
            RepositoryError.add errs
              (RepositoryError.NotInstalled nv)
      in
        fn namevers => fn errs =>
           let
             val (namevers,unknown) = List.partition isKnown namevers

             val errs = List.foldl add errs unknown
           in
             (namevers,errs)
           end
      end;

  fun checkNotOnRepo remote =
      let
        fun check (nv,errs) =
            if not (RepositoryRemote.member nv remote) then errs
            else
              RepositoryError.add errs
                (RepositoryError.AlreadyOnRemote (nv,remote))
      in
        fn namevers => fn errs => List.foldl check errs namevers
      end;

  fun checkAncestorsOnRepo dir repo =
      let
        fun addMissing anc errs =
            DirectoryError.AncestorNotOnRepo (anc,repo) :: errs

        fun addDifferent anc errs =
            DirectoryError.AncestorWrongChecksumOnRepo (anc,repo) :: errs

        fun checkAnc (anc,errs) =
            let
              val chk =
                  case checksum dir anc of
                    SOME c => c
                  | NONE =>
                    let
                      val err =
                          "depends on package " ^
                          PackageNameVersion.toString anc ^
                          " which seems to be badly installed"
                    in
                      raise Error err
                    end
            in
              case DirectoryRepo.peek repo anc of
                NONE => addMissing anc errs
              | SOME chk' =>
                if Checksum.equal chk chk' then errs
                else addDifferent anc errs
            end
      in
        fn namevers => fn errs =>
           let
             val nvs = PackageNameVersionSet.fromList namevers

             val ancs = includesRTC dir nvs

             val ancs = PackageNameVersionSet.difference ancs nvs
           in
             PackageNameVersionSet.foldl checkAnc errs ancs
           end
      end;

  fun checkSameAuthor dir namevers errs =
      let
        val auths = collectAuthors dir (List.rev namevers)
      in
        case auths of
          [] => raise Bug "Directory.checkUpload.checkSameAuthor"
        | [(_,auth)] => (auth,errs)
        | (_,auth) :: _ :: _ =>
          (auth, DirectoryError.MultipleAuthors auths :: errs)
      end;

  fun checkObsoleteInstalled dir repo =
      let
        fun check (nv,(obs,errs)) =
            case DirectoryRepo.previousNameVersion repo nv of
              NONE => (obs,errs)
            | SOME (nv',chk') =>
              case checksum dir nv' of
                NONE =>
                let
                  val err =
                      DirectoryError.UninstalledObsolete
                        {upload = nv,
                         obsolete = nv'}
                in
                  (obs, err :: errs)
                end
              | SOME chk =>
                if Checksum.equal chk chk' then (nv' :: obs, errs)
                else
                  let
                    val err =
                        DirectoryError.WrongChecksumObsolete
                          {upload = nv,
                           obsolete = nv'}
                  in
                    (obs, err :: errs)
                  end
      in
        fn namevers => fn errs => List.foldl check ([],errs) namevers
      end;

  fun checkObsoleteAuthors dir author obsolete errs =
      let
        fun notAuthor (_,auth) = not (PackageAuthor.equal auth author)

        val auths = collectAuthors dir obsolete

        val auths = List.filter notAuthor auths
      in
        if List.null auths then errs
        else DirectoryError.ObsoleteAuthors auths :: errs
      end;
in
  fun checkUpload dir {repo, support, packages = namevers} =
      let
        val errs = []

        (* Check there exist upload packages *)

        val () =
            if not (List.null namevers) then ()
            else raise Bug "Directory.checkUpload: no upload packages"

        (* Check upload packages are installed *)

        val () =
            if allInstalled dir support then ()
            else raise Bug "Directory.checkUpload: support not installed"

        val (namevers,errs) = checkInstalled dir namevers errs
      in
        if List.null namevers then List.rev errs
        else
          let
            (* Check upload packages are in install order *)

            val () =
                if dependencyOrdered dir (support @ namevers) then ()
                else raise Bug "Directory.checkUpload: not in dependency order"

            (* Check upload packages are not installed on the repo *)

            val errs = checkNotOnRepo repo (support @ namevers) errs

            (* Check upload ancestor packages are installed on the repo *)

            val errs = checkAncestorsOnRepo dir repo (support @ namevers) errs

            (* Check upload packages have the same author *)

            val (author,errs) = checkSameAuthor dir namevers errs

            (* Warn if obsolete packages are not installed *)

            val (obsolete,errs) = checkObsoleteInstalled dir repo namevers errs

            (* Warn about obsoleting packages by other authors *)

            val errs = checkObsoleteAuthors dir author obsolete errs
          in
            List.rev errs
          end
      end;
end;

fun supportUpload dir upl namever =
    let
      val chk =
          case checksum dir namever of
            SOME c => c
          | NONE =>
            let
              val err =
                  "package " ^ PackageNameVersion.toString namever ^
                  " seems to be badly installed"
            in
              raise Error err
            end

      val () = DirectoryRepo.supportUpload upl namever chk
    in
      ()
    end;

fun packageUpload dir upl namever =
    let
      val info = get dir namever

      val chk =
          case checksum dir namever of
            SOME c => c
          | NONE =>
            let
              val err =
                  "package " ^ PackageNameVersion.toString namever ^
                  " seems to be badly installed"
            in
              raise Error err
            end

      val () = DirectoryRepo.packageUpload upl info chk
    in
      ()
    end;

fun ppUpload dir {repo,support,packages} =
    let
      fun ppStep step pps =
          Print.inconsistentBlock 3
            (Print.ppInt step ::
             Print.ppString ". " ::
             pps)

      val ppNameVer = PackageNameVersion.pp

      fun ppNameVers nv nvs =
          ppNameVer nv ::
          List.map (Print.sequence Print.break o ppNameVer) nvs

      val ppAuthor = PackageAuthor.pp

      val step = 0

      val (step,ppSupport) =
          case support of
            [] => (step,Print.skip)
          | nv :: nvs =>
            let
              val step = step + 1

              val num = length nvs + 1

              val mesg = "Request installation of "

              val ppNum =
                  if num = 1 then Print.ppString "a support package:"
                  else
                    Print.sequence (Print.ppInt num)
                      (Print.ppString " support packages:")

              val pp =
                  ppStep step
                    (Print.ppString mesg ::
                     ppNum ::
                     Print.newline ::
                     ppNameVers nv nvs)
            in
              (step, Print.sequence Print.newline pp)
            end

      val (author,step,ppPackages) =
          case packages of
            [] => raise Bug "Directory.ppUpload: no packages"
          | nv :: nvs =>
            let
              val author =
                  let
                    val info = get dir nv

                    val pkg = PackageInfo.package info
                  in
                    Package.author pkg
                  end

              val step = step + 1

              val num = length nvs + 1

              val mesg = "Upload "

              val ppNum =
                  if num = 1 then Print.ppString "the package:"
                  else
                    Print.sequence (Print.ppInt num)
                      (Print.ppString " packages:")

              val pp =
                  ppStep step
                    (Print.ppString mesg ::
                     ppNum ::
                     Print.newline ::
                     ppNameVers nv nvs)
            in
              (author,step,pp)
            end

      val (step,ppAuthorConfirm) =
          let
            val step = step + 1

            val mesg = "Send a confirmation email to the package author:"

            val pp =
                ppStep step
                  [Print.ppString mesg,
                   Print.newline,
                   ppAuthor author]
          in
            (step,pp)
          end

      val ppRepo =
          Print.inconsistentBlock 2
            [Print.ppString "About to upload to ",
             DirectoryRepo.pp repo,
             Print.ppString " in ",
             Print.ppInt step,
             Print.ppString " steps"]
    in
      Print.inconsistentBlock 0
        [ppRepo,
         ppSupport,
         Print.newline,
         ppPackages,
         Print.newline,
         ppAuthorConfirm]
    end;

end
