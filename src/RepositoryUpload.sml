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
              val auth = Repository.author repo nv

              val nvs =
                  case PackageAuthorMap.peek acc auth of
                    NONE => PackageNameVersionSet.empty
                  | SOME s => s

              val nvs = PackageNameVersionSet.add nvs nv
            in
              PackageAuthorMap.insert acc (auth,nvs)
            end

        fun flip ((auth,nvs),acc) = (nvs,auth) :: acc
      in
        fn nvs =>
           let
             val auths = List.foldl add (PackageAuthorMap.new ()) nvs
           in
             List.foldl flip [] (PackageAuthorMap.toList auths)
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

  fun checkNotOnRemote remote =
      let
        fun check (nv,errs) =
            if not (RepositoryRemote.member nv remote) then errs
            else
              RepositoryError.add errs
                (RepositoryError.AlreadyOnRemote (nv,remote))
      in
        fn namevers => fn errs => List.foldl check errs namevers
      end;

  fun checkAncestorsOnRemote repo remote =
      let
        fun addMissing anc errs =
            RepositoryError.add errs
              (RepositoryError.AncestorNotOnRemote (anc,remote))

        fun addDifferent anc errs =
            RepositoryError.add errs
              (RepositoryError.AncestorWrongChecksumOnRemote (anc,remote))

        fun checkAnc (anc,errs) =
            let
              val chk =
                  case Repository.peek repo anc of
                    SOME pkg => Package.checksum pkg
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
              case RepositoryRemote.peek remote anc of
                NONE => addMissing anc errs
              | SOME chk' =>
                if Checksum.equal chk chk' then errs
                else addDifferent anc errs
            end
      in
        fn namevers => fn errs =>
           let
             val nvs = PackageNameVersionSet.fromList namevers

             val ancs = Repository.includesRTC repo nvs

             val ancs = PackageNameVersionSet.difference ancs nvs
           in
             PackageNameVersionSet.foldl checkAnc errs ancs
           end
      end;

  fun checkSameAuthor repo namevers errs =
      let
        val auths = collectAuthors repo namevers
      in
        case auths of
          [] => raise Bug "RepositoryUpload.check.checkSameAuthor"
        | [(_,auth)] => (auth,errs)
        | (_,auth) :: _ :: _ =>
          let
            val errs =
                RepositoryError.add errs
                  (RepositoryError.MultipleAuthors auths)
          in
            (auth,errs)
          end
      end;

  fun checkObsoleteInstalled repo remote =
      let
        fun check (nv,(obs,errs)) =
            case RepositoryRemote.previousNameVersion remote nv of
              NONE => (obs,errs)
            | SOME (nv',chk') =>
              case Repository.peek repo nv' of
                NONE =>
                let
                  val err =
                      RepositoryError.UninstalledObsolete
                        {upload = nv,
                         obsolete = nv'}
                in
                  (obs, RepositoryError.add errs err)
                end
              | SOME pkg =>
                if Checksum.equal (Package.checksum pkg) chk' then
                  (nv' :: obs, errs)
                else
                  let
                    val err =
                        RepositoryError.WrongChecksumObsolete
                          {upload = nv,
                           obsolete = nv'}
                  in
                    (obs, RepositoryError.add errs err)
                  end
      in
        fn namevers => fn errs => List.foldl check ([],errs) namevers
      end;

  fun checkObsoleteAuthors repo author obsolete errs =
      let
        fun notAuthor (_,auth) = not (PackageAuthor.equal auth author)

        val auths = collectAuthors repo obsolete

        val auths = List.filter notAuthor auths
      in
        if List.null auths then errs
        else
          RepositoryError.add errs
            (RepositoryError.ObsoleteAuthors auths)
      end;
in
  fun check upload =
      let
        val Upload
              {repository = repo,
               remote,
               support,
               packages = namevers} = upload

        val errs = RepositoryError.clean

        (* Check there exist upload packages *)

        val () =
            if not (List.null namevers) then ()
            else raise Bug "RepositoryUpload.check: no upload packages"

        (* Check upload packages are installed *)

        val () =
            if allInstalled repo support then ()
            else raise Bug "RepositoryUpload.check: support not installed"

        val (namevers,errs) = checkInstalled repo namevers errs
      in
        if List.null namevers then errs
        else
          let
            val pkgs = support @ namevers

            (* Check upload packages are in install order *)

            val () =
                if Repository.dependencyOrdered repo pkgs then ()
                else raise Bug "RepositoryUpload.check: not in dependency order"

            (* Check upload packages are not installed on the remote repo *)

            val errs = checkNotOnRemote remote pkgs errs

            (* Check ancestor packages are installed on the remote repo *)

            val errs = checkAncestorsOnRemote repo remote pkgs errs

            (* Check upload packages have the same author *)

            val (author,errs) = checkSameAuthor repo namevers errs

            (* Warn if obsolete packages are not installed *)

            val (obsolete,errs) =
                checkObsoleteInstalled repo remote namevers errs

            (* Warn about obsoleting packages by other authors *)

            val errs = checkObsoleteAuthors repo author obsolete errs
          in
            errs
          end
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Execute the upload.                                                       *)
(* ------------------------------------------------------------------------- *)

local
  fun startUpload remote =
      let
        val upload = RepositoryRemote.startUpload remote

        val {url} = RepositoryRemote.urlUpload upload

        val mesg =
            "started upload to " ^
            RepositoryRemote.toString remote ^
            ":\n  " ^ url

        val () = chat mesg

        val () = TextIO.flushOut TextIO.stdOut
      in
        upload
      end;

  fun supportUpload repo upload namever =
      let
        val chk =
            case Repository.peek repo namever of
              SOME pkg => Package.checksum pkg
            | NONE =>
              let
                val err =
                    "package " ^ PackageNameVersion.toString namever ^
                    " seems to be badly installed"
              in
                raise Error err
              end

        val () = RepositoryRemote.supportUpload upload namever chk
      in
        ()
      end;

  fun packageUpload repo upload namever =
      let
        val pkg = Repository.get repo namever

        val () = RepositoryRemote.packageUpload upload pkg
      in
        ()
      end;

  fun finishUpload remote upload =
      let
        val () = RepositoryRemote.finishUpload upload

        val mesg =
            "finished upload to " ^ RepositoryRemote.toString remote ^
            ", sent author confirmation email"

        val () = chat mesg

        val () = TextIO.flushOut TextIO.stdOut
      in
        ()
      end;

  fun deleteUpload remote upload =
      let
        val () = RepositoryRemote.deleteUpload upload

        val mesg =
            "encountered error, so deleted upload to " ^
            RepositoryRemote.toString remote

        val () = chat mesg

        val () = TextIO.flushOut TextIO.stdOut
      in
        ()
      end;
in
  fun upload info =
      let
        val Upload
              {repository = repo,
               remote,
               support,
               packages} = info

        val upl = startUpload remote

        val () =
            let
              val () = List.app (supportUpload repo upl) support

              val () = List.app (packageUpload repo upl) packages

              val () = finishUpload remote upl
            in
              ()
            end
            handle Error err =>
              let
                val () =
                    deleteUpload remote upl
                    handle Error err' => raise Error (err ^ "\n" ^ err')
              in
                raise Error err
              end
      in
        ()
      end
      handle Error err =>
        raise Error (err ^ "\npackage upload failed");
end;

(* ------------------------------------------------------------------------- *)
(* Summarize the upload.                                                     *)
(* ------------------------------------------------------------------------- *)

fun pp upload =
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

      val Upload
            {repository = repo,
             remote,
             support,
             packages} = upload

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
            [] => raise Bug "RepositoryUpload.pp: no packages"
          | nv :: nvs =>
            let
              val author = Repository.author repo nv

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

      val ppRemote =
          Print.inconsistentBlock 2
            [Print.ppString "About to upload to ",
             RepositoryRemote.pp remote,
             Print.ppString " in ",
             Print.ppInt step,
             Print.ppString " steps"]
    in
      Print.inconsistentBlock 0
        [ppRemote,
         ppSupport,
         Print.newline,
         ppPackages,
         Print.newline,
         ppAuthorConfirm]
    end;

end
