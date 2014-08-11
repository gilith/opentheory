(* ========================================================================= *)
(* PACKAGE DOCUMENTS                                                         *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageDocument :> PackageDocument =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "html";

(* ------------------------------------------------------------------------- *)
(* Package document filenames.                                               *)
(* ------------------------------------------------------------------------- *)

fun mkFilename namever =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = PackageNameVersion.toString namever,
             ext = SOME fileExtension}
    in
      {filename = filename}
    end;

fun destFilename {filename} =
    let
      val {base,ext} = OS.Path.splitBaseExt (OS.Path.file filename)
    in
      case ext of
        NONE => NONE
      | SOME x =>
        if x <> fileExtension then NONE
        else total PackageNameVersion.fromString base
    end;

fun isFilename file = Option.isSome (destFilename file);

(* ------------------------------------------------------------------------- *)
(* A type of package documents.                                              *)
(* ------------------------------------------------------------------------- *)

datatype document' =
    Document' of
      {information : PackageInformation.information option,
       checksum : Checksum.checksum option,
       summary : PackageSummary.summary,
       files : {theory : string option, tarball : string option},
       tool : Html.inline list}

type document = document';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk doc' : document = doc';

fun dest doc : document' = doc;

(* ------------------------------------------------------------------------- *)
(* Output formats.                                                           *)
(* ------------------------------------------------------------------------- *)

local
  val css =
      ["body { background-color: white; color: black; font-family: sans-serif; margin: 8px }",
       "div#main { margin: 2px; border: 1px dotted black; padding: 12px }",
       "div#main h1 { margin-top: 0.5em; }",
       "div#main td { background-color: #e0e8ff; margin: 0; border: 1px dotted white; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px; vertical-align: top; }",
       "div#main ul { padding-left: 1.25em; }",
       "div#main p.sequent { font-family: courier, monospace; }",
       "div#main span.warning { color: #ff0000; }",
       "div#main span.standard-axiom { font-weight: bold; }",
       "div#main span.syntax { color: #606080; }",
       "div#main span.namespace { font-style: italic; }",
       "div#main span.var { color: #008000; }",
       "div#main span.negation { color: #800080; }",
       "div#main span.infix { color: #000080; }",
       "div#main span.binder { color: #800000; }",
       "div#footer { margin: 2px }",
       "div#footer p { margin: 0; font-style: italic; font-size: smaller }",
       "div#footer p a { text-decoration: none }"];

  val style =
      let
        val attrs = Html.singletonAttrs ("type","text/css")

        val css = join "\n" css
      in
        Html.Style (attrs,css)
      end;

  fun mkTitle namever =
      let
        val text =
            case namever of
              SOME nv => PackageNameVersion.toString nv
            | NONE => "OpenTheory package"
      in
        Html.Title text
      end;

  fun mkName info =
      let
        val text = "Package"

        val text =
            case info of
              NONE => text
            | SOME inf =>
              let
                val name = PackageName.toString (PackageInformation.name inf)

                val {description} = PackageInformation.description inf
              in
                text ^ " " ^ name ^ ": " ^ description
              end
      in
        Html.H1 [Html.Text text]
      end;

  fun mkInfo info chko =
      let
        val isRequiresTag = PackageTag.equalName PackageName.requiresTag
        and isShowTag = PackageTag.equalName PackageName.showTag

        fun tagName n = Html.Text (PackageName.toString n)

        fun tagRow n v =
            let
              val n = tagName n

              val n = Html.TableEntry (Html.emptyAttrs, Html.Inline [n])
              and v = Html.TableEntry (Html.emptyAttrs, Html.Inline v)
            in
              Html.TableRow [n,v]
            end

        fun tagEntry n v = tagRow n [Html.Text v]

        fun tagBlock tag =
            let
              val PackageTag.Tag' {name,value} = PackageTag.dest tag
            in
              tagEntry name value
            end

        val tagsBlock =
            let
              val tags =
                  case info of
                    NONE => []
                  | SOME inf => PackageInformation.tags inf

              val (reqs,tags) = List.partition isRequiresTag tags

              val (show,tags) = List.partition isShowTag tags

              val ts = List.map tagBlock tags

              val ts =
                  case chko of
                    NONE => ts
                  | SOME chk =>
                    let
                      val n = PackageName.checksumTag
                      and v = Checksum.toString chk
                    in
                      ts @ [tagEntry n v]
                    end

              val ts =
                  case PackageTag.requires reqs of
                    [] => ts
                  | req :: reqs =>
                    let
                      fun add (r,l) = Html.Break :: tagName r :: l

                      val value =
                          tagName req :: List.foldl add [] (List.rev reqs)
                    in
                      ts @ [tagRow PackageName.requiresTag value]
                    end

              val ts =
                  if List.null show then ts
                  else
                    let
                      val show = PackageTag.toShow show
                    in
                      ts @ [tagRow PackageName.showTag (Show.toHtml show)]
                    end
            in
              ts
            end
      in
        if List.null tagsBlock then []
        else
          [Html.H2 [Html.Text "Information"],
           Html.Table (Html.emptyAttrs,tagsBlock)]
      end;

  fun mkFiles files =
      let
        val {theory,tarball} = files

        val tarballItem =
            case tarball of
              NONE => []
            | SOME tar =>
              let
                val text =
                    [Html.Text "Package tarball ",
                     Html.Anchor (Html.hrefAttrs tar, [Html.Text tar])]
              in
                [Html.ListItem (Html.emptyAttrs, [Html.Inline text])]
              end

        val theoryItem =
            case theory of
              NONE => []
            | SOME thy =>
              let
                val text =
                    [Html.Text "Theory source file ",
                     Html.Anchor (Html.hrefAttrs thy, [Html.Text thy])]

                val text =
                    if not (Option.isSome tarball) then text
                    else text @ [Html.Text " (included in the package tarball)"]
              in
                [Html.ListItem (Html.emptyAttrs, [Html.Inline text])]
              end

        val items = tarballItem @ theoryItem
      in
        if List.null items then []
        else
          [Html.H2 [Html.Text "Files"],
           Html.Ulist (Html.emptyAttrs,items)]
      end;

  fun mkSummary info sum =
      let
        val show =
            case info of
              NONE => Show.default
            | SOME inf => PackageInformation.show inf
      in
        PackageSummary.toHtml Summary.NoContext show sum
      end;

  val mkFooter =
      let
        val opentheory =
            let
              val attrs =
                  Html.singletonAttrs
                    ("href","http://www.gilith.com/research/opentheory")

              val text = "OpenTheory"
            in
              Html.Anchor (attrs, [Html.Text text])
            end

        val packageDocumentGeneratedBy =
            Html.Text " package document generated by "
      in
        fn tool =>
           let
             val inlines = opentheory :: packageDocumentGeneratedBy :: tool
           in
             Html.Para (Html.emptyAttrs,inlines)
           end
      end;
in
  fun toHtml doc =
      let
        val Document' {information,checksum,summary,files,tool} = dest doc

        val head =
            let
              val namever =
                  case information of
                    SOME info => SOME (PackageInformation.nameVersion info)
                  | NONE => NONE

              val title = mkTitle namever
            in
              Html.Head (title,[],[style])
            end

        val body =
            let
              val main =
                  let
                    val nameBlock = mkName information

                    val infoBlocks = mkInfo information checksum

                    val fileBlocks = mkFiles files

                    val summaryBlocks = mkSummary information summary

                    val blocks =
                        nameBlock ::
                        infoBlocks @
                        fileBlocks @
                        summaryBlocks

                    val attrs = Html.singletonAttrs ("id","main")
                  in
                    Html.Div (attrs, Html.Block blocks)
                  end

              val footer =
                  let
                    val attrs = Html.singletonAttrs ("id","footer")

                    val blocks = [mkFooter tool]
                  in
                    Html.Div (attrs, Html.Block blocks)
                  end

              val blocks = [main,footer]
            in
              Html.Body (Html.emptyAttrs,blocks)
            end
      in
        Html.Html (head,body)
      end;
end;

fun toHtmlFile {document = doc, filename} =
    let
      val html = toHtml doc

      val strm = Print.toStreamWithLineLength {lineLength = NONE} Html.pp html
    in
      Stream.toTextFile {filename = filename} strm
    end;

end
