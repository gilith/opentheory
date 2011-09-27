(* ========================================================================= *)
(* PACKAGE DOCUMENTS                                                         *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageDocument :> PackageDocument =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "html";

(* ------------------------------------------------------------------------- *)
(* Document filenames.                                                       *)
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
      {package : Package.package,
       summary : PackageSummary.summary,
       files : {theory : string, tarball : string},
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
       "div#main span.syntax { color: #606080; }",
       "div#main span.namespace { font-style: italic; }",
       "div#main span.var { color: #008000; }",
       "div#main span.negation { color: #800080; }",
       "div#main span.infix { color: #000080; }",
       "div#main span.binder { color: #800000; }",
       "div#footer { margin: 2px }",
       "div#footer p { margin: 0; font-style: italic }"];

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
            "OpenTheory package " ^
            PackageNameVersion.toString namever
      in
        Html.Title text
      end;

  fun mkName pkg =
      let
        val name = PackageName.toString (Package.name pkg)

        val {description} = Package.description pkg

        val text = "Package " ^ name ^ ": " ^ description
      in
        Html.H1 [Html.Text text]
      end;

  fun mkInfo pkg =
      let
        fun isShowTag tag =
            PackageName.equal (PackageTag.name tag) PackageName.showTag

        fun tagEntry n v =
            let
              val n = Html.Text (PackageName.toString n)

              val n = Html.TableEntry (Html.emptyAttrs, Html.Inline [n])
              and v = Html.TableEntry (Html.emptyAttrs, Html.Inline v)
            in
              Html.TableRow [n,v]
            end

        fun tagBlock tag =
            let
              val PackageTag.Tag' {name,value} = PackageTag.dest tag

              val value = [Html.Text value]
            in
              tagEntry name value
            end

        val tagsBlock =
            let
              val tags = Package.tags pkg

              val (show,tags) = List.partition isShowTag tags

              val ts = List.map tagBlock tags

              val ts =
                  if List.null show then ts
                  else
                    let
                      val show = PackageTag.toShow show
                    in
                      ts @ [tagEntry PackageName.showTag (Show.toHtml show)]
                    end
            in
              Html.Table (Html.emptyAttrs,ts)
            end
      in
        [Html.H2 [Html.Text "Information"],
         tagsBlock]
      end;

  fun mkFiles files =
      let
        val {theory,tarball} = files

        val tarballItem =
            let
              val text =
                  [Html.Text "Package tarball ",
                   Html.Anchor (Html.hrefAttrs tarball,
                                [Html.Text tarball])]
            in
              Html.ListItem (Html.emptyAttrs, [Html.Inline text])
            end

        val theoryItem =
            let
              val text =
                  [Html.Text "Theory file ",
                   Html.Anchor (Html.hrefAttrs theory,
                                [Html.Text theory]),
                   Html.Text " (included in the package tarball)"]
            in
              Html.ListItem (Html.emptyAttrs, [Html.Inline text])
            end

        val filesBlock =
            let
              val items = [tarballItem,theoryItem]
            in
              Html.Ulist (Html.emptyAttrs,items)
            end
      in
        [Html.H2 [Html.Text "Files"],
         filesBlock]
      end;

  fun mkSummary pkg sum =
      let
        val tags = Package.tags pkg

        val show = PackageTag.toShow tags
      in
        PackageSummary.toHtml show sum
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

        val packageSummaryGeneratedBy =
            Html.Text " package summary generated by "
      in
        fn tool =>
           let
             val inlines = opentheory :: packageSummaryGeneratedBy :: tool
           in
             Html.Para (Html.emptyAttrs,inlines)
           end
      end;
in
  fun toHtml doc =
      let
        val Document' {package,summary,files,tool} = dest doc

        val head =
            let
              val title = mkTitle (Package.nameVersion package)
            in
              Html.Head (title,[],[style])
            end

        val body =
            let
              val main =
                  let
                    val nameBlock = mkName package

                    val infoBlocks = mkInfo package

                    val fileBlocks = mkFiles files

                    val summaryBlocks = mkSummary package summary

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

      val strm = Print.toStream Html.pp html
    in
      Stream.toTextFile {filename = filename} strm
    end;

end
