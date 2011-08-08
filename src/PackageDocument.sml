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
       files : {theory : string, tarball : string}}

type document = document';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk doc' : document = doc';

fun dest doc : document' = doc;

(* ------------------------------------------------------------------------- *)
(* Output formats.                                                           *)
(* ------------------------------------------------------------------------- *)

fun toHtml doc =
    let
      val Document' {package,summary,files} = dest doc

      val namever = Package.nameVersion package

      val tags = Package.tags package

      val title =
          let
            val text =
                "OpenTheory package " ^ PackageNameVersion.toString namever
          in
            Html.Title text
          end

      val style =
          let
            val attrs = Html.singletonAttrs ("type","text/css")

            val css = join "\n"
                ["body { background-color: white; color: black; font-family: sans-serif; margin: 0.5em; border: 1px dotted black; padding: 1em; }",
                 "h1 { margin-top: 0.5em; }",
                 "td { background-color: #e0e8ff; margin: 0; border: 1px dotted white; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px; vertical-align: top; }",
                 "ul { padding-left: 1.25em; }",
                 "p.sequent { font-family: courier, monospace; }",
                 "span.warning { color: #ff0000; }",
                 "span.namespace { font-style: italic; }",
                 "span.var { color: #007f00; }",
                 "span.negation { color: #00003f; }",
                 "span.infix { color: #00007f; }",
                 "span.binder { color: #7f0000; }"]
          in
            Html.Style (attrs,css)
          end

      val head = Html.Head (title,[],[style])

      val nameBlock =
          let
            val name = PackageName.toString (PackageNameVersion.name namever)

            val {description} = Package.description package

            val text = "Package " ^ name ^ ": " ^ description
          in
            Html.H1 [Html.Text text]
          end

      val tagsBlock =
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

      val tagsBlocks =
          [Html.H2 [Html.Text "Information"],
           tagsBlock]

      val filesBlock =
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

            val items = [tarballItem,theoryItem]
          in
            Html.Ulist (Html.emptyAttrs,items)
          end

      val fileBlocks =
          [Html.H2 [Html.Text "Files"],
           filesBlock]

      val summaryBlocks =
          PackageSummary.toHtml (PackageTag.toShow tags) summary

      val blocks =
          nameBlock ::
          tagsBlocks @
          fileBlocks @
          summaryBlocks

      val body = Html.Body (Html.emptyAttrs,blocks)
    in
      Html.Html (head,body)
    end;

fun toHtmlFile {document = doc, filename} =
    let
      val html = toHtml doc

      val strm = Print.toStream Html.pp html
    in
      Stream.toTextFile {filename = filename} strm
    end;

end
