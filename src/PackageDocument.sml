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

fun mkFilename name =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = PackageName.toString name,
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
        else total PackageName.fromString base
    end;

fun isFilename file = Option.isSome (destFilename file);

(* ------------------------------------------------------------------------- *)
(* A type of package documents.                                              *)
(* ------------------------------------------------------------------------- *)

datatype document' =
    Document' of
      {name : PackageName.name,
       package : Package.package,
       summary : Summary.summary,
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
      val Document' {name,package,summary,files} = dest doc

      val tags = Package.tags package

      val title = Html.Title ("OpenTheory package " ^ PackageName.toString name)

      val style =
          let
            val attrs = Html.singletonAttrs ("type","text/css")

            val css = join "\n"
                ["body { background-color: white; color: black; font-family: sans-serif; margin: 0.5em; border: 1px dotted black; padding: 1em; }",
                 "h1 { margin-top: 0.5em; }",
                 "td { background-color: #e0e8ff; margin: 0; border: 1px dotted white; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px; }",
                 "ul { padding-left: 1.25em; }",
                 "p.sequent { font-family: courier, monospace; }",
                 "span.var { color: #007f00; }",
                 "span.const { color: #000000; }",
                 "span.negation { color: #00003f; }",
                 "span.infix { color: #00007f; }",
                 "span.binder { color: #7f0000; }"]
          in
            Html.Style (attrs,css)
          end

      val head = Html.Head (title,[],[style])

      val nameBlock =
          let
            val text = PackageBase.toString (PackageName.base name)

            val text =
                case Package.description package of
                  NONE => text
                | SOME desc => text ^ ": " ^ desc
          in
            Html.H1 [Html.Text text]
          end

      val tagsBlock =
          let
            fun tagBlock tag =
                let
                  val Tag.Tag' {name,value} = Tag.dest tag

                  val n =
                      Html.TableEntry
                        (Html.emptyAttrs, Html.Inline [Html.Text name])
                  and v =
                      Html.TableEntry
                        (Html.emptyAttrs, Html.Inline [Html.Text value])
                in
                  Html.TableRow [n,v]
                end

            val ts = map tagBlock tags
          in
            Html.Table (Html.emptyAttrs,ts)
          end

      val tagsBlocks =
          [Html.H2 [Html.Text "Package Information"],
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

      val downloadBlocks =
          [Html.H2 [Html.Text "Downloads"],
           filesBlock]

      val summaryBlocks =
          Summary.toHtml (Show.fromTags tags) summary

      val blocks =
          nameBlock ::
          tagsBlocks @
          downloadBlocks @
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
