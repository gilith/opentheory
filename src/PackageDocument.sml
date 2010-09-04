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

      val head = Html.Head (title,[])

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

      val metaBlocks =
          [Html.H2 [Html.Text "Package Meta-Data"],
           tagsBlock]

      val filesBlock =
          let
            val {theory,tarball} = files

            val tarballItem =
                Html.ListItem (Html.emptyAttrs, [Html.Text tarball])

            val theoryItem =
                Html.ListItem (Html.emptyAttrs, [Html.Text theory])

            val items = [tarballItem,theoryItem]
          in
            Html.Ulist (Html.emptyAttrs,items)
          end

      val downloadBlocks =
          [Html.H2 [Html.Text "Downloads"],
           filesBlock]

      val blocks =
          nameBlock ::
          metaBlocks @
          downloadBlocks

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
