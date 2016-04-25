(* ========================================================================= *)
(* GENERATING HTML DOCUMENTS                                                 *)
(* Copyright (c) 2005 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Html =
sig

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val DOCTYPE : string

(* ------------------------------------------------------------------------- *)
(* Encoding strings for HTML.                                                *)
(* ------------------------------------------------------------------------- *)

val encode : string -> string

(* ------------------------------------------------------------------------- *)
(* Attributes.                                                               *)
(* ------------------------------------------------------------------------- *)

datatype attrs = Attrs of string StringMap.map

val emptyAttrs : attrs

val singletonAttrs : string * string -> attrs

val unionAttrs : attrs -> attrs -> attrs

val toListAttrs : attrs -> (string * string) list

val fromListAttrs : (string * string) list -> attrs

val htmlAttrs : attrs

val tightTableAttrs : attrs

val hrefAttrs : string -> attrs

(* ------------------------------------------------------------------------- *)
(* A simple HTML grammar.                                                    *)
(* ------------------------------------------------------------------------- *)

datatype html = Html of head * body

and head = Head of title * meta list * style list

and title = Title of string

and meta = Meta of attrs

and style = Style of attrs * string

and body = Body of attrs * block list

and block =
    H1 of inline list
  | H2 of inline list
  | H3 of inline list
  | H4 of inline list
  | Table of attrs * tableRow list
  | Div of attrs * flow
  | Para of attrs * inline list
  | Ulist of attrs * listItem list
  | Img of attrs
  | Hr

and inline =
    Raw of string
  | Entity of string  (* e.g., "amp" or "#8870" *)
  | Text of string
  | Anchor of attrs * inline list
  | Big of inline list
  | Small of inline list
  | Strong of inline list
  | Em of inline list
  | Sub of inline list
  | Sup of inline list
  | Span of attrs * inline list
  | Break

and flow =
    Block of block list
  | Inline of inline list

and tableRow = TableRow of tableEntry list

and tableEntry = TableEntry of attrs * flow

and listItem = ListItem of attrs * flow list

(* ------------------------------------------------------------------------- *)
(* Constructors.                                                             *)
(* ------------------------------------------------------------------------- *)

val contentTypeMeta : meta

val emptyFlow : flow

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppAttrs : attrs Print.pp

val ppHtml : html Print.pp

val ppHead : head Print.pp  (* Automatically includes contentTypeMeta *)

val ppTitle : title Print.pp

val ppMeta : meta Print.pp

val ppStyle : style Print.pp

val ppBody : body Print.pp

val ppBlock : block Print.pp

val ppInline : inline Print.pp

val ppFlow : flow Print.pp

val ppTableRow : tableRow Print.pp

val ppTableEntry : tableEntry Print.pp

val pp : html Print.pp  (* DOCTYPE + ppHtml *)

(* ------------------------------------------------------------------------- *)
(* Pretty printing to HTML paragraphs.                                       *)
(* ------------------------------------------------------------------------- *)

val ppFixed : inline list Print.pp

val toFixed : 'a Print.pp -> 'a -> inline list

end
