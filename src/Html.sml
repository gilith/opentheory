(* ========================================================================= *)
(* GENERATING HTML DOCUMENTS                                                 *)
(* Copyright (c) 2005 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Html :> Html =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants                                                                 *)
(* ------------------------------------------------------------------------- *)

val DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">";

(* ------------------------------------------------------------------------- *)
(* Encoding strings for HTML.                                                *)
(* ------------------------------------------------------------------------- *)

fun encodeChar c =
    case c of
      #"<" => "&lt;"
    | #">" => "&gt;"
    | #"&" => "&amp;"
    | #"\"" => "&quot;"
    | #"'" => "&apos;"
    | _ => str c;

val encode = String.translate encodeChar;

(* ------------------------------------------------------------------------- *)
(* Attributes                                                                *)
(* ------------------------------------------------------------------------- *)

datatype attrs = Attrs of string StringMap.map

val emptyAttrs = Attrs (StringMap.new ());

fun nullAttrs (Attrs m) = StringMap.null m;

fun singletonAttrs k_v = Attrs (StringMap.singleton k_v);

local
  fun merge _ = raise Bug "Html.unionAttrs: duplicate attribute";
in
  fun unionAttrs (Attrs a1) (Attrs a2) =
      Attrs (StringMap.union merge a1 a2);
end;

fun toListAttrs (Attrs m) = StringMap.toList m;

fun fromListAttrs l = Attrs (StringMap.fromList l);

val htmlAttrs =
    fromListAttrs
      [("xmlns","http://www.w3.org/1999/xhtml"),
       ("xml:lang","en"),
       ("lang","en")];

val tightTableAttrs =
    fromListAttrs
      [("cellpadding","0"),
       ("cellspacing","0")];

fun hrefAttrs url = singletonAttrs ("href",url);

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
  | Para of attrs * inline list
  | Div of attrs * flow
  | Ulist of attrs * listItem list
  | Img of attrs
  | Hr

and inline =
    Raw of string
  | Entity of string
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

and listItem = ListItem of attrs * flow list;

(* ------------------------------------------------------------------------- *)
(* Constructors.                                                             *)
(* ------------------------------------------------------------------------- *)

val contentTypeMeta =
    Meta
      (fromListAttrs
         [("http-equiv","Content-Type"),
          ("content","text/html; charset=UTF-8")]);

val emptyFlow = Block [];

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppEncode = Print.ppMap encode Print.ppString;

fun ppEntity name =
    Print.program
      [Print.ppString "&",
       Print.ppString name,
       Print.ppString ";"];

fun ppAttr (k,v) =
    Print.program
      [Print.ppString k,
       Print.ppString "=\"",
       Print.ppString v,
       Print.ppString "\""];

local
  fun ppSpaceAttr kv = Print.sequence Print.break (ppAttr kv);
in
  fun ppAttrs attrs =
      case toListAttrs attrs of
        [] => Print.skip
      | kv :: kvs =>
        Print.inconsistentBlock 0
          (ppAttr kv :: List.map ppSpaceAttr kvs);
end;

local
  fun ppSpaceAttrs attrs =
      if nullAttrs attrs then Print.skip
      else Print.sequence Print.break (ppAttrs attrs);
in
  fun ppTagOpen name attrs =
      Print.inconsistentBlock 0
        [Print.ppString "<",
         Print.ppString name,
         ppSpaceAttrs attrs,
         Print.ppString ">"];

  fun ppTagClose name =
      Print.inconsistentBlock 0
        [Print.ppString "</",
         Print.ppString name,
         Print.ppString ">"];

  fun ppTagSelfClosing name attrs =
      Print.inconsistentBlock 0
        [Print.ppString "<",
         Print.ppString name,
         ppSpaceAttrs attrs,
         Print.ppString " />"];
end;

fun ppTag name attrs pps =
    Print.inconsistentBlock 0
      [Print.program (ppTagOpen name attrs :: pps),
       ppTagClose name];

local
  val breakPoint = Print.breaks 0;

  fun addBreakPoint (p,ps) = breakPoint :: p :: ps;
in
  fun ppTagBlock name attrs pps =
      let
        val pps =
            case List.rev pps of
              [] => pps
            | l => List.foldl addBreakPoint [breakPoint] l
      in
        ppTag name attrs pps
      end;
end;

fun ppHtml (Html (head,body)) =
    let
      val name = "html"
      and attrs = htmlAttrs
      and pps =
          [ppHead head,
           ppBody body]
    in
      ppTagBlock name attrs pps
    end

and ppHead (Head (title,metas,styles)) =
    let
      val name = "head"
      and attrs = emptyAttrs
      and pps =
          (ppMeta contentTypeMeta ::
           ppTitle title ::
           List.map ppMeta metas @
           List.map ppStyle styles)
    in
      ppTagBlock name attrs pps
    end

and ppTitle (Title title) =
    let
      val name = "title"
      and attrs = emptyAttrs
      and pps = [ppEncode title]
    in
      ppTag name attrs pps
    end

and ppMeta (Meta attrs) =
    let
      val name = "meta"
    in
      ppTagSelfClosing name attrs
    end

and ppStyle (Style (attrs,style)) =
    let
      val name = "style"
      and pps = [ppEncode style]
    in
      ppTag name attrs pps
    end

and ppBody (Body (attrs,blocks)) =
    let
      val name = "body"
      and pps = List.map ppBlock blocks
    in
      ppTagBlock name attrs pps
    end

and ppBlock block =
    case block of
      H1 inlines =>
      let
        val name = "h1"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | H2 inlines =>
      let
        val name = "h2"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | H3 inlines =>
      let
        val name = "h3"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | H4 inlines =>
      let
        val name = "h4"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Table (attrs,rows) =>
      let
        val name = "table"
        and pps = List.map ppTableRow rows
      in
        ppTagBlock name attrs pps
      end
    | Para (attrs,inlines) =>
      let
        val name = "p"
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Div (attrs,flow) =>
      let
        val name = "div"
        and pps = [ppFlow flow]
      in
        ppTag name attrs pps
      end
    | Ulist (attrs,items) =>
      let
        val name = "ul"
        and pps = List.map ppListItem items
      in
        ppTag name attrs pps
      end
    | Img attrs =>
      let
        val name = "img"
      in
        ppTagSelfClosing name attrs
      end
    | Hr =>
      let
        val name = "hr"
        and attrs = emptyAttrs
      in
        ppTagSelfClosing name attrs
      end

and ppInline inline =
    case inline of
      Raw s => Print.ppString s
    | Entity name => ppEntity name
    | Text text => ppEncode text
    | Anchor (attrs,inlines) =>
      let
        val name = "a"
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Big inlines =>
      let
        val name = "big"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Small inlines =>
      let
        val name = "small"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Strong inlines =>
      let
        val name = "strong"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Em inlines =>
      let
        val name = "em"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Sub inlines =>
      let
        val name = "sub"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Sup inlines =>
      let
        val name = "sup"
        and attrs = emptyAttrs
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Span (attrs,inlines) =>
      let
        val name = "span"
        and pps = List.map ppInline inlines
      in
        ppTag name attrs pps
      end
    | Break =>
      let
        val name = "br"
        and attrs = emptyAttrs
      in
        ppTagSelfClosing name attrs
      end

and ppFlow flow =
    case flow of
      Block blocks => Print.program (List.map ppBlock blocks)
    | Inline inlines => Print.program (List.map ppInline inlines)

and ppTableRow (TableRow entries) =
    let
      val name = "tr"
      and attrs = emptyAttrs
      and pps = List.map ppTableEntry entries
    in
      ppTagBlock name attrs pps
    end

and ppTableEntry (TableEntry (attrs,flow)) =
    let
      val name = "td"
      and pps = [ppFlow flow]
    in
      ppTag name attrs pps
    end

and ppListItem (ListItem (attrs,flows)) =
    let
      val name = "li"
      and pps = List.map ppFlow flows
    in
      ppTag name attrs pps
    end;

fun ppInlines inlines = Print.program (List.map ppInline inlines);

fun pp html =
    Print.sequence
      (Print.ppString DOCTYPE)
      (ppHtml html);

(* ------------------------------------------------------------------------- *)
(* Pretty printing to HTML paragraphs.                                       *)
(* ------------------------------------------------------------------------- *)

fun countFixed inline =
    case inline of
      Raw _ => raise Bug "Html.countFixed: Raw"
    | Entity _ => 1
    | Text text => size text
    | Anchor (_,inlines) => countListFixed inlines
    | Big inlines => countListFixed inlines
    | Small inlines => countListFixed inlines
    | Strong inlines => countListFixed inlines
    | Em inlines => countListFixed inlines
    | Sub inlines => countListFixed inlines
    | Sup inlines => countListFixed inlines
    | Span (_,inlines) => countListFixed inlines
    | Break => raise Bug "Html.countFixed: Break"

and countListFixed inlines =
    List.foldl (fn (inline,n) => countFixed inline + n) 0 inlines;

fun ppFixed inlines =
    let
      val n = countListFixed inlines
      and s = Print.toString ppInlines inlines

      val w = Print.Word {word = s, size = n}
    in
      Print.ppWord w
    end;

local
  val nbsp = Print.toString ppInline (Entity "nbsp");

  fun calcIndent n = String.concat (funpow n (cons nbsp) []);

  val cacheSize = 2 * !Print.lineLength;

  val cachedIndent = Vector.tabulate (cacheSize,calcIndent);
in
  fun mkIndent n =
      if n < cacheSize then Vector.sub (cachedIndent,n)
      else calcIndent n;
end;

fun fixIndent {indent,line} = mkIndent indent ^ line;

fun toFixed ppA a =
    let
      val len = SOME (!Print.lineLength)

      val strm = Print.render {lineLength = len} (ppA a)

      val lines = List.map fixIndent (Stream.toList strm)
    in
      case List.rev (List.map Raw lines) of
        [] => []
      | elt :: elts => List.foldl (fn (e,z) => e :: Break :: z) [elt] elts
    end;

end
