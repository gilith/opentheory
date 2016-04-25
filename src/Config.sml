(* ========================================================================= *)
(* CONFIGURATION FILES                                                       *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Config :>  Config =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val closeSectionChar = #"]"
and commentChar = #"#"
and escapeChar = #"\\"
and keyValueSepChar = #"="
and openSectionChar = #"[";

val specialChars =
    [closeSectionChar,
     commentChar,
     escapeChar,
     keyValueSepChar,
     openSectionChar];

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun foldlPartialList f =
    let
      fun inc z xs =
          case xs of
            [] => z
          | x :: xs =>
            case f (x,z) of
              SOME z => inc z xs
            | NONE => z
    in
      inc
    end;

(* ------------------------------------------------------------------------- *)
(* A type of configuration files.                                            *)
(* ------------------------------------------------------------------------- *)

datatype keyValue =
    KeyValue of
      {key : string,
       value : string};

datatype section =
    Section of
      {name : string,
       keyValues : keyValue list};

datatype config =
    Config of
      {sections : section list};

val empty = Config {sections = []};

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  val ppCloseSection = Print.ppChar closeSectionChar
  and ppKeyValueSep = Print.ppChar keyValueSepChar
  and ppOpenSection = Print.ppChar openSectionChar;

  val ppSpace = Print.ppChar #" ";

  local
    fun escape c =
        if mem c specialChars then String.implode [escapeChar,c] else str c;

    val escapeString = String.translate escape;
  in
    val ppEscape = Print.ppMap escapeString Print.ppString;
  end;

  val ppSectionSep = Print.newlines 2;
in
  fun ppKey {key = k} = ppEscape k;

  fun ppKeyValue kv =
      let
        val KeyValue {key = k, value = v} = kv
      in
        Print.consistentBlock 0
          [ppKey {key = k},
           ppSpace,
           ppKeyValueSep,
           ppSpace,
           ppEscape v]
      end;

  fun ppSectionName {name = n} =
      Print.consistentBlock 0
        [ppOpenSection,
         ppEscape n,
         ppCloseSection];

  fun ppSection sect =
      let
        val Section {name = n, keyValues = kvs} = sect
      in
        Print.consistentBlock 0
          (ppSectionName {name = n} ::
           List.map (Print.sequence Print.newline o ppKeyValue) kvs)
      end;

  fun pp config =
      let
        val Config {sections = sl} = config
      in
        case sl of
          [] => Print.skip
        | s :: sl =>
          Print.consistentBlock 0
            (ppSection s ::
             List.map (Print.sequence ppSectionSep o ppSection) sl)
      end;
end;

val toStringKey = Print.toString ppKey;

val toStringSectionName = Print.toString ppSectionName;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  datatype lexState =
      NormalLex
    | EscapeLex;

  fun escape c =
      if mem c specialChars then c
      else raise Error ("escape followed by '" ^ str c ^ "' char");

  fun lex (c,l_s) =
      let
        val (l,s) = l_s
      in
        case s of
          NormalLex =>
          if c = commentChar then NONE
          else if c = escapeChar then SOME (l,EscapeLex)
          else SOME (c :: l, NormalLex)
        | EscapeLex => SOME (escape c :: l, NormalLex)
      end;

  fun lexLine l =
      let
        val l = dropWhile Char.isSpace l

        val (l,t) = foldlPartialList lex ([],NormalLex) l

        val () =
            case t of
              NormalLex => ()
            | EscapeLex => raise Error "escape followed by newline"

        val l = dropWhile Char.isSpace l

        val l = List.rev (#"\n" :: l)
      in
        l
      end;
in
  val lexer = Parse.>> (Parse.any,lexLine);
end;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  fun isSpaceChar c = Char.isSpace c andalso c <> #"\n";

  fun isSectionNameChar c = Char.isAlphaNum c;

  fun isKeyChar c = Char.isAlphaNum c;

  fun isValueChar c = c <> #"\n";

  val closeSectionParser = exactChar closeSectionChar
  and commentParser = exactChar commentChar
  and escapeParser = exactChar escapeChar
  and keyValueSepParser = exactChar keyValueSepChar
  and openSectionParser = exactChar openSectionChar;

  val newlineParser = exactChar #"\n";

  val spaceParser = many (some isSpaceChar) >> K ();

  val sectionNameParser = atLeastOne (some isSectionNameChar) >> String.implode;

  val keyParser = atLeastOne (some isKeyChar) >> String.implode;

  val valueParser = many (some isValueChar) >> String.implode;

  val beginSectionParser =
      openSectionParser ++ sectionNameParser ++
      closeSectionParser ++ newlineParser >>
      (fn ((),(s,((),()))) => s);

  val keyValueParser =
      keyParser ++ spaceParser ++
      keyValueSepParser ++ spaceParser ++
      valueParser ++ newlineParser >>
      (fn (k,((),((),((),(v,()))))) => KeyValue {key = k, value = v});

  val endSectionParser =
      atLeastOne newlineParser >> K ();

  val sectionParser =
      beginSectionParser ++
      many keyValueParser ++
      (endSectionParser || finished) >>
      (fn (n,(kvs,())) => Section {name = n, keyValues = kvs});
in
  val parser = many newlineParser ++ sectionParser >> snd;

  val parser' = parser >> (fn sect => [sect]);
end;

(* ------------------------------------------------------------------------- *)
(* I/O.                                                                      *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {config,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp config);

fun fromTextFile {filename} =
    let
      (* Estimating parse error line numbers *)

      val lines = Stream.fromTextFile {filename = filename}

      val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
    in
      (let
         (* The character stream *)

         val chars = Parse.everything lexer chars

         (* The section stream *)

         val sections = Parse.everything parser' chars
       in
         Config {sections = Stream.toList sections}
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in config file \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

end
