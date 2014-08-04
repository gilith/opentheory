(* ========================================================================= *)
(* PACKAGE THEORY SOURCE FILES                                               *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageSource :> PackageSource =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "thy";

(* ------------------------------------------------------------------------- *)
(* Theory package source filenames.                                          *)
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
(* A type of theory package source files.                                    *)
(* ------------------------------------------------------------------------- *)

datatype source' =
    Source' of
      {tags : PackageTag.tag list,
       theories : PackageTheory.theory list};

type source = source';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk src' : source = src';

fun dest src : source' = src;

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

fun tags' (Source' {tags = x, ...}) = x;

fun tags src = tags' (dest src);

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

fun name src = PackageTag.findName (tags src);

fun version src = PackageTag.findVersion (tags src);

fun nameVersion src =
    let
      val b = name src
      and v = version src

      val nv' = PackageNameVersion.NameVersion' {name = b, version = v}
    in
      PackageNameVersion.mk nv'
    end;

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

fun description src = PackageTag.findDescription (tags src);

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

fun author src = PackageTag.findAuthor (tags src);

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

fun license src = PackageTag.findLicense (tags src);

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

fun extraFiles src = PackageTag.toExtraList (tags src);

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

fun requires src = PackageTag.requires (tags src);

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

fun show src = PackageTag.toShow (tags src);

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

fun theory' (Source' {theories = x, ...}) = x;

fun theory src = theory' (dest src);

fun emptyTheory src =
    case theory src of
      [thy] => PackageTheory.emptyMain thy
    | _ => false;

(* ------------------------------------------------------------------------- *)
(* Package articles.                                                         *)
(* ------------------------------------------------------------------------- *)

fun articles src = PackageTheory.articles (theory src);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun includes src = PackageTheory.includes (theory src);

fun updateIncludes f src =
    let
      val Source' {tags,theories} = dest src
    in
      case PackageTheory.updateIncludes f theories of
        SOME theories =>
        let
          val src' = Source' {tags = tags, theories = theories}
        in
          SOME (mk src')
        end
      | NONE => NONE
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppThy thy =
      Print.program
        [Print.newline,
         Print.newline,
         PackageTheory.pp thy];
in
  fun pp' src =
    let
      val Source' {tags,theories} = src
    in
      if List.null tags then
        case theories of
          [] => Print.skip
        | thy :: theories =>
          Print.consistentBlock 0
            (PackageTheory.pp thy ::
             List.map ppThy theories)
      else
        Print.consistentBlock 0
          (PackageTag.ppList tags ::
           List.map ppThy theories)
    end;
end;

val pp = Print.ppMap dest pp';

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val sourceSpaceParser' =
      (PackageTag.parserList ++
       atLeastOne PackageTheory.parser) >>
      (fn (ts,ths) => Source' {tags = ts, theories = ths});

  val sourceSpaceParser = sourceSpaceParser' >> mk;
in
  val parser = manySpace ++ sourceSpaceParser >> snd;

  val parser' = parser >> (fn src => [src]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {source,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp source);

fun fromTextFile {filename} =
    let
      (* Estimating parse error line numbers *)

      val lines = Stream.fromTextFile {filename = filename}

      val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
    in
      (let
         (* The character stream *)

         val chars = Stream.filter (not o Interpretation.isCommentLine) chars

         val chars = Parse.everything Parse.any chars

         (* The source stream *)

         val srcs = Parse.everything parser' chars
       in
         case Stream.toList srcs of
           [] => raise Error "missing theory block"
         | [src] => src
         | _ :: _ :: _ => raise Error "multiple tag blocks"
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in theory source file \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

end
