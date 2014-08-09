(* ========================================================================= *)
(* PACKAGE INFORMATION                                                       *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageInfo :> PackageInfo =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "thy";

(* ------------------------------------------------------------------------- *)
(* Package information is stored in theory files.                            *)
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
(* A type of package information.                                            *)
(* ------------------------------------------------------------------------- *)

datatype info' =
    Info' of
      {tags : PackageTag.tag list,
       theories : PackageTheory.theory list};

type info = info';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk info' : info = info';

fun dest info : info' = info;

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

fun tags' (Info' {tags = x, ...}) = x;

fun tags info = tags' (dest info);

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

fun name info = PackageTag.findName (tags info);

fun version info = PackageTag.findVersion (tags info);

fun nameVersion info =
    let
      val b = name info
      and v = version info

      val nv' = PackageNameVersion.NameVersion' {name = b, version = v}
    in
      PackageNameVersion.mk nv'
    end;

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

fun description info = PackageTag.findDescription (tags info);

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

fun author info = PackageTag.findAuthor (tags info);

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

fun license info = PackageTag.findLicense (tags info);

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

fun extraFiles info = PackageTag.toExtraList (tags info);

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

fun requires info = PackageTag.requires (tags info);

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

fun show info = PackageTag.toShow (tags info);

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

fun theory' (Info' {theories = x, ...}) = x;

fun theory info = theory' (dest info);

fun emptyTheory info =
    case theory info of
      [thy] => PackageTheory.emptyMain thy
    | _ => false;

(* ------------------------------------------------------------------------- *)
(* Package articles.                                                         *)
(* ------------------------------------------------------------------------- *)

fun articles info = PackageTheory.articles (theory info);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun includes info = PackageTheory.includes (theory info);

fun updateIncludes f info =
    let
      val Info' {tags,theories} = dest info
    in
      case PackageTheory.updateIncludes f theories of
        SOME theories =>
        let
          val info' = Info' {tags = tags, theories = theories}
        in
          SOME (mk info')
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
  fun pp' info =
    let
      val Info' {tags,theories} = info
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

  val infoSpaceParser' =
      (PackageTag.parserList ++
       atLeastOne PackageTheory.parser) >>
      (fn (ts,ths) => Info' {tags = ts, theories = ths});

  val infoSpaceParser = infoSpaceParser' >> mk;
in
  val parser = manySpace ++ infoSpaceParser >> snd;

  val parser' = parser >> (fn info => [info]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {info,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp info);

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

         (* The info stream *)

         val info = Parse.everything parser' chars
       in
         case Stream.toList info of
           [] => raise Error "missing theory block"
         | [inf] => inf
         | _ :: _ :: _ => raise Error "multiple tag blocks"
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in theory source file \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

end
