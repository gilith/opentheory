(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE SYNTAX                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Package :> Package =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "thy";

(* ------------------------------------------------------------------------- *)
(* Theory package filenames.                                                 *)
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
(* Types of theory package syntax.                                           *)
(* ------------------------------------------------------------------------- *)

datatype package' =
    Package' of
      {tags : PackageTag.tag list,
       theories : PackageTheory.theory list};

type package = package';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk pkg' : package = pkg';

fun dest pkg : package' = pkg;

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

fun tags' (Package' {tags = x, ...}) = x;

fun tags pkg = tags' (dest pkg);

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

fun name pkg = PackageTag.findName (tags pkg);

fun version pkg = PackageTag.findVersion (tags pkg);

fun nameVersion pkg =
    let
      val b = name pkg
      and v = version pkg

      val nv' = PackageNameVersion.NameVersion' {name = b, version = v}
    in
      PackageNameVersion.mk nv'
    end;

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

fun description pkg = PackageTag.findDescription (tags pkg);

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

fun author pkg = PackageTag.findAuthor (tags pkg);

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

fun license pkg = PackageTag.findLicense (tags pkg);

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

fun extraFiles pkg = PackageTag.toExtraList (tags pkg);

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

fun requires pkg = PackageTag.requires (tags pkg);

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

fun show pkg = PackageTag.toShow (tags pkg);

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

fun theory' (Package' {theories = x, ...}) = x;

fun theory pkg = theory' (dest pkg);

fun emptyTheory pkg =
    case theory pkg of
      [thy] => PackageTheory.emptyMain thy
    | _ => false;

(* ------------------------------------------------------------------------- *)
(* Package articles.                                                         *)
(* ------------------------------------------------------------------------- *)

fun articles pkg = PackageTheory.articles (theory pkg);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun includes pkg = PackageTheory.includes (theory pkg);

fun updateIncludes f pkg =
    let
      val Package' {tags,theories} = dest pkg
    in
      case PackageTheory.updateIncludes f theories of
        SOME theories =>
        let
          val pkg' = Package' {tags = tags, theories = theories}
        in
          SOME (mk pkg')
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
  fun pp' pkg =
    let
      val Package' {tags,theories} = pkg
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

  val packageSpaceParser' =
      (PackageTag.parserList ++
       atLeastOne PackageTheory.parser) >>
      (fn (ts,ths) => Package' {tags = ts, theories = ths});

  val packageSpaceParser = packageSpaceParser' >> mk;
in
  val parser = manySpace ++ packageSpaceParser >> snd;

  val parser' = parser >> (fn pkg => [pkg]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {package,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp package);

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

         (* The package stream *)

         val pkgs = Parse.everything parser' chars
       in
         case Stream.toList pkgs of
           [] => raise Error "missing theory block"
         | [pkg] => pkg
         | _ :: _ :: _ => raise Error "multiple tag blocks"
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in package file \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

end
