(* ========================================================================= *)
(* PACKAGE INFORMATION                                                       *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageInformation :> PackageInformation =
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

datatype information' =
    Information' of
      {tags : PackageTag.tag list,
       theory : PackageTheory.theory list};

type information = information';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk info' : information = info';

fun dest info : information' = info;

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

fun tags' (Information' {tags = x, ...}) = x;

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

fun theory' (Information' {theory = x, ...}) = x;

fun theory info = theory' (dest info);

fun emptyTheory info =
    case theory info of
      [thy] => PackageTheory.emptyMain thy
    | _ => false;

(* ------------------------------------------------------------------------- *)
(* Package articles.                                                         *)
(* ------------------------------------------------------------------------- *)

fun articleFiles info = PackageTheory.articles (theory info);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun includes info = PackageTheory.includes (theory info);

fun nameVersionIncludes info =
    PackageNameVersionSet.fromList (List.map fst (includes info));

fun updateIncludes f info =
    let
      val Information' {tags,theory} = dest info
    in
      case PackageTheory.updateIncludes f theory of
        SOME theory =>
        let
          val info' = Information' {tags = tags, theory = theory}
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
      val Information' {tags,theory} = info
    in
      if List.null tags then
        case theory of
          [] => Print.skip
        | thy :: theory =>
          Print.consistentBlock 0
            (PackageTheory.pp thy ::
             List.map ppThy theory)
      else
        Print.consistentBlock 0
          (PackageTag.ppList tags ::
           List.map ppThy theory)
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

  val informationSpaceParser' =
      (PackageTag.parserList ++
       atLeastOne PackageTheory.parser) >>
      (fn (ts,ths) => Information' {tags = ts, theory = ths});

  val informationSpaceParser = informationSpaceParser' >> mk;
in
  val parser = manySpace ++ informationSpaceParser >> snd;

  val parser' = parser >> (fn info => [info]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {information,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp information);

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

         (* The information stream *)

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
