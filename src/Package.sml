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

val baseTag = "name"
and descriptionTag = "description"
and fileExtension = "thy"
and fileSuffixTag = "-file"
and versionTag = "version";

(* ------------------------------------------------------------------------- *)
(* Theory package filenames.                                                 *)
(* ------------------------------------------------------------------------- *)

fun mkFilename base =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = PackageBase.toString base,
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
        else total PackageBase.fromString base
    end;

fun isFilename file = Option.isSome (destFilename file);

(* ------------------------------------------------------------------------- *)
(* Types of theory package syntax.                                           *)
(* ------------------------------------------------------------------------- *)

datatype package' =
    Package' of
      {tags : Tag.tag list,
       theories : PackageTheory.theory list};

type package = package';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk pkg' : package = pkg';

fun dest pkg : package' = pkg;

fun tags' (Package' {tags = x, ...}) = x;

fun theories' (Package' {theories = x, ...}) = x;

fun tags pkg = tags' (dest pkg);

fun theories pkg = theories' (dest pkg);

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

fun base pkg =
    case List.filter (equal baseTag o Tag.name) (tags pkg) of
      [] => raise Error ("no " ^ baseTag ^ " tag")
    | [tag] => PackageBase.fromString (Tag.value tag)
    | _ :: _ :: _ => raise Error ("multiple " ^ baseTag ^ " tags");

fun version pkg =
    case List.filter (equal versionTag o Tag.name) (tags pkg) of
      [] => raise Error ("no " ^ versionTag ^ " tag")
    | [tag] => PackageVersion.fromString (Tag.value tag)
    | _ :: _ :: _ => raise Error ("multiple " ^ versionTag ^ " tags");

fun name pkg =
    let
      val b = base pkg
      and v = version pkg
    in
      PackageName.mk (PackageName.Name' {base = b, version = v})
    end;

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

fun description pkg =
    case List.find (equal descriptionTag o Tag.name) (tags pkg) of
      NONE => NONE
    | SOME tag => SOME (Tag.value tag);

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun articles pkg = PackageTheory.articles (theories pkg);

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun packages pkg = PackageTheory.packages (theories pkg);

(* ------------------------------------------------------------------------- *)
(* File dependencies.                                                        *)
(* ------------------------------------------------------------------------- *)

datatype extraFile =
    ExtraFile of
      {name : string,
       filename : string};

fun nameExtraFile (ExtraFile {name = x, ...}) = x;

fun filenameExtraFile (ExtraFile {filename = x, ...}) = {filename = x};

fun normalizeExtraFile (ExtraFile {name,filename}) =
    let
      val filename = OS.Path.file filename
    in
      ExtraFile {name = name, filename = filename}
    end;

fun toTagExtraFile (ExtraFile {name,filename}) =
    let
(*OpenTheoryDebug
      val _ = String.isSuffix fileSuffixTag name orelse
              raise Bug "Package.toTagExtraFile"
*)
      val value = PackageTheory.toStringFilename {filename = filename}
    in
      Tag.mk (Tag.Tag' {name = name, value = value})
    end;

fun fromTagExtraFile tag =
    let
      val name = Tag.name tag
    in
      if not (String.isSuffix fileSuffixTag name) then NONE
      else
        let
          val value = Tag.value tag

          val {filename} = PackageTheory.fromStringFilename value
        in
          SOME (ExtraFile {name = name, filename = filename})
        end
    end;

fun extraFiles pkg = List.mapPartial fromTagExtraFile (tags pkg);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppThy thy =
      Print.program
        [Print.addNewline,
         Print.addNewline,
         PackageTheory.pp thy];
in
  fun pp' pkg =
    let
      val Package' {tags,theories} = pkg
    in
      if null tags then
        case theories of
          [] => Print.skip
        | thy :: theories =>
          Print.blockProgram Print.Consistent 0
            (PackageTheory.pp thy ::
             map ppThy theories)
      else
        Print.blockProgram Print.Consistent 0
          (Tag.ppList tags ::
           map ppThy theories)
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
      (Tag.parserList ++
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
         | _ :: _ :: _ => raise Error "multiple theory blocks"
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in package file \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

end
