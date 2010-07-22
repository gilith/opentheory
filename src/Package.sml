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

val fileSuffixTag = "-file"
and nameTag = "name"
and versionTag = "version";

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
    case List.filter (equal nameTag o Tag.name) (tags pkg) of
      [] => raise Error ("no " ^ nameTag ^ " tag")
    | [tag] => PackageBase.fromString (Tag.value tag)
    | _ :: _ :: _ => raise Error ("multiple " ^ nameTag ^ " tags");

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

local
  fun dest tag =
      let
        val name = Tag.name tag
      in
        if not (String.isSuffix fileSuffixTag name) then NONE
        else
          let
            val value = Tag.value tag

            val {filename} = PackageTheory.fromStringFilename value
          in
            SOME {name = name, filename = filename}
          end
      end;
in
  fun extraFiles pkg = List.mapPartial dest (tags pkg);
end;

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
