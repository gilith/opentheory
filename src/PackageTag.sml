(* ========================================================================= *)
(* PACKAGE INFORMATION STORED AS "NAME: VALUE" TAGS                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageTag :> PackageTag =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val separatorString = ":";

(* ------------------------------------------------------------------------- *)
(* A type of tag names.                                                      *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name;

(* ------------------------------------------------------------------------- *)
(* A type of tag values.                                                     *)
(* ------------------------------------------------------------------------- *)

type value = string;

val ppValue = Print.ppString;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val valueParser =
      let
        fun isValueChar c = c <> #"\n"
      in
        many (some isValueChar) >> String.implode
      end;
in
  val parserValue = valueParser;
end;

(* ------------------------------------------------------------------------- *)
(* A type of tags.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype tag' =
    Tag' of
      {name : name,
       value : value};

type tag = tag';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk t : tag = t;

fun dest t : tag' = t;

fun name (Tag' {name = x, ...}) = x;

fun value (Tag' {value = x, ...}) = x;

fun equalName n tag = PackageName.equal n (name tag);

fun destName n tag =
    if equalName n tag then SOME (value tag) else NONE;

fun filterName name = List.mapPartial (destName name);

fun peekName name tags =
    case filterName name tags of
      [] => NONE
    | [v] => SOME v
    | _ :: _ :: _ =>
      raise Error ("multiple " ^ PackageName.toString name ^ " tags");

fun getName name tags =
    case peekName name tags of
      NONE => raise Error ("no " ^ PackageName.toString name ^ " tag")
    | SOME v => v;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (t1,t2) =
    let
      val Tag' {name = n1, value = v1} = t1
      and Tag' {name = n2, value = v2} = t2
    in
      case PackageName.compare (n1,n2) of
        LESS => LESS
      | EQUAL => String.compare (v1,v2)
      | GREATER => GREATER
    end;

fun equal (t1 : tag) t2 = compare (t1,t2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Package basics.                                                           *)
(* ------------------------------------------------------------------------- *)

fun findName tags =
    PackageName.fromString (getName PackageName.nameTag tags);

fun findVersion tags =
    PackageVersion.fromString (getName PackageName.versionTag tags);

fun findDescription tags =
    {description = getName PackageName.descriptionTag tags};

fun findAuthor tags =
    {author = getName PackageName.authorTag tags};

fun findLicense tags =
    {license = getName PackageName.licenseTag tags};

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

fun mkExtraName n = PackageName.append n PackageName.extraSuffixTag;

val destExtraName = PackageName.destStrictSuffix PackageName.extraSuffixTag;

fun toExtra tag =
    let
      val Tag' {name,value} = dest tag
    in
      case destExtraName name of
        NONE => NONE
      | SOME n =>
        let
          val ext = PackageExtra.Extra {name = n, filename = value}
        in
          SOME (PackageExtra.mk ext)
        end
    end;

fun fromExtra ext =
    let
      val PackageExtra.Extra {name,filename} = PackageExtra.dest ext

      val n = mkExtraName name
    in
      mk (Tag' {name = n, value = filename})
    end;

val toExtraList = List.mapPartial toExtra;

val fromExtraList = List.map fromExtra;

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

local
  fun destRequires tag =
      case destName PackageName.requiresTag tag of
        SOME v => SOME (PackageName.fromString v)
      | NONE => NONE;
in
  val requires = List.mapPartial destRequires
end;

(* ------------------------------------------------------------------------- *)
(* Shows.                                                                    *)
(* ------------------------------------------------------------------------- *)

fun toMapping tag =
    case destName PackageName.showTag tag of
      SOME v => SOME (Show.fromStringMapping v)
    | NONE => NONE;

fun fromMapping m =
    let
      val name = PackageName.showTag

      val value = Show.toStringMapping m
    in
      mk (Tag' {name = name, value = value})
    end;

fun toShow tags =
    let
      val ms = List.mapPartial toMapping tags
    in
      Show.fromList ms
    end;

fun fromShow show = List.map fromMapping (Show.toList show);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSeparator = Print.ppString separatorString;

fun pp tag =
    let
      val Tag' {name,value} = tag
    in
      Print.consistentBlock 0
        [PackageName.pp name,
         ppSeparator,
         Print.ppString " ",
         ppValue value]
    end;

fun ppList tags =
    case tags of
      [] => Print.skip
    | tag :: tags =>
      Print.consistentBlock 0
        (pp tag :: List.map (Print.sequence Print.newline o pp) tags);

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val separatorParser = exactString separatorString;

  val tagParser =
      (PackageName.parser ++ manySpace ++
       separatorParser ++ manySpace ++
       parserValue) >>
      (fn (n,((),((),((),v)))) => Tag' {name = n, value = v});

  val tagSpaceParser = tagParser ++ manySpace >> fst;
in
  val parser = manySpace ++ tagSpaceParser >> snd;

  val parserList = manySpace ++ many tagSpaceParser >> snd;
end;

end
