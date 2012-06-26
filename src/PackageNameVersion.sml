(* ========================================================================= *)
(* PACKAGE NAME/VERSIONS                                                     *)
(* Copyright (c) 2009 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure PackageNameVersion :> PackageNameVersion =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val separatorString = "-";

(* ------------------------------------------------------------------------- *)
(* A type of theory package name/versions.                                   *)
(* ------------------------------------------------------------------------- *)

datatype nameVersion' =
    NameVersion' of
      {name : PackageName.name,
       version : PackageVersion.version};

type nameVersion = nameVersion';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk nv' : nameVersion = nv';

fun dest nv : nameVersion' = nv;

fun name' (NameVersion' {name = x, ...}) = x;

fun version' (NameVersion' {version = x, ...}) = x;

fun name nv = name' (dest nv);

fun version nv = version' (dest nv);

fun equalName n nv = PackageName.equal n (name nv);

fun equalVersion v nv = PackageVersion.equal v (version nv);

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (nv1,nv2) =
    let
      val NameVersion' {name = b1, version = v1} = dest nv1
      and NameVersion' {name = b2, version = v2} = dest nv2
    in
      case PackageName.compare (b1,b2) of
        LESS => LESS
      | EQUAL => PackageVersion.compare (v1,v2)
      | GREATER => GREATER
    end;

fun equal nv1 nv2 =
    let
      val NameVersion' {name = b1, version = v1} = dest nv1
      and NameVersion' {name = b2, version = v2} = dest nv2
    in
      PackageName.equal b1 b2 andalso
      PackageVersion.equal v1 v2
    end;

(* ------------------------------------------------------------------------- *)
(* Prefix names.                                                             *)
(* ------------------------------------------------------------------------- *)

fun isPrefixName nv1 nv2 =
    PackageName.isPrefix (name nv1) (name nv2);

fun isStrictPrefixName nv1 nv2 =
    PackageName.isStrictPrefix (name nv1) (name nv2);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSeparator = Print.ppString separatorString;

fun pp' nv =
    let
      val NameVersion' {name = n, version = v} = nv
    in
      Print.program
        [PackageName.pp n,
         ppSeparator,
         PackageVersion.pp v]
    end;

val pp = Print.ppMap dest pp';

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Converting to a logic name.                                               *)
(* ------------------------------------------------------------------------- *)

fun toGlobal nv = Name.mkGlobal (toString nv);

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

  val parser' =
      PackageName.parser ++
      separatorParser ++
      PackageVersion.parser >>
      (fn (n,((),v)) => NameVersion' {name = n, version = v});
in
  val parser = parser' >> mk;
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package NAME-VERSION format: " ^ s);

end

structure PackageNameVersionOrdered =
struct
  type t = PackageNameVersion.nameVersion;
  val compare = PackageNameVersion.compare;
end

structure PackageNameVersionMap =
struct

  local
    structure S = KeyMap (PackageNameVersionOrdered);
  in
    open S;
  end;

  fun previousNameVersion m namever =
      let
        val PackageNameVersion.NameVersion' {name,version} =
            PackageNameVersion.dest namever

        fun earlier (nv,_) =
            let
              val PackageNameVersion.NameVersion' {name = n, version = v} =
                  PackageNameVersion.dest nv
            in
              PackageName.equal name n andalso
              case PackageVersion.compare (v,version) of
                LESS => true
              | _ => false
            end
      in
        findr earlier m
      end;

  fun latestNameVersion m name =
      let
        fun pred (nv,_) = PackageNameVersion.equalName name nv
      in
        findr pred m
      end;

  local
    fun toStrm oiter =
        case oiter of
          NONE => Stream.Nil
        | SOME iter => Stream.Cons (readIterator iter, toCons iter)

    and toCons iter () = toStrm (advanceIterator iter);
  in
    fun toStream m = toStrm (mkIterator m);
  end;

end

structure PackageNameVersionSet =
struct

  local
    structure S = ElementSet (PackageNameVersionMap);
  in
    open S;
  end;

  val name =
      let
        fun inc (nv,s) = PackageNameSet.add s (PackageNameVersion.name nv)
      in
        foldl inc PackageNameSet.empty
      end;

  fun previousNameVersion set namever =
      let
        val PackageNameVersion.NameVersion' {name,version} =
            PackageNameVersion.dest namever

        fun earlier nv =
            let
              val PackageNameVersion.NameVersion' {name = n, version = v} =
                  PackageNameVersion.dest nv
            in
              PackageName.equal name n andalso
              case PackageVersion.compare (v,version) of
                LESS => true
              | _ => false
            end
      in
        findr earlier set
      end;

  fun latestNameVersion set name =
      findr (PackageNameVersion.equalName name) set;

  local
    fun incNew (namever,name_acc) =
        let
          val (name,acc) = name_acc

          val n = PackageNameVersion.name namever
        in
          if PackageName.equal n name then name_acc else (n, add acc namever)
        end;
  in
    fun latestVersions set =
        case findr (Useful.K true) set of
          NONE => empty
        | SOME namever =>
          let
            val name_acc = (PackageNameVersion.name namever, singleton namever)

            val (_,acc) = foldr incNew name_acc set
          in
            acc
          end
  end;

  val pp =
      Print.ppMap
        toList
        (Print.ppBracket "{" "}" (Print.ppOpList "," PackageNameVersion.pp));

end

structure PackageNameVersionGraph =
VertexGraph (
  structure KM = PackageNameVersionMap
  and ES = PackageNameVersionSet
);
