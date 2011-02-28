(* ========================================================================= *)
(* PACKAGE NAME/VERSIONS                                                     *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
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

  fun previousVersion m nv =
      let
        val n = PackageNameVersion.name nv
        and v = PackageNameVersion.version nv

        fun sameName (nv',_) = PackageNameVersion.equalName n nv'

        fun earlier (nv',_) =
            let
              val v' = PackageNameVersion.version nv'
            in
              case PackageVersion.compare (v',v) of
                LESS => true
              | _ => false
            end

        val m = filter sameName m
      in
        findr earlier m
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

  fun preOrder children set =
      let
        fun dfsCheck (namever,(seen,acc)) =
            if member namever seen then (seen,acc)
            else dfsNameVersion (seen,acc) namever

        and dfsNameVersion (seen,acc) namever =
            let
              val seen = add seen namever

              val (seen,acc) = dfsSet (seen,acc) (children namever)

              val acc = if member namever set then namever :: acc else acc
            in
              (seen,acc)
            end

        and dfsSet seen_acc namevers = foldl dfsCheck seen_acc namevers

        val (_,acc) = dfsSet (empty,[]) set
      in
        acc
      end;

  fun postOrder children set = rev (preOrder children set);

  fun latestVersion set n =
      let
        val nvs = filter (PackageNameVersion.equalName n) set
      in
        findr (Useful.K true) nvs
      end;

  fun previousVersion set nv =
      let
        val n = PackageNameVersion.name nv
        and v = PackageNameVersion.version nv

        fun sameName nv' = PackageNameVersion.equalName n nv'

        fun earlier nv' =
            let
              val v' = PackageNameVersion.version nv'
            in
              case PackageVersion.compare (v',v) of
                LESS => true
              | _ => false
            end

        val set = filter sameName set
      in
        findr earlier set
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
