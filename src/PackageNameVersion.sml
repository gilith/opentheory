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

fun mk n' : nameVersion = n';

fun dest n : nameVersion' = n;

fun name' (NameVersion' {name = x, ...}) = x;

fun version' (NameVersion' {version = x, ...}) = x;

fun name n = name' (dest n);

fun version n = version' (dest n);

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (i1,i2) =
    let
      val NameVersion' {name = b1, version = v1} = dest i1
      and NameVersion' {name = b2, version = v2} = dest i2
    in
      case PackageName.compare (b1,b2) of
        LESS => LESS
      | EQUAL => PackageVersion.compare (v1,v2)
      | GREATER => GREATER
    end;

fun equal i1 i2 =
    let
      val NameVersion' {name = b1, version = v1} = dest i1
      and NameVersion' {name = b2, version = v2} = dest i2
    in
      PackageName.equal b1 b2 andalso
      PackageVersion.equal v1 v2
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSeparator = Print.ppString separatorString;

fun pp' n =
    let
      val NameVersion' {name = b, version = v} = n
    in
      Print.program
        [PackageName.pp b,
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
      (fn (b,((),v)) => NameVersion' {name = b, version = v});
in
  val parser = parser' >> mk;
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package name/version format: " ^ s);

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

  fun close f =
      let
        fun adds acc set = foldl check acc set

        and check (namever,acc) =
            if member namever acc then acc
            else expand (add acc namever) namever

        and expand acc namever = adds acc (f namever)
      in
        adds empty
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

  val pp =
      Print.ppMap
        toList
        (Print.ppBracket "{" "}" (Print.ppOpList "," PackageNameVersion.pp));

end

