(* ========================================================================= *)
(* PACKAGE AUTHORS                                                           *)
(* Copyright (c) 2012 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageAuthor :> PackageAuthor =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val emailStartChar = #"<"
and emailEndChar = #">";

(* ------------------------------------------------------------------------- *)
(* A type of theory package authors.                                         *)
(* ------------------------------------------------------------------------- *)

datatype author' =
    Author' of
      {name : string,
       email : string};

type author = author';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk auth' : author = auth';

fun dest auth : author' = auth;

fun name' (Author' {name = x, ...}) = {name = x};

fun email' (Author' {email = x, ...}) = {email = x};

fun name auth = name' (dest auth);

fun email auth = email' (dest auth);

fun equalName n auth = name auth = n;

fun equalEmail e auth = email auth = e;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (auth1,auth2) =
    let
      val Author' {name = n1, email = e1} = dest auth1
      and Author' {name = n2, email = e2} = dest auth2
    in
      case String.compare (n1,n2) of
        LESS => LESS
      | EQUAL => String.compare (e1,e2)
      | GREATER => GREATER
    end;

fun equal (auth1 : author) auth2 = auth1 = auth2;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppEmailStart = Print.ppChar emailStartChar
and ppEmailEnd = Print.ppChar emailEndChar;

fun pp' auth =
    let
      val Author' {name = n, email = e} = auth
    in
      Print.program
        [Print.ppString n,
         Print.space,
         ppEmailStart,
         Print.ppString e,
         ppEmailEnd]
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

  val emailStartParser = exactChar emailStartChar
  and emailEndParser = exactChar emailEndChar
  and spaceParser = exactChar #" ";

  fun isNameChar c =
      Char.isAlpha c orelse
      Char.isDigit c orelse
      c = #"-" orelse
      c = #"'";

  fun isEmailChar c =
      Char.isAlpha c orelse
      Char.isDigit c orelse
      c = #"_" orelse
      c = #"." orelse
      c = #"@" orelse
      c = #"-";

  val nameComponentParser = atLeastOne (some isNameChar);

  val spaceNameComponentParser =
      spaceParser ++ nameComponentParser >> (fn ((),cl) => #" " :: cl);

  val nameParser =
      nameComponentParser ++ many spaceNameComponentParser >>
      (fn (s,sl) => String.implode (List.concat (s :: sl)));

  val emailParser = atLeastOne (some isEmailChar) >> String.implode;

  val parser' =
      nameParser ++
      spaceParser ++
      emailStartParser ++
      emailParser ++
      emailEndParser >>
      (fn (n,((),((),(e,())))) => Author' {name = n, email = e});
in
  val parser = parser' >> mk;
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package author NAME <EMAIL> format: " ^ s);

end

structure PackageAuthorOrdered =
struct type t = PackageAuthor.author val compare = PackageAuthor.compare end

structure PackageAuthorMap = KeyMap (PackageAuthorOrdered)

structure PackageAuthorSet =
struct

  local
    structure S = ElementSet (PackageAuthorMap);
  in
    open S;
  end;

  val pp =
      Print.ppMap
        toList
        (Print.ppBracket "{" "}" (Print.ppOpList "," PackageAuthor.pp));

end
