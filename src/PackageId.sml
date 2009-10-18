(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE IDS                                     *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageId :> PackageId =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val separatorString = "-";

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun concatWith s =
    let
      fun add (x,l) = s :: x :: l
    in
      fn [] => ""
       | x :: xs =>
         let
           val xs = List.foldl add [] (rev xs)
         in
           String.concat (x :: xs)
         end
    end;

(* ------------------------------------------------------------------------- *)
(* A type of theory package ids.                                             *)
(* ------------------------------------------------------------------------- *)

datatype id =
    Id of
      {base : string,
       version : PackageVersion.version};

fun base (Id {base = x, ...}) = x;

fun version (Id {version = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (i1,i2) =
    let
      val Id {base = b1, version = v1} = i1
      and Id {base = b2, version = v2} = i2
    in
      case String.compare (b1,b2) of
        LESS => LESS
      | EQUAL => PackageVersion.compare (v1,v2)
      | GREATER => GREATER
    end;

fun equal i1 i2 =
    let
      val Id {base = b1, version = v1} = i1
      and Id {base = b2, version = v2} = i2
    in
      b1 = b2 andalso PackageVersion.equal v1 v2
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSeparator = Print.addString separatorString;

fun pp (Id {base = b, version = v}) =
    Print.program
      [Print.ppString b,
       ppSeparator,
       PackageVersion.pp v];

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

  val componentParser =
      let
        fun isInitialChar c = Char.isLower c

        fun isSubsequentChar c = Char.isLower c orelse Char.isDigit c
      in
        (some isInitialChar ++ many (some isSubsequentChar)) >>
        (fn (c,cs) => implode (c :: cs))
      end;

  val baseParser =
      componentParser ++
      many (separatorParser ++ componentParser >> snd) >>
      (fn (b,l) => concatWith separatorString (b :: l));
in
  val parser =
      baseParser ++
      separatorParser ++
      PackageVersion.parser >>
      (fn (b,((),v)) => Id {base = b, version = v});
end;

end
