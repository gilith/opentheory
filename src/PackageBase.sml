(* ========================================================================= *)
(* PACKAGE BASE NAMES                                                        *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageBase :> PackageBase =
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
(* A type of theory package names.                                           *)
(* ------------------------------------------------------------------------- *)

type base = string;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = String.compare;

fun equal (b1 : base) b2 = b1 = b2;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppString;

fun toString (b : base) = b;

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
in
  val parser =
      componentParser ++
      many (separatorParser ++ componentParser >> snd) >>
      (fn (b,l) => concatWith separatorString (b :: l));
end;

fun fromString s =
    Parse.fromString parser s
    handle Parse.NoParse =>
      raise Error ("bad package base name format: " ^ s);

end

structure PackageBaseOrdered =
struct type t = PackageBase.base val compare = PackageBase.compare end

structure PackageBaseSet = ElementSet (PackageBaseOrdered)

structure PackageBaseMap = KeyMap (PackageBaseOrdered)
