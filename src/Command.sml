(* ========================================================================= *)
(* OPENTHEORY COMMANDS                                                       *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Command :> Command =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val absCommandString = "abs"
and appCommandString = "app"
and callCommandString = "call"
and consCommandString = "cons"
and constCommandString = "const"
and defCommandString = "def"
and errorCommandString = "error"
and negationChar = #"-"
and nilCommandString = "nil"
and popCommandString = "pop"
and refCommandString = "ref"
and removeCommandString = "remove"
and returnCommandString = "return"
and saveCommandString = "save"
and thmCommandString = "thm"
and typeVarCommandString = "type_var"
and typeOpCommandString = "type_op"
and varCommandString = "var";

(* ------------------------------------------------------------------------- *)
(* A type of commands.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype command =
    Num of int
  | Name of Name.name
  | Error
  | Nil
  | Cons
  | TypeVar
  | TypeOp
  | Var
  | Const
  | App
  | Abs
  | Thm
  | Call
  | Return
  | Def
  | Ref
  | Remove
  | Pop
  | Save;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppPos n = Print.ppInt n;
in
  fun ppNum i =
      if i >= 0 then ppPos i
      else Print.sequence (Print.ppChar negationChar) (ppPos (~i));
end;

fun pp cmd =
    case cmd of
      Num i => ppNum i
    | Name n => Name.ppQuoted n
    | Error => Print.ppString errorCommandString
    | Nil => Print.ppString nilCommandString
    | Cons => Print.ppString consCommandString
    | TypeVar => Print.ppString typeVarCommandString
    | TypeOp => Print.ppString typeOpCommandString
    | Var => Print.ppString varCommandString
    | Const => Print.ppString constCommandString
    | App => Print.ppString appCommandString
    | Abs => Print.ppString absCommandString
    | Thm => Print.ppString thmCommandString
    | Call => Print.ppString callCommandString
    | Return => Print.ppString returnCommandString
    | Def => Print.ppString defCommandString
    | Ref => Print.ppString refCommandString
    | Remove => Print.ppString removeCommandString
    | Pop => Print.ppString popCommandString
    | Save => Print.ppString saveCommandString;

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

  local
    val zeroParser = exactChar #"0" >> K 0;

    val nonzeroParser =
        exactChar #"1" >> K 1 ||
        exactChar #"2" >> K 2 ||
        exactChar #"3" >> K 3 ||
        exactChar #"4" >> K 4 ||
        exactChar #"5" >> K 5 ||
        exactChar #"6" >> K 6 ||
        exactChar #"7" >> K 7 ||
        exactChar #"8" >> K 8 ||
        exactChar #"9" >> K 9;

    val digitParser = zeroParser || nonzeroParser;

    fun mkNum acc l =
        case l of
          [] => acc
        | d :: l => mkNum (10 * acc + d) l;

    val posParser = nonzeroParser ++ many digitParser >> uncurry mkNum;

    val negParser = exactChar negationChar ++ posParser >> (fn (_,i) => ~i);
  in
    val numParser = negParser || zeroParser || posParser;
  end;

  val nameParser = Name.quotedParser;

  fun commandParser s c = exactString s >> K c;
in
  val parser =
      numParser >> Num ||
      nameParser >> Name ||
      (* Sorted in decreasing length order *)
      commandParser typeVarCommandString TypeVar ||
      commandParser typeOpCommandString TypeOp ||
      commandParser removeCommandString Remove ||
      commandParser returnCommandString Return ||
      commandParser constCommandString Const ||
      commandParser errorCommandString Error ||
      commandParser callCommandString Call ||
      commandParser appCommandString App ||
      commandParser consCommandString Cons ||
      commandParser saveCommandString Save ||
      commandParser absCommandString Abs ||
      commandParser defCommandString Def ||
      commandParser nilCommandString Nil ||
      commandParser popCommandString Pop ||
      commandParser refCommandString Ref ||
      commandParser thmCommandString Thm ||
      commandParser varCommandString Var;

  val spacedParser =
      (manySpace ++ parser ++ manySpace) >> (fn ((),(t,())) => [t]);
end;

end
