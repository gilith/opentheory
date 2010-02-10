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

val absTermCommandString = "absTerm"
and appTermCommandString = "appTerm"
and callCommandString = "call"
and consCommandString = "cons"
and constCommandString = "const"
and constTermCommandString = "constTerm"
and defCommandString = "def"
and errorCommandString = "error"
and negationChar = #"-"
and nilCommandString = "nil"
and opTypeCommandString = "opType"
and popCommandString = "pop"
and refCommandString = "ref"
and removeCommandString = "remove"
and returnCommandString = "return"
and saveCommandString = "save"
and thmCommandString = "thm"
and typeVarCommandString = "typeVar"
and typeOpCommandString = "typeOp"
and varCommandString = "var"
and varTermCommandString = "varTerm"
and varTypeCommandString = "varType";

(* ------------------------------------------------------------------------- *)
(* A type of commands.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype command =
  (* Special commands *)
    Num of int
  | Name of Name.name
  (* Regular commands *)
  | AbsTerm
  | AppTerm
  | Call
  | Cons
  | Const
  | ConstTerm
  | Def
  | Error
  | Nil
  | OpType
  | Pop
  | Ref
  | Remove
  | Return
  | Save
  | Thm
  | TypeOp
  | Var
  | VarTerm
  | VarType;

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
    (* Special commands *)
      Num i => ppNum i
    | Name n => Name.ppQuoted n
    (* Regular commands *)
    | AbsTerm => Print.ppString absTermCommandString
    | AppTerm => Print.ppString appTermCommandString
    | Call => Print.ppString callCommandString
    | Cons => Print.ppString consCommandString
    | Const => Print.ppString constCommandString
    | ConstTerm => Print.ppString constTermCommandString
    | Def => Print.ppString defCommandString
    | Error => Print.ppString errorCommandString
    | Nil => Print.ppString nilCommandString
    | OpType => Print.ppString opTypeCommandString
    | Pop => Print.ppString popCommandString
    | Ref => Print.ppString refCommandString
    | Remove => Print.ppString removeCommandString
    | Return => Print.ppString returnCommandString
    | Save => Print.ppString saveCommandString
    | Thm => Print.ppString thmCommandString
    | TypeOp => Print.ppString typeOpCommandString
    | Var => Print.ppString varCommandString
    | VarTerm => Print.ppString varTermCommandString
    | VarType => Print.ppString varTypeCommandString;

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
      (* Special command parsers *)
      numParser >> Num ||
      nameParser >> Name ||
      (* Regular command parsers are sorted by length to avoid prefix matching *)
      (* Commands of length 9 *)
      commandParser constTermCommandString ConstTerm ||
      (* Commands of length 7 *)
      commandParser absTermCommandString AbsTerm ||
      commandParser appTermCommandString AppTerm ||
      commandParser varTermCommandString VarTerm ||
      commandParser varTypeCommandString VarType ||
      (* Commands of length 6 *)
      commandParser opTypeCommandString OpType ||
      commandParser removeCommandString Remove ||
      commandParser returnCommandString Return ||
      commandParser typeOpCommandString TypeOp ||
      (* Commands of length 5 *)
      commandParser constCommandString Const ||
      commandParser errorCommandString Error ||
      (* Commands of length 4 *)
      commandParser callCommandString Call ||
      commandParser consCommandString Cons ||
      commandParser saveCommandString Save ||
      (* Commands of length 3 *)
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
