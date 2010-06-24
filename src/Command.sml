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
and absTermCommandString = "absTerm"
and appCommandString = "app"
and appTermCommandString = "appTerm"
and assumeCommandString = "assume"
and axiomCommandString = "axiom"
and betaConvCommandString = "betaConv"
and consCommandString = "cons"
and constCommandString = "const"
and constTermCommandString = "constTerm"
and deductAntisymCommandString = "deductAntisym"
and defCommandString = "def"
and defineConstCommandString = "defineConst"
and defineTypeOpCommandString = "defineTypeOp"
and eqMpCommandString = "eqMp"
and negationChar = #"-"
and nilCommandString = "nil"
and opTypeCommandString = "opType"
and popCommandString = "pop"
and refCommandString = "ref"
and reflCommandString = "refl"
and removeCommandString = "remove"
and substCommandString = "subst"
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
  | Abs
  | AbsTerm
  | App
  | AppTerm
  | Assume
  | Axiom
  | BetaConv
  | Cons
  | Const
  | ConstTerm
  | DeductAntisym
  | Def
  | DefineConst
  | DefineTypeOp
  | EqMp
  | Nil
  | OpType
  | Pop
  | Ref
  | Refl
  | Remove
  | Subst
  | Thm
  | TypeOp
  | Var
  | VarTerm
  | VarType;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

fun compare cmd1_cmd2 =
    case cmd1_cmd2 of
      (Num n1, Num n2) => Int.compare (n1,n2)
    | (Num _, _) => LESS
    | (_, Num _) => GREATER
    | (Name n1, Name n2) => Name.compare (n1,n2)
    | (Name _, _) => LESS
    | (_, Name _) => GREATER
    | (Abs,Abs) => EQUAL
    | (Abs,_) => LESS
    | (_,Abs) => GREATER
    | (AbsTerm,AbsTerm) => EQUAL
    | (AbsTerm,_) => LESS
    | (_,AbsTerm) => GREATER
    | (App,App) => EQUAL
    | (App,_) => LESS
    | (_,App) => GREATER
    | (AppTerm,AppTerm) => EQUAL
    | (AppTerm,_) => LESS
    | (_,AppTerm) => GREATER
    | (Assume,Assume) => EQUAL
    | (Assume,_) => LESS
    | (_,Assume) => GREATER
    | (Axiom,Axiom) => EQUAL
    | (Axiom,_) => LESS
    | (_,Axiom) => GREATER
    | (BetaConv,BetaConv) => EQUAL
    | (BetaConv,_) => LESS
    | (_,BetaConv) => GREATER
    | (Cons,Cons) => EQUAL
    | (Cons,_) => LESS
    | (_,Cons) => GREATER
    | (Const,Const) => EQUAL
    | (Const,_) => LESS
    | (_,Const) => GREATER
    | (ConstTerm,ConstTerm) => EQUAL
    | (ConstTerm,_) => LESS
    | (_,ConstTerm) => GREATER
    | (DeductAntisym,DeductAntisym) => EQUAL
    | (DeductAntisym,_) => LESS
    | (_,DeductAntisym) => GREATER
    | (Def,Def) => EQUAL
    | (Def,_) => LESS
    | (_,Def) => GREATER
    | (DefineConst,DefineConst) => EQUAL
    | (DefineConst,_) => LESS
    | (_,DefineConst) => GREATER
    | (DefineTypeOp,DefineTypeOp) => EQUAL
    | (DefineTypeOp,_) => LESS
    | (_,DefineTypeOp) => GREATER
    | (EqMp,EqMp) => EQUAL
    | (EqMp,_) => LESS
    | (_,EqMp) => GREATER
    | (Nil,Nil) => EQUAL
    | (Nil,_) => LESS
    | (_,Nil) => GREATER
    | (OpType,OpType) => EQUAL
    | (OpType,_) => LESS
    | (_,OpType) => GREATER
    | (Pop,Pop) => EQUAL
    | (Pop,_) => LESS
    | (_,Pop) => GREATER
    | (Ref,Ref) => EQUAL
    | (Ref,_) => LESS
    | (_,Ref) => GREATER
    | (Refl,Refl) => EQUAL
    | (Refl,_) => LESS
    | (_,Refl) => GREATER
    | (Remove,Remove) => EQUAL
    | (Remove,_) => LESS
    | (_,Remove) => GREATER
    | (Subst,Subst) => EQUAL
    | (Subst,_) => LESS
    | (_,Subst) => GREATER
    | (Thm,Thm) => EQUAL
    | (Thm,_) => LESS
    | (_,Thm) => GREATER
    | (TypeOp,TypeOp) => EQUAL
    | (TypeOp,_) => LESS
    | (_,TypeOp) => GREATER
    | (Var,Var) => EQUAL
    | (Var,_) => LESS
    | (_,Var) => GREATER
    | (VarTerm,VarTerm) => EQUAL
    | (VarTerm,_) => LESS
    | (_,VarTerm) => GREATER
    | (VarType,VarType) => EQUAL;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppAbsCommand = Print.addString absCommandString
and ppAbsTermCommand = Print.addString absTermCommandString
and ppAppCommand = Print.addString appCommandString
and ppAppTermCommand = Print.addString appTermCommandString
and ppAssumeCommand = Print.addString assumeCommandString
and ppAxiomCommand = Print.addString axiomCommandString
and ppBetaConvCommand = Print.addString betaConvCommandString
and ppConsCommand = Print.addString consCommandString
and ppConstCommand = Print.addString constCommandString
and ppConstTermCommand = Print.addString constTermCommandString
and ppDeductAntisymCommand = Print.addString deductAntisymCommandString
and ppDefCommand = Print.addString defCommandString
and ppDefineConstCommand = Print.addString defineConstCommandString
and ppDefineTypeOpCommand = Print.addString defineTypeOpCommandString
and ppEqMpCommand = Print.addString eqMpCommandString
and ppNilCommand = Print.addString nilCommandString
and ppOpTypeCommand = Print.addString opTypeCommandString
and ppPopCommand = Print.addString popCommandString
and ppRefCommand = Print.addString refCommandString
and ppReflCommand = Print.addString reflCommandString
and ppRemoveCommand = Print.addString removeCommandString
and ppSubstCommand = Print.addString substCommandString
and ppThmCommand = Print.addString thmCommandString
and ppTypeOpCommand = Print.addString typeOpCommandString
and ppVarCommand = Print.addString varCommandString
and ppVarTermCommand = Print.addString varTermCommandString
and ppVarTypeCommand = Print.addString varTypeCommandString;

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
    | Abs => ppAbsCommand
    | AbsTerm => ppAbsTermCommand
    | App => ppAppCommand
    | AppTerm => ppAppTermCommand
    | Assume => ppAssumeCommand
    | Axiom => ppAxiomCommand
    | BetaConv => ppBetaConvCommand
    | Cons => ppConsCommand
    | Const => ppConstCommand
    | ConstTerm => ppConstTermCommand
    | DeductAntisym => ppDeductAntisymCommand
    | Def => ppDefCommand
    | DefineConst => ppDefineConstCommand
    | DefineTypeOp => ppDefineTypeOpCommand
    | EqMp => ppEqMpCommand
    | Nil => ppNilCommand
    | OpType => ppOpTypeCommand
    | Pop => ppPopCommand
    | Ref => ppRefCommand
    | Refl => ppReflCommand
    | Remove => ppRemoveCommand
    | Subst => ppSubstCommand
    | Thm => ppThmCommand
    | TypeOp => ppTypeOpCommand
    | Var => ppVarCommand
    | VarTerm => ppVarTermCommand
    | VarType => ppVarTypeCommand;

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

  val absCommandParser = exactString absCommandString
  and absTermCommandParser = exactString absTermCommandString
  and appCommandParser = exactString appCommandString
  and appTermCommandParser = exactString appTermCommandString
  and assumeCommandParser = exactString assumeCommandString
  and axiomCommandParser = exactString axiomCommandString
  and betaConvCommandParser = exactString betaConvCommandString
  and consCommandParser = exactString consCommandString
  and constCommandParser = exactString constCommandString
  and constTermCommandParser = exactString constTermCommandString
  and deductAntisymCommandParser = exactString deductAntisymCommandString
  and defCommandParser = exactString defCommandString
  and defineConstCommandParser = exactString defineConstCommandString
  and defineTypeOpCommandParser = exactString defineTypeOpCommandString
  and eqMpCommandParser = exactString eqMpCommandString
  and nilCommandParser = exactString nilCommandString
  and opTypeCommandParser = exactString opTypeCommandString
  and popCommandParser = exactString popCommandString
  and refCommandParser = exactString refCommandString
  and reflCommandParser = exactString reflCommandString
  and removeCommandParser = exactString removeCommandString
  and substCommandParser = exactString substCommandString
  and thmCommandParser = exactString thmCommandString
  and typeOpCommandParser = exactString typeOpCommandString
  and varCommandParser = exactString varCommandString
  and varTermCommandParser = exactString varTermCommandString
  and varTypeCommandParser = exactString varTypeCommandString;
in
  val parser =
      (* Special command parsers *)
      numParser >> Num ||
      nameParser >> Name ||
      (* Regular command parsers are sorted by length to avoid prefix matching *)
      (* Commands of length 13 *)
      deductAntisymCommandParser >> K DeductAntisym ||
      (* Commands of length 12 *)
      defineTypeOpCommandParser >> K DefineTypeOp ||
      (* Commands of length 11 *)
      defineConstCommandParser >> K DefineConst ||
      (* Commands of length 9 *)
      constTermCommandParser >> K ConstTerm ||
      (* Commands of length 8 *)
      betaConvCommandParser >> K BetaConv ||
      (* Commands of length 7 *)
      absTermCommandParser >> K AbsTerm ||
      appTermCommandParser >> K AppTerm ||
      varTermCommandParser >> K VarTerm ||
      varTypeCommandParser >> K VarType ||
      (* Commands of length 6 *)
      assumeCommandParser >> K Assume ||
      opTypeCommandParser >> K OpType ||
      removeCommandParser >> K Remove ||
      typeOpCommandParser >> K TypeOp ||
      (* Commands of length 5 *)
      axiomCommandParser >> K Axiom ||
      constCommandParser >> K Const ||
      (* Commands of length 4 *)
      consCommandParser >> K Cons ||
      eqMpCommandParser >> K EqMp ||
      (* Commands of length 3 *)
      absCommandParser >> K Abs ||
      appCommandParser >> K App ||
      defCommandParser >> K Def ||
      nilCommandParser >> K Nil ||
      popCommandParser >> K Pop ||
      refCommandParser >> K Ref ||
      thmCommandParser >> K Thm ||
      varCommandParser >> K Var;

  val spacedParser =
      (manySpace ++ parser ++ manySpace) >> (fn ((),(t,())) => [t]);
end;

end

structure CommandOrdered =
struct type t = Command.command val compare = Command.compare end

structure CommandSet = ElementSet (CommandOrdered)

structure CommandMap = KeyMap (CommandOrdered)
