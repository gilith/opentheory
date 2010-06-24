(* ========================================================================= *)
(* OPENTHEORY COMMANDS                                                       *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Command =
sig

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
  | VarType

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : command * command -> order

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : command Print.pp

val toString : command -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,command) Parse.parser

val spacedParser : (char, command list) Parse.parser

end
