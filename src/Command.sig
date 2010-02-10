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
  | VarType

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
