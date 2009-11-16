(* ========================================================================= *)
(* NAME/VALUE TAGS                                                           *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Tag :> Tag =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val separatorString = ":";

(* ------------------------------------------------------------------------- *)
(* Types of theory package syntax.                                           *)
(* ------------------------------------------------------------------------- *)

datatype tag' =
    Tag' of
      {name : string,
       value : string};

type tag = tag';

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk t : tag = t;

fun dest t : tag' = t;

fun name (Tag' {name = x, ...}) = x;

fun value (Tag' {value = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (t1,t2) =
    let
      val Tag' {name = n1, value = v1} = t1
      and Tag' {name = n2, value = v2} = t2
    in
      case String.compare (n1,n2) of
        LESS => LESS
      | EQUAL => String.compare (v1,v2)
      | GREATER => GREATER
    end;

fun equal (t1 : tag) t2 = t1 = t2;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppSeparator = Print.addString (separatorString ^ " ");

fun pp tag =
    let
      val Tag' {name,value} = tag
    in
      Print.blockProgram Print.Consistent 0
        [Print.addString name,
         ppSeparator,
         Print.addString value]
    end;

fun ppList tags =
    case tags of
      [] => Print.skip
    | tag :: tags =>
      Print.blockProgram Print.Consistent 0
        (pp tag :: map (Print.sequence Print.addNewline o pp) tags);

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

  val nameParser =
      let
        fun isInitialChar c = Char.isLower c

        fun isSubsequentChar c = Char.isAlphaNum c
      in
        (some isInitialChar ++ many (some isSubsequentChar)) >>
        (fn (c,cs) => implode (c :: cs))
      end;

  val valueParser =
      let
        fun isValueChar c = c <> #"\n"
      in
        many (some isValueChar) >> implode
      end;

  val tagParser =
      (nameParser ++ manySpace ++
       separatorParser ++ manySpace ++
       valueParser) >>
      (fn (n,((),((),((),v)))) => Tag' {name = n, value = v});

  val tagSpaceParser = tagParser ++ manySpace >> fst;
in
  val parser = manySpace ++ tagSpaceParser >> snd;

  val parserList = manySpace ++ many tagSpaceParser >> snd;
end;

end
