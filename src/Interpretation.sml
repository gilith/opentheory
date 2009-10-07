(* ========================================================================= *)
(* INTERPRETING OPENTHEORY NAMES                                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Interpretation :> Interpretation =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val rewriteString = "->";

val terminatorString = ";";

(* ------------------------------------------------------------------------- *)
(* A type of rewrite rules for names.                                        *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    TypeOpRewrite of Name.name * Name.name
  | ConstRewrite of Name.name * Name.name;

local
  fun ppName2 prefix (x,y) =
      Print.blockProgram Print.Inconsistent 2
        [Print.addString prefix,
         Print.addBreak 1,
         Name.ppQuoted x,
         Print.addString " ",
         Print.addString rewriteString,
         Print.addBreak 1,
         Name.ppQuoted y,
         Print.addString terminatorString];
in
  fun ppRewrite r =
      case r of
        TypeOpRewrite x_y => ppName2 "type" x_y
      | ConstRewrite x_y => ppName2 "const" x_y
end;

fun ppRewriteList rs =
    case rs of
      [] => Print.skip
    | r :: rs =>
      Print.blockProgram Print.Consistent 0
        (ppRewrite r ::
         map (Print.sequence (Print.addBreak 1) o ppRewrite) rs);

val toStringRewrite = Print.toString ppRewrite;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  fun nameParser prefix =
      (exactString prefix ++ manySpace ++
       Name.quotedParser ++ manySpace ++
       exactString rewriteString ++ manySpace ++
       Name.quotedParser ++ manySpace ++
       exactString terminatorString) >>
      (fn ((),((),(x,((),((),((),(y,((),())))))))) => (x,y));

  val rewriteParser =
      nameParser "type" >> TypeOpRewrite ||
      nameParser "const" >> ConstRewrite;

  val rewriteSpaceParser = rewriteParser ++ manySpace >> fst;

  val rewriteListSpaceParser = many rewriteSpaceParser;
in
  val parserRewrite = manySpace ++ rewriteSpaceParser >> snd;

  val parserRewriteList = manySpace ++ rewriteListSpaceParser >> snd;
end;

(* ------------------------------------------------------------------------- *)
(* A type of interpretations (bad pun on interpreting art (article files)).  *)
(* ------------------------------------------------------------------------- *)

datatype interpretation =
    Interpretation of
      {typeOps : Name.name NameMap.map,
       consts : Name.name NameMap.map}

(* ------------------------------------------------------------------------- *)
(* Normalizing interpretations (removing "x" -> "x" rewrites).               *)
(* ------------------------------------------------------------------------- *)

local
  fun differentName (x,y) = not (Name.equal x y);

  val normalizeName = NameMap.filter differentName;
in
  fun normalize int =
      let
        val Interpretation {typeOps,consts} = int

        val typeOps = normalizeName typeOps

        val consts = normalizeName consts
      in
        Interpretation
          {typeOps = typeOps,
           consts = consts}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val natural =
    let
      val typeOps = NameMap.new ()
      val consts = NameMap.new ()
    in
      Interpretation
        {typeOps = typeOps,
         consts = consts}
    end;

local
  fun addName m x_y =
      let
        val (x,_) = x_y
        val _ = not (NameMap.inDomain x m) orelse
                raise Error "Interpretation.add: duplicate"
      in
        NameMap.insert m x_y
      end;

  fun addRewrite (rw,int) =
      let
        val Interpretation {typeOps,consts} = int
      in
        case rw of
          TypeOpRewrite x_y =>
          let
            val typeOps = addName typeOps x_y
          in
            Interpretation
              {typeOps = typeOps,
               consts = consts}
          end
        | ConstRewrite x_y =>
          let
            val consts = addName consts x_y
          in
            Interpretation
              {typeOps = typeOps,
               consts = consts}
          end
      end;
in
  fun fromRewriteList rws = normalize (List.foldl addRewrite natural rws);
end;

local
  fun addTypeOp (x,y,l) = TypeOpRewrite (x,y) :: l;

  fun addConst (x,y,l) = ConstRewrite (x,y) :: l;
in
  fun toRewriteList int =
    let
      val Interpretation {typeOps,consts} = int

      val rws = []

      val rws = NameMap.foldr addConst rws consts

      val rws = NameMap.foldr addTypeOp rws typeOps
    in
      rws
    end;
end;

(* ------------------------------------------------------------------------- *)
(* Translating OpenTheory names.                                             *)
(* ------------------------------------------------------------------------- *)

fun interpretName m x =
    case NameMap.peek m x of
      SOME y => y
    | NONE => x;

fun interpretTypeOp int x =
    let
      val Interpretation {typeOps,...} = int
    in
      interpretName typeOps x
    end;

fun interpretConst int x =
    let
      val Interpretation {consts,...} = int
    in
      interpretName consts x
    end;

(* ------------------------------------------------------------------------- *)
(* Composing interpretations.                                                *)
(* ------------------------------------------------------------------------- *)

local
  fun composeNames m1 m2 =
      let
        fun add1 (x,y,m) =
            let
              val z = interpretName m2 y
            in
              if Name.equal x z then m else NameMap.insert m (x,z)
            end

        fun add2 (x,y,m) =
            if NameMap.inDomain x m1 then m else NameMap.insert m (x,y)

        val m = NameMap.new ()

        val m = NameMap.foldl add1 m m1

        val m = NameMap.foldl add2 m m2
      in
        m
      end;
in
  fun compose int1 int2 =
      let
        val Interpretation {typeOps = t1, consts = c1} = int1
        and Interpretation {typeOps = t2, consts = c2} = int2

        val typeOps = composeNames t1 t2

        val consts = composeNames c1 c2
      in
        Interpretation
          {typeOps = typeOps,
           consts = consts}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap toRewriteList ppRewriteList;

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
in
  val parser = parserRewriteList >> fromRewriteList;

  val parser' = parser >> (fn int => [int]);
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {interpretation,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp interpretation);

local
  (* Comment lines *)

  fun isComment l =
      case List.find (not o Char.isSpace) l of
        NONE => true
      | SOME #"#" => true
      | _ => false;
in
  fun fromTextFile {filename} =
      let
        (* Estimating parse error line numbers *)

        val lines = Stream.fromTextFile {filename = filename}

        val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
      in
        (let
           (* The character stream *)

           val chars = Stream.filter (not o isComment) chars

           val chars = Parse.everything Parse.any chars

           (* The interpretation stream *)

           val ints = Parse.everything parser' chars
         in
           case Stream.toList ints of
             [] => raise Bug "Interpretation.fromTextFile: no interpretation"
           | [int] => int
           | _ :: _ :: _ =>
             raise Bug "Interpretation.fromTextFile: multiple interpretation"
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in interpretation file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

end
