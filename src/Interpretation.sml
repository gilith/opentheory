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

val constString = "const"
and rewriteString = "as"
and typeOpString = "type";

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
         Name.ppQuoted y];
in
  fun ppRewrite r =
      case r of
        TypeOpRewrite x_y => ppName2 typeOpString x_y
      | ConstRewrite x_y => ppName2 constString x_y
end;

fun ppRewriteList rs =
    case rs of
      [] => Print.skip
    | r :: rs =>
      Print.blockProgram Print.Consistent 0
        (ppRewrite r ::
         map (Print.sequence Print.addNewline o ppRewrite) rs);

val toStringRewrite = Print.toString ppRewrite;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val constParser = exactString constString
  and rewriteParser = exactString rewriteString
  and typeOpParser = exactString typeOpString;

  fun nameParser prefixParser =
      (prefixParser ++ manySpace ++
       Name.quotedParser ++ manySpace ++
       rewriteParser ++ manySpace ++
       Name.quotedParser) >>
      (fn ((),((),(x,((),((),((),y)))))) => (x,y));

  val rewriteParser =
      nameParser typeOpParser >> TypeOpRewrite ||
      nameParser constParser >> ConstRewrite;

  val rewriteSpaceParser = rewriteParser ++ manySpace >> fst;

  val rewriteListSpaceParser = many rewriteSpaceParser;
in
  val parserRewrite = manySpace ++ rewriteSpaceParser >> snd;

  val parserRewrite' = parserRewrite >> (fn rw => [rw]);

  val parserRewriteList = manySpace ++ rewriteListSpaceParser >> snd;
end;

(* ------------------------------------------------------------------------- *)
(* A type of interpretations (bad pun on interpreting art(icle) files).      *)
(* ------------------------------------------------------------------------- *)

datatype interpretation =
    Interpretation of
      {typeOps : Name.name NameMap.map,
       consts : Name.name NameMap.map}

local
  val compareName = NameMap.compare Name.compare;
in
  fun compare (int1,int2) =
      let
        val Interpretation {typeOps = tyOps1, consts = cons1} = int1
        and Interpretation {typeOps = tyOps2, consts = cons2} = int2
      in
        case compareName (tyOps1,tyOps2) of
          LESS => LESS
        | EQUAL => compareName (cons1,cons2)
        | GREATER => GREATER
      end;
end;

local
  val equalName = NameMap.equal Name.equal;
in
  fun equal int1 int2 =
      let
        val Interpretation {typeOps = tyOps1, consts = cons1} = int1
        and Interpretation {typeOps = tyOps2, consts = cons2} = int2
      in
        equalName tyOps1 tyOps2 andalso
        equalName cons1 cons2
      end;
end;

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
  fun fromRewriteList rws =
      let
(*OpenTheoryTrace3
        val _ = Print.trace ppRewriteList "Interpretation.fromRewriteList" rws
*)
      in
        normalize (List.foldl addRewrite natural rws)
      end;
end;

fun fromRewriteStream strm = fromRewriteList (Stream.toList strm);

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
(* Restricting interpretations.                                              *)
(* ------------------------------------------------------------------------- *)

fun restrict sym =
    let
      fun knownTypeOp (t,_) = Symbol.knownTypeOp sym t

      fun knownConst (c,_) = Symbol.knownConst sym c
    in
      fn Interpretation {typeOps,consts} =>
         let
           val typeOps = NameMap.filter knownTypeOp typeOps

           val consts = NameMap.filter knownConst consts
         in
           Interpretation
             {typeOps = typeOps,
              consts = consts}
         end
    end;

local
  fun sameName m1 m2 n =
      case (NameMap.peek m1 n, NameMap.peek m2 n) of
        (NONE,NONE) => true
      | (NONE, SOME _) => false
      | (SOME _, NONE) => false
      | (SOME n1, SOME n2) => Name.equal n1 n2;
in
  fun restrictEqual sym int1 int2 =
      let
        val Interpretation {typeOps = t1, consts = c1} = int1
        and Interpretation {typeOps = t2, consts = c2} = int2

        fun sameTypeOp t = sameName t1 t2 (TypeOp.name t)

        fun sameConst c = sameName c1 c2 (Const.name c)
      in
        TypeOpSet.all sameTypeOp (Symbol.typeOps sym) andalso
        ConstSet.all sameConst (Symbol.consts sym)
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
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun isCommentLine l =
    case List.find (not o Char.isSpace) l of
      NONE => true
    | SOME #"#" => true
    | _ => false;

fun toTextFile {interpretation,filename} =
    Stream.toTextFile {filename = filename} (Print.toStream pp interpretation);

fun fromTextFile {filename} =
    let
      (* Estimating parse error line numbers *)

      val lines = Stream.fromTextFile {filename = filename}

      val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
    in
      (let
         (* The character stream *)

         val chars = Stream.filter (not o isCommentLine) chars

         val chars = Parse.everything Parse.any chars

         (* The interpretation stream *)

         val rws = Parse.everything parserRewrite' chars
       in
         fromRewriteStream rws
       end
       handle Parse.NoParse => raise Error "parse error")
      handle Error err =>
        raise Error ("error in interpretation file \"" ^ filename ^ "\" " ^
                     parseErrorLocation () ^ "\n" ^ err)
    end;

end
