(* ========================================================================= *)
(* OPENTHEORY NAMES                                                          *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Name :> Name =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of names.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype name =
    Name of
      {namespace : Namespace.namespace,
       component : Namespace.component,
       toNamespace : Namespace.namespace};

fun mk (ns,c) =
    Name
      {namespace = ns,
       component = c,
       toNamespace = Namespace.mkNested (ns,c)};

fun dest (Name {namespace = ns, component = c, ...}) = (ns,c);

fun namespace (Name {namespace = x, ...}) = x;

fun component (Name {component = x, ...}) = x;

fun toNamespace (Name {toNamespace = x, ...}) = x;

fun fromNamespace tns =
    let
      val (ns,c) = Namespace.destNested tns
    in
      Name
        {namespace = ns,
         component = c,
         toNamespace = tns}
    end;

(* ------------------------------------------------------------------------- *)
(* The top level namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

fun mkGlobal c = mk (Namespace.global,c);

fun destGlobal n =
    let
      val () =
          if Namespace.isGlobal (namespace n) then ()
          else raise Error "Name.destGlobal"
    in
      component n
    end;

val isGlobal = can destGlobal;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

fun compare (n1,n2) =
    let
      val Name {toNamespace = t1, ...} = n1
      and Name {toNamespace = t2, ...} = n2
    in
      Namespace.compare (t1,t2)
    end;

fun equal n1 n2 =
    let
      val Name {namespace = ns1, component = c1, ...} = n1
      and Name {namespace = ns2, component = c2, ...} = n2
    in
      c1 = c2 andalso Namespace.equal ns1 ns2
    end;

val equalList = listEqual equal;

(* ------------------------------------------------------------------------- *)
(* Fresh names.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  val new = Namespace.mkNested (Namespace.global,"new");

  fun numName i = mk (new, Int.toString i);
in
  fun newName () = numName (newInt ());

  fun newNames n = List.map numName (newInts n);
end;

fun variantPrime {avoid} =
    let
      fun variant n =
          if not (avoid n) then n
          else
            let
              val (ns,s) = dest n

              val s = s ^ "'"

              val n = mk (ns,s)
            in
              variant n
            end
    in
      variant
    end;

local
  fun isDigitOrPrime c = c = #"'" orelse Char.isDigit c;
in
  fun variantNum {avoid} n =
      if not (avoid n) then n
      else
        let
          val (ns,s) = dest n

          val s = stripSuffix isDigitOrPrime s

          fun variant i =
              let
                val s_i = s ^ Int.toString i

                val n = mk (ns,s_i)
              in
                if avoid n then variant (i + 1) else n
              end
        in
          variant 0
        end;
end;

(* ------------------------------------------------------------------------- *)
(* Rewriting names.                                                          *)
(* ------------------------------------------------------------------------- *)

fun rewrite x_y n =
    case Namespace.rewrite x_y (namespace n) of
      NONE => NONE
    | SOME ns => SOME (mk (ns, component n));

(* ------------------------------------------------------------------------- *)
(* Characters.                                                               *)
(* ------------------------------------------------------------------------- *)

fun firstChar n =
    let
      val (ns,s) = dest n

      val () =
          if Namespace.isGlobal ns then ()
          else raise Bug "Name.firstChar"

      val i = String.size s
    in
      if i = 0 then NONE else SOME (String.sub (s,0))
    end;

fun lastChar n =
    let
      val (_,s) = dest n

      val i = String.size s
    in
      if i = 0 then NONE else SOME (String.sub (s, i - 1))
    end;

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Primitive *)

val boolTypeOp = mkGlobal Namespace.boolTypeOpComponent
and funTypeOp = mkGlobal Namespace.funTypeOpComponent
and indTypeOp = mkGlobal Namespace.indTypeOpComponent;

(* Bytes *)

local
  fun mkByte c = mk (Namespace.byte,c);
in
  val byteTypeOp = mkByte Namespace.byteTypeOpComponent;
end;

(* Lists *)

local
  fun mkList c = mk (Namespace.list,c);
in
  val listTypeOp = mkList Namespace.listTypeOpComponent;
end;

(* Natural numbers *)

local
  fun mkNatural c = mk (Namespace.natural,c);
in
  val naturalTypeOp = mkNatural Namespace.naturalTypeOpComponent;
end;

(* Options *)

local
  fun mkOption c = mk (Namespace.option,c);
in
  val optionTypeOp = mkOption Namespace.optionTypeOpComponent;
end;

(* Pairs *)

local
  fun mkPair c = mk (Namespace.pair,c);
in
  val pairTypeOp = mkPair Namespace.pairTypeOpComponent;
end;

(* Random streams *)

local
  fun mkRandom c = mk (Namespace.random,c);
in
  val randomTypeOp = mkRandom Namespace.randomTypeOpComponent;
end;

(* Streams *)

local
  fun mkStream c = mk (Namespace.stream,c);
in
  val streamTypeOp = mkStream Namespace.streamTypeOpComponent;
end;

(* 16-bit words *)

local
  fun mkWord16 c = mk (Namespace.word16,c);
in
  val word16TypeOp = mkWord16 Namespace.word16TypeOpComponent;
end;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Primitive *)

val eqConst = mkGlobal Namespace.eqConstComponent
and selectConst = mkGlobal Namespace.selectConstComponent;

(* Boolean *)

local
  fun mkBool c = mk (Namespace.bool,c);
in
  val condConst = mkBool Namespace.condConstComponent
  and conjConst = mkBool Namespace.conjConstComponent
  and disjConst = mkBool Namespace.disjConstComponent
  and existsConst = mkBool Namespace.existsConstComponent
  and existsUniqueConst = mkBool Namespace.existsUniqueConstComponent
  and falseConst = mkBool Namespace.falseConstComponent
  and forallConst = mkBool Namespace.forallConstComponent
  and impConst = mkBool Namespace.impConstComponent
  and negConst = mkBool Namespace.negConstComponent
  and trueConst = mkBool Namespace.trueConstComponent;
end;

(* Bytes *)

local
  fun mkByte c = mk (Namespace.byte,c);
in
  val addByteConst = mkByte Namespace.addConstComponent
  and andByteConst = mkByte Namespace.andConstComponent
  and bitByteConst = mkByte Namespace.bitConstComponent
  and fromNaturalByteConst = mkByte Namespace.fromNaturalConstComponent
  and leByteConst = mkByte Namespace.leConstComponent
  and ltByteConst = mkByte Namespace.ltConstComponent
  and multiplyByteConst = mkByte Namespace.multiplyConstComponent
  and notByteConst = mkByte Namespace.notConstComponent
  and orByteConst = mkByte Namespace.orConstComponent
  and shiftLeftByteConst = mkByte Namespace.shiftLeftConstComponent
  and shiftRightByteConst = mkByte Namespace.shiftRightConstComponent
  and subtractByteConst = mkByte Namespace.subtractConstComponent;
end;

(* Functions *)

local
  fun mkFunction c = mk (Namespace.function,c);
in
  val composeConst = mkFunction Namespace.composeConstComponent
  and idConst = mkFunction Namespace.idConstComponent
  and surjectiveConst = mkFunction Namespace.surjectiveConstComponent;
end;

(* Lists *)

local
  fun mkList c = mk (Namespace.list,c);
in
  val allConst = mkList Namespace.allConstComponent
  and anyConst = mkList Namespace.anyConstComponent
  and appendConst = mkList Namespace.appendConstComponent
  and concatConst = mkList Namespace.concatConstComponent
  and consConst = mkList Namespace.consConstComponent
  and headConst = mkList Namespace.headConstComponent
  and lengthConst = mkList Namespace.lengthConstComponent
  and mapConst = mkList Namespace.mapConstComponent
  and nilConst = mkList Namespace.nilConstComponent
  and tailConst = mkList Namespace.tailConstComponent;
end;

(* Natural numbers *)

local
  fun mkNatural c = mk (Namespace.natural,c);
in
  val addConst = mkNatural Namespace.addConstComponent
  and bit0Const = mkNatural Namespace.bit0ConstComponent
  and bit1Const = mkNatural Namespace.bit1ConstComponent
  and divConst = mkNatural Namespace.divConstComponent
  and leConst = mkNatural Namespace.leConstComponent
  and ltConst = mkNatural Namespace.ltConstComponent
  and minimalConst = mkNatural Namespace.minimalConstComponent
  and modConst = mkNatural Namespace.modConstComponent
  and multiplyConst = mkNatural Namespace.multiplyConstComponent
  and subtractConst = mkNatural Namespace.subtractConstComponent
  and sucConst = mkNatural Namespace.sucConstComponent
  and zeroConst = mkNatural Namespace.zeroConstComponent;
end;

fun isFromNaturalConst n =
    component n = Namespace.fromNaturalConstComponent;

(* Options *)

local
  fun mkOption c = mk (Namespace.option,c);
in
  val noneConst = mkOption Namespace.noneConstComponent
  and someConst = mkOption Namespace.someConstComponent;
end;

(* Pairs *)

local
  fun mkPair c = mk (Namespace.pair,c);
in
  val fstConst = mkPair Namespace.fstConstComponent
  and pairConst = mkPair Namespace.pairConstComponent
  and sndConst = mkPair Namespace.sndConstComponent;
end;

(* Random streams *)

local
  fun mkRandom c = mk (Namespace.random,c);
in
  val bitConst = mkRandom Namespace.bitConstComponent
  and splitConst = mkRandom Namespace.splitConstComponent;
end;

(* Sets *)

local
  fun mkSet c = mk (Namespace.set,c);
in
  val differenceConst = mkSet Namespace.differenceConstComponent
  and emptyConst = mkSet Namespace.emptyConstComponent
  and fromPredicateConst = mkSet Namespace.fromPredicateConstComponent
  and intersectConst = mkSet Namespace.intersectConstComponent
  and memberConst = mkSet Namespace.memberConstComponent
  and properSubsetConst = mkSet Namespace.properSubsetConstComponent
  and subsetConst = mkSet Namespace.subsetConstComponent
  and unionConst = mkSet Namespace.unionConstComponent;
end;

(* Streams *)

local
  fun mkStream c = mk (Namespace.stream,c);
in
  val appendStreamConst = mkStream Namespace.appendConstComponent
  and concatStreamConst = mkStream Namespace.concatConstComponent
  and consStreamConst = mkStream Namespace.consConstComponent
  and headStreamConst = mkStream Namespace.headConstComponent
  and lengthStreamConst = mkStream Namespace.lengthConstComponent
  and mapStreamConst = mkStream Namespace.mapConstComponent
  and nilStreamConst = mkStream Namespace.nilConstComponent
  and tailStreamConst = mkStream Namespace.tailConstComponent;
end;

(* 16-bit words *)

local
  fun mkWord16 c = mk (Namespace.word16,c);
in
  val addWord16Const = mkWord16 Namespace.addConstComponent
  and andWord16Const = mkWord16 Namespace.andConstComponent
  and bitWord16Const = mkWord16 Namespace.bitConstComponent
  and fromBytesWord16Const = mkWord16 Namespace.fromBytesConstComponent
  and fromNaturalWord16Const = mkWord16 Namespace.fromNaturalConstComponent
  and leWord16Const = mkWord16 Namespace.leConstComponent
  and ltWord16Const = mkWord16 Namespace.ltConstComponent
  and multiplyWord16Const = mkWord16 Namespace.multiplyConstComponent
  and notWord16Const = mkWord16 Namespace.notConstComponent
  and orWord16Const = mkWord16 Namespace.orConstComponent
  and shiftLeftWord16Const = mkWord16 Namespace.shiftLeftConstComponent
  and shiftRightWord16Const = mkWord16 Namespace.shiftRightConstComponent
  and subtractWord16Const = mkWord16 Namespace.subtractConstComponent
  and toBytesWord16Const = mkWord16 Namespace.toBytesConstComponent;
end;

(* Case expressions *)

local
  fun strip l =
      case l of
        [] => raise Error "Name.destCase"
      | s :: l =>
        if s = Namespace.caseConstComponent then ([],l)
        else
          let
            val (sl,l) = strip l
          in
            (s :: sl, l)
          end;

  fun add ns (s,acc) = mk (ns,s) :: acc;
in
  fun destCase n =
      let
        val (ns,c) = dest n

        val (ns,cs) = strip (Namespace.toList ns)

        val ns = Namespace.fromList ns

        val n = mk (ns,Namespace.caseConstComponent)

        val bs = List.foldl (add ns) [] (c :: List.rev cs)
      in
        (n,bs)
      end;
end;

val isCase = can destCase;

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  fun process ns =
      case total fromNamespace ns of
        SOME n => n
      | NONE => raise NoParse;
in
  val quotedParser = Namespace.quotedParser >> process;
end;

val quotedFromString = Parse.fromString quotedParser;

val pp = Print.ppMap toNamespace Namespace.pp;

val ppQuoted = Print.ppMap toNamespace Namespace.ppQuoted;

fun toHtml n = Namespace.toHtml (toNamespace n);

val ppList = Print.ppList pp;

val toString = Print.toString pp;

val toStringList = Print.toString ppList;

val quotedToString = Print.toString ppQuoted;

val fromString = mkGlobal;

end

structure NameOrdered =
struct type t = Name.name val compare = Name.compare end

structure NameMap = KeyMap (NameOrdered)

structure NameSet =
struct

local
  structure S = ElementSet (NameMap);
in
  open S;
end;

local
  fun addName (n,s) = NamespaceSet.add s (Name.namespace n);
in
  fun namespace s = foldl addName NamespaceSet.empty s;
end;

val pp =
    Print.ppMap toList
      (Print.ppBracket "{" "}" (Print.ppOpList "," Name.pp));

end
