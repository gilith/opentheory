(* ========================================================================= *)
(* OPENTHEORY NAMES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Name :> Name =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of names.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype name = Name of Namespace.namespace * string;

val mk = Name;

fun dest (Name n) = n;

fun namespace n = fst (dest n);

(* ------------------------------------------------------------------------- *)
(* The top level namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

fun mkGlobal s = Name (Namespace.global,s);

fun destGlobal (Name (n,s)) =
    if Namespace.isGlobal n then s
    else raise Error "Name.destGlobal";

val isGlobal = can destGlobal;

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

fun compare (Name n1, Name n2) =
    prodCompare Namespace.compare Namespace.compareComponent (n1,n2);

fun equal (Name (ns1,n1)) (Name (ns2,n2)) =
    n1 = n2 andalso Namespace.equal ns1 ns2;

val equalList = listEqual equal;

(* ------------------------------------------------------------------------- *)
(* Fresh names.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  val new = Namespace.mkNested (Namespace.global,"new");

  fun numName i = Name (new, Int.toString i);
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
              val Name (ns,s) = n

              val s = s ^ "'"

              val n = Name (ns,s)
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
          val Name (ns,s) = n

          val s = stripSuffix isDigitOrPrime s

          fun variant i =
              let
                val s_i = s ^ Int.toString i

                val n = Name (ns,s_i)
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

fun rewrite x_y (name as Name (ns,n)) =
    case Namespace.rewrite x_y ns of
      NONE => NONE
    | SOME ns => SOME (Name (ns,n));

(* ------------------------------------------------------------------------- *)
(* Characters.                                                               *)
(* ------------------------------------------------------------------------- *)

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

(* Lists *)

local
  fun mkList c = mk (Namespace.list,c);
in
  val listTypeOp = mkList Namespace.listTypeOpComponent;
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

(* Natural numbers *)

local
  fun mkNatural c = mk (Namespace.natural,c);
in
  val naturalTypeOp = mkNatural Namespace.naturalTypeOpComponent;
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

(* Functions *)

local
  fun mkFunction c = mk (Namespace.function,c);
in
  val composeConst = mkFunction Namespace.composeConstComponent;
end;

(* Lists *)

local
  fun mkList c = mk (Namespace.list,c);
in
  val consConst = mkList Namespace.consConstComponent
  and nilConst = mkList Namespace.nilConstComponent;
end;

(* Options *)

local
  fun mkOption c = mk (Namespace.option,c);
in
  val noneConst = mkOption Namespace.noneConstComponent
  and someConst = mkOption Namespace.someConstComponent;
end;

(* Natural numbers *)

local
  fun mkNatural c = mk (Namespace.natural,c);
in
  val bit0Const = mkNatural Namespace.bit0ConstComponent
  and bit1Const = mkNatural Namespace.bit1ConstComponent
  and minimalConst = mkNatural Namespace.minimalConstComponent
  and sucConst = mkNatural Namespace.sucConstComponent
  and zeroConst = mkNatural Namespace.zeroConstComponent;
end;

fun isFromNaturalConst (Name (_,s)) =
    s = Namespace.fromNaturalConstComponent;

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

  fun fromNamespace ns =
      if Namespace.isGlobal ns then raise NoParse
      else Name (Namespace.destNested ns);
in
  val quotedParser = Namespace.quotedParser >> fromNamespace;
end;

local
  fun toNamespace (Name ns_n) = Namespace.mkNested ns_n;
in
  val pp = Print.ppMap toNamespace Namespace.pp;

  val ppQuoted = Print.ppMap toNamespace Namespace.ppQuoted;

  fun toHtml n = Namespace.toHtml (toNamespace n);
end;

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
