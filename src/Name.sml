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

val boolTypeOp = mkGlobal "bool"
and funTypeOp = mkGlobal "->"
and indTypeOp = mkGlobal "ind";

(* Lists *)

val listTypeOp = mk (Namespace.list,"list");

(* Options *)

val optionTypeOp = mk (Namespace.option,"option");

(* Pairs *)

val pairTypeOp = mk (Namespace.pair,"*");

(* Natural numbers *)

val naturalTypeOp = mk (Namespace.natural,"natural");

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Primitive *)

val eqConst = mkGlobal "="
and selectConst = mkGlobal "select";

(* Boolean *)

val condConst = mk (Namespace.bool,"cond")
and conjConst = mk (Namespace.bool,"/\\")
and disjConst = mk (Namespace.bool,"\\/")
and existsConst = mk (Namespace.bool,"?")
and existsUniqueConst = mk (Namespace.bool,"?!")
and falseConst = mk (Namespace.bool,"F")
and forallConst = mk (Namespace.bool,"!")
and impConst = mk (Namespace.bool,"==>")
and negConst = mk (Namespace.bool,"~")
and trueConst = mk (Namespace.bool,"T");

(* Lists *)

val consConst = mk (Namespace.list,"::")
and nilConst = mk (Namespace.list,"[]");

(* Options *)

val noneConst = mk (Namespace.option,"none")
and someConst = mk (Namespace.option,"some");

(* Natural numbers *)

val bit0Const = mk (Namespace.natural,"bit0")
and bit1Const = mk (Namespace.natural,"bit1")
and sucConst = mk (Namespace.natural,"suc")
and zeroConst = mk (Namespace.natural,"zero");

fun isFromNaturalConst (Name (_,s)) = s = "fromNatural";

(* Sets *)

val fromPredicateConst = mk (Namespace.set,"fromPredicate");

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

val toString = Print.toString pp;

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

val pp =
    Print.ppMap toList
      (Print.ppBracket "{" "}" (Print.ppOpList "," Name.pp));

end
