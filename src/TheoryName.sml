(* ========================================================================= *)
(* NAMED THEORIES                                                            *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure TheoryName :> TheoryName =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of named theories.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype nameTheory = TheoryName of PackageTheory.name * Theory.theory;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk = TheoryName;

fun name (TheoryName (n,_)) = n;

fun theory (TheoryName (_,thy)) = thy;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (nt1,nt2) =
    let
      val TheoryName (n1,t1) = nt1
      and TheoryName (n2,t2) = nt2
    in
      case PackageName.compare (n1,n2) of
        LESS => LESS
      | EQUAL => Theory.compare (t1,t2)
      | GREATER => GREATER
    end;

fun equal nt1 nt2 = compare (nt1,nt2) = EQUAL;

(* ------------------------------------------------------------------------- *)
(* Union theories.                                                           *)
(* ------------------------------------------------------------------------- *)

fun isUnion nt = Theory.isUnion (theory nt);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp (TheoryName (name,thy)) =
    Print.consistentBlock 0
      [PackageName.pp name,
       Theory.ppId (Theory.id thy)];

end

structure TheoryNameOrdered =
struct type t = TheoryName.nameTheory val compare = TheoryName.compare end

structure TheoryNameMap =
struct

  local
    structure S = KeyMap (TheoryNameOrdered);
  in
    open S;
  end;

  fun pp ppX =
      let
        val ppNTX = Print.ppOp2 " =>" TheoryName.pp ppX
      in
        fn m =>
          Print.consistentBlock 2
            [Print.ppString "TheoryNameMap",
             Print.break,
             Print.ppList ppNTX (toList m)]
      end;

end

structure TheoryNameSet =
struct

  local
    structure S = ElementSet (TheoryNameMap);
  in
    open S;
  end;

end
