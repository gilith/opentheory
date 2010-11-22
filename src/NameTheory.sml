(* ========================================================================= *)
(* NAMED THEORIES                                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure NameTheory :> NameTheory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of named theories.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype nameTheory = NameTheory of PackageTheory.name * Theory.theory;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk = NameTheory;

fun name (NameTheory (n,_)) = n;

fun theory (NameTheory (_,thy)) = thy;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (nt1,nt2) =
    let
      val NameTheory (n1,t1) = nt1
      and NameTheory (n2,t2) = nt2
    in
      case PackageBase.compare (n1,n2) of
        LESS => LESS
      | EQUAL => Theory.compare (t1,t2)
      | GREATER => GREATER
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp (NameTheory (name,thy)) =
    Print.blockProgram Print.Consistent 0
      [PackageTheory.ppName name,
       Theory.ppId (Theory.id thy)];

end

structure NameTheoryOrdered =
struct type t = NameTheory.nameTheory val compare = NameTheory.compare end

structure NameTheoryMap =
struct

  local
    structure S = KeyMap (NameTheoryOrdered);
  in
    open S;
  end;

  fun pp ppX =
      let
        val ppNTX = Print.ppOp2 " =>" NameTheory.pp ppX
      in
        fn m =>
          Print.blockProgram Print.Consistent 2
            [Print.ppString "NameTheoryMap",
             Print.addBreak 1,
             Print.ppList ppNTX (toList m)]
      end;

end

structure NameTheorySet =
struct

  local
    structure S = ElementSet (NameTheoryMap);
  in
    open S;
  end;

end
