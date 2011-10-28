(* ========================================================================= *)
(* THEOREM OBJECTS                                                           *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectThm :> ObjectThm =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theorem objects.                                                *)
(* ------------------------------------------------------------------------- *)

datatype thm =
    Thm of
      {proof : ObjectProv.object,
       hyp : ObjectProv.object,
       concl : ObjectProv.object};

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare (th1,th2) =
    let
      val Thm {proof = p1, hyp = h1, concl = c1} = th1
      and Thm {proof = p2, hyp = h2, concl = c2} = th2
    in
      case ObjectProv.compare (p1,p2) of
        LESS => LESS
      | GREATER => GREATER
      | EQUAL =>
        case ObjectProv.compare (h1,h2) of
          LESS => LESS
        | GREATER => GREATER
        | EQUAL => ObjectProv.compare (c1,c2)
    end;

(* ------------------------------------------------------------------------- *)
(* Converting to a real theorem.                                             *)
(* ------------------------------------------------------------------------- *)

fun thm th =
    let
      val Thm {proof,hyp,concl} = th

      val t = ObjectProv.destThm proof
      and seq = ObjectProv.destSequent (hyp,concl)
    in
      Rule.alpha seq t
    end;

(* ------------------------------------------------------------------------- *)
(* Mapping over theorems.                                                    *)
(* ------------------------------------------------------------------------- *)

fun maps f th acc =
    let
      val Thm {proof,hyp,concl} = th

      val (proof',acc) = f proof acc

      val (hyp',acc) = f hyp acc

      val (concl',acc) = f concl acc

      val unchanged = true

      val (unchanged,proof) =
          case proof' of
            NONE => (unchanged,proof)
          | SOME obj => (false,obj)

      val (unchanged,hyp) =
          case hyp' of
            NONE => (unchanged,hyp)
          | SOME obj => (false,obj)

      val (unchanged,concl) =
          case concl' of
            NONE => (unchanged,concl)
          | SOME obj => (false,obj)

      val th' =
          if unchanged then NONE
          else SOME (Thm {proof = proof, hyp = hyp, concl = concl})
    in
      (th',acc)
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap thm Thm.pp;

end

structure ObjectThmOrdered =
struct type t = ObjectThm.thm val compare = ObjectThm.compare end

structure ObjectThmMap = KeyMap (ObjectThmOrdered)

structure ObjectThmSet =
struct

local
  structure S = ElementSet (ObjectThmMap);
in
  open S;
end;

val pp = Print.ppBracket "{" "}" (Print.ppMap size Print.ppInt);

end
