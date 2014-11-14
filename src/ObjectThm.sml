(* ========================================================================= *)
(* THEOREM OBJECTS                                                           *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectThm :> ObjectThm =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theorem object information.                                     *)
(* ------------------------------------------------------------------------- *)

datatype thm' =
    Thm of
      {proof : Object.object,
       hyp : Object.object,
       concl : Object.object};

fun proof' (Thm {proof = x, ...}) = x;

fun hyp' (Thm {hyp = x, ...}) = x;

fun concl' (Thm {concl = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Converting to a real theorem.                                             *)
(* ------------------------------------------------------------------------- *)

fun thm' inf =
    let
      val Thm {proof,hyp,concl} = inf

      val t = Object.destThm proof
      and seq = Object.destSequent (hyp,concl)
    in
      Rule.alpha seq t
    end;

(* ------------------------------------------------------------------------- *)
(* A type of theorem objects.                                                *)
(* ------------------------------------------------------------------------- *)

datatype thm =
    ThmInf of
      {info : thm',
       thm : Thm.thm};

fun mk inf =
    let
      val th = thm' inf
    in
      ThmInf
        {info = inf,
         thm = th}
    end;

fun dest (ThmInf {info = x, ...}) = x;

fun thm (ThmInf {thm = x, ...}) = x;

fun proof th = proof' (dest th);

fun hyp th = hyp' (dest th);

fun concl th = concl' (dest th);

(* ------------------------------------------------------------------------- *)
(* Mapping over theorems.                                                    *)
(* ------------------------------------------------------------------------- *)

fun maps' f inf acc =
    let
      val Thm {proof,hyp,concl} = inf

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

      val inf' =
          if unchanged then NONE
          else SOME (Thm {proof = proof, hyp = hyp, concl = concl})
    in
      (inf',acc)
    end;

fun maps f th acc =
    let
      val inf = dest th

      val (inf',acc) = maps' f inf acc

      val th' =
          case inf' of
            NONE => NONE
          | SOME inf => SOME (mk inf)
    in
      (th',acc)
    end;

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

val sharingEliminateUnwanted = maps ObjectUnwanted.sharingEliminate;

(* ------------------------------------------------------------------------- *)
(* Adding to a store.                                                        *)
(* ------------------------------------------------------------------------- *)

fun addStore' store inf =
    let
      val Thm {proof,hyp,concl} = inf

      val store = ObjectStore.add store proof

      val store = ObjectStore.add store hyp

      val store = ObjectStore.add store concl
    in
      store
    end;

fun addStore store th = addStore' store (dest th);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap thm Thm.pp;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

fun compare' (inf1,inf2) =
    let
      val Thm {proof = p1, hyp = h1, concl = c1} = inf1
      and Thm {proof = p2, hyp = h2, concl = c2} = inf2
    in
      case Object.compare (p1,p2) of
        LESS => LESS
      | GREATER => GREATER
      | EQUAL =>
        case Object.compare (h1,h2) of
          LESS => LESS
        | GREATER => GREATER
        | EQUAL => Object.compare (c1,c2)
    end;

fun compare (th1,th2) = compare' (dest th1, dest th2);

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

local
  fun mapsElt f (x,(unchanged,set',acc)) =
      let
        val (x',acc) = f x acc

        val (unchanged,x) =
            case x' of
              NONE => (unchanged,x)
            | SOME x => (false,x)

        val set' = add set' x
      in
        (unchanged,set',acc)
      end;
in
  fun maps f set acc =
      let
        val unchanged = true
        and set' = empty

        val (unchanged,set',acc) = foldl (mapsElt f) (unchanged,set',acc) set

        val set' = if unchanged then NONE else SOME set'
      in
        (set',acc)
      end;
end;

val pp = Print.ppBracket "{" "}" (Print.ppMap size Print.ppInt);

end
