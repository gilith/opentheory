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
(* Rename symbol names.                                                      *)
(* ------------------------------------------------------------------------- *)

val sharingRename = maps ObjectRename.sharingRename;

(* ------------------------------------------------------------------------- *)
(* Replace definitions with theory assumptions.                              *)
(* ------------------------------------------------------------------------- *)

val sharingSkipDefinitions = maps ObjectSkipDefinitions.sharingSkipDefinitions;

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

end
