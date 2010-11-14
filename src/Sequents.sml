(* ========================================================================= *)
(* SEQUENTS AND THEIR SYMBOLS                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Sequents :> Sequents =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of sequents and their symbols.                                     *)
(* ------------------------------------------------------------------------- *)

datatype sequents =
    Sequents of
      {sequents : SequentSet.set,
       symbol : Symbol.symbol};

val empty =
    let
      val seqs = SequentSet.empty
      and sym = Symbol.empty
    in
      Sequents
        {sequents = seqs,
         symbol = sym}
    end;

fun size (Sequents {sequents = seqs, ...}) = SequentSet.size seqs;

fun sequents (Sequents {sequents = x, ...}) = x;

fun symbol (Sequents {symbol = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Adding sequents.                                                          *)
(* ------------------------------------------------------------------------- *)

fun add sequents seq =
    let
      val Sequents {sequents = seqs, symbol = sym} = sequents

      val seqs = SequentSet.add seqs seq

      val sym = Symbol.addSequent sym seq
    in
      Sequents
        {sequents = seqs,
         symbol = sym}
    end;

local
  fun add1 (seq,seqs) = add seqs seq;
in
  fun addList seqs seql = List.foldl add1 seqs seql;

  fun addSet seqs set = SequentSet.foldl add1 seqs set;
end;

fun addThms sequents thms =
    let
      val Sequents {sequents = seqs, symbol = sym} = sequents

      val seqs = SequentSet.union seqs (ThmSet.sequents (Thms.thms thms))

      val sym = Symbol.union sym (Thms.symbol thms)
    in
      Sequents
        {sequents = seqs,
         symbol = sym}
    end;

val singleton = add empty;

val fromList = addList empty;

val fromSet = addSet empty;

val fromThms = addThms empty;

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

fun union sequents1 sequents2 =
    let
      val Sequents {sequents = seqs1, symbol = sym1} = sequents1
      and Sequents {sequents = seqs2, symbol = sym2} = sequents2

      val seqs = SequentSet.union seqs1 seqs2

      val sym = Symbol.union sym1 sym2
    in
      Sequents
        {sequents = seqs,
         symbol = sym}
    end;

local
  fun uncurriedUnion (sequents1,sequents2) = union sequents1 sequents2;
in
  fun unionList sequentsl =
      case sequentsl of
        [] => empty
      | sequents :: sequentsl => List.foldl uncurriedUnion sequents sequentsl;
end;

(* ------------------------------------------------------------------------- *)
(* Substitutions.                                                            *)
(* ------------------------------------------------------------------------- *)

fun sharingSubst seqs sub =
    let
      val sqs = sequents seqs

      val (sqs',sub) = SequentSet.sharingSubst sqs sub

      val seqs' =
          case sqs' of
            SOME sqs => SOME (fromSet sqs)
          | NONE => NONE
    in
      (seqs',sub)
    end;

fun subst sub seqs =
    let
      val (seqs',_) = sharingSubst seqs sub
    in
      seqs'
    end;

(* ------------------------------------------------------------------------- *)
(* Rewrites.                                                                 *)
(* ------------------------------------------------------------------------- *)

fun sharingRewrite seqs rewr =
    let
      val sqs = sequents seqs

      val (sqs',rewr) = SequentSet.sharingRewrite sqs rewr

      val seqs' =
          case sqs' of
            SOME sqs => SOME (fromSet sqs)
          | NONE => NONE
    in
      (seqs',rewr)
    end;

fun rewrite rewr seqs =
    let
      val (seqs',_) = sharingRewrite seqs rewr
    in
      seqs'
    end;

end
