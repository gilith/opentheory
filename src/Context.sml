(* ========================================================================= *)
(* THEORY CONTEXTS                                                           *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Context :> Context =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory contexts.                                                *)
(* ------------------------------------------------------------------------- *)

datatype context =
    Context of
      {symbols : Symbol.symbol,
       sequents : SequentSet.set};

val empty =
    Context
      {symbols = Symbol.empty,
       sequents = SequentSet.empty};

fun symbols (Context {symbols = s, ...}) = s;

fun sequents (Context {sequents = s, ...}) = s;

fun addSequent cxt seq =
    let
      val Context {symbols,sequents} = cxt
    in
      if SequentSet.member seq sequents then cxt
      else
        let
          val symbols = Symbol.addSequent symbols seq
          val sequents = SequentSet.add sequents seq
        in
          Context
            {symbols = symbols,
             sequents = sequents}
        end
    end;

fun addSequentSet cxt seqs =
    let
      val Context {symbols,sequents} = cxt
      val symbols = Symbol.addSequentSet symbols seqs
      val sequents = SequentSet.union sequents seqs
    in
      Context
        {symbols = symbols,
         sequents = sequents}
    end;

fun fromSequentSet seqs = addSequentSet empty seqs;

end
