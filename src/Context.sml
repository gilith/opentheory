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
      {symbol : Symbol.symbol,
       sequents : SequentSet.set};

val empty =
    Context
      {symbol = Symbol.empty,
       sequents = SequentSet.empty};

fun symbol (Context {symbol = x, ...}) = x;

fun sequents (Context {sequents = x, ...}) = x;

fun addSequent cxt seq =
    let
      val Context {symbol,sequents} = cxt
    in
      if SequentSet.member seq sequents then cxt
      else
        let
          val symbol = Symbol.addSequent symbol seq
          val sequents = SequentSet.add sequents seq
        in
          Context
            {symbol = symbol,
             sequents = sequents}
        end
    end;

fun addSequentSet cxt seqs =
    let
      val Context {symbol,sequents} = cxt
      val symbol = Symbol.addSequentSet symbol seqs
      val sequents = SequentSet.union sequents seqs
    in
      Context
        {symbol = symbol,
         sequents = sequents}
    end;

fun fromSequentSet seqs = addSequentSet empty seqs;

end
