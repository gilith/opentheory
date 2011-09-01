(* ========================================================================= *)
(* PROFILING INFERENCE COMMANDS                                              *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Inference :> Inference =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of inference command profiles.                                     *)
(* ------------------------------------------------------------------------- *)

datatype inference = Inference of int CommandMap.map;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty = Inference (CommandMap.new ());

fun null (Inference m) = CommandMap.null m;

(* ------------------------------------------------------------------------- *)
(* Adding inferences.                                                        *)
(* ------------------------------------------------------------------------- *)

fun add inf cmd =
    if not (Command.isInference cmd) then inf
    else
      let
        val Inference m = inf

        val i = Option.getOpt (CommandMap.peek m cmd, 0)

        val m = CommandMap.insert m (cmd, i + 1)
      in
        Inference m
      end;

(* ------------------------------------------------------------------------- *)
(* Merging inference profiles.                                               *)
(* ------------------------------------------------------------------------- *)

local
  fun add ((_,i1),(_,i2)) = SOME (i1 + i2);
in
  fun union (Inference m1) (Inference m2) =
      let
        val m = CommandMap.union add m1 m2
      in
        Inference m
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  val alignment : columnAlignment list =
      [{leftAlign = true, padChar = #"."},
       {leftAlign = false, padChar = #"."}];

  val countToString = Print.toString Print.ppPrettyInt;

  fun mkRow (s,i) = [s ^ " ...", " " ^ countToString i];

  fun mkInfRow (n,i) = mkRow (Command.toString n, i);

  fun mkTotalRow i = mkRow ("Total",i);
in
  fun pp inf =
      if null inf then Print.ppString "No primitive inferences"
      else
        let
          val Inference m = inf

          val infs = sortMap snd (revCompare Int.compare) (CommandMap.toList m)

          val tot = List.foldl (fn ((_,i),k) => i + k) 0 infs

          val table = List.map mkInfRow infs @ [mkTotalRow tot]

          val rows = alignTable alignment table
        in
          Print.consistentBlock 0
            (Print.ppString "Primitive inferences:" ::
             List.map (Print.sequence Print.newline o Print.ppString) rows)
        end;
end;

end
