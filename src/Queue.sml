(* ========================================================================= *)
(* A QUEUE DATATYPE FOR ML                                                   *)
(* Copyright (c) 2001 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Queue :> Queue =
struct

datatype 'a queue = Queue of 'a list * 'a list;

fun mkQueue ([], ys as _ :: _) = Queue (List.rev ys, [])
  | mkQueue q = Queue q;

val empty = Queue ([],[]);

fun add z (Queue (xs,ys)) = mkQueue (xs, z :: ys);

fun null (Queue (xs,_)) = List.null xs;

fun hd (Queue (xs,_)) = List.hd xs;

fun tl (Queue (xs,ys)) = mkQueue (List.tl xs, ys);

fun hdTl q = (hd q, tl q);

fun size (Queue (xs,ys)) = List.length xs + List.length ys;

fun fromList l = Queue (List.rev l, []);

fun toList (Queue (xs,ys)) = xs @ List.rev ys;

fun toString q =
    "Queue[" ^ (if null q then "" else Int.toString (size q)) ^ "]";

end
