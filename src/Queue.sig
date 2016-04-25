(* ========================================================================= *)
(* A QUEUE DATATYPE FOR ML                                                   *)
(* Copyright (c) 2001 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Queue =
sig

type 'a queue

val empty : 'a queue

val null : 'a queue -> bool

val add : 'a -> 'a queue -> 'a queue

val hd : 'a queue -> 'a  (* raises Empty *)

val tl : 'a queue -> 'a queue  (* raises Empty *)

val hdTl : 'a queue -> 'a * 'a queue  (* raises Empty *)

val size : 'a queue -> int

val fromList : 'a list -> 'a queue

val toList : 'a queue -> 'a list

val toString : 'a queue -> string

end
