(* ========================================================================= *)
(* OBJECT DICTIONARIES                                                       *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure ObjectDict :> ObjectDict =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of object dictionaries.                                            *)
(* ------------------------------------------------------------------------- *)

datatype dict = Dict of ObjectProv.object IntMap.map;

val empty = Dict (IntMap.new ());

fun size (Dict m) = IntMap.size m;

fun define (Dict dict) (key,obj) = Dict (IntMap.insert dict (key,obj));

fun refer (Dict dict) key =
    case IntMap.peek dict key of
      SOME obj => obj
    | NONE =>
      raise Error ("ObjectDict.refer: no entry for key " ^ Int.toString key);

fun remove (Dict dict) key =
    case IntMap.peek dict key of
      SOME obj => (Dict (IntMap.delete dict key), obj)
    | NONE =>
      raise Error ("ObjectDict.remove: no entry for key " ^ Int.toString key);

end
