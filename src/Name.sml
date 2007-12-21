(* ========================================================================= *)
(* NAMES                                                                     *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Name :> Name =
struct

type name = string;

val compare = String.compare;

end

structure NameOrdered =
struct type t = Name.name val compare = Name.compare end

structure NameSet =
ElementSet (NameOrdered)

structure NameMap =
KeyMap (NameOrdered)
