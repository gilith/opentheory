(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY GRAPHS                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Graph =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory graphs.                                                  *)
(* ------------------------------------------------------------------------- *)

type graph

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty : graph

val instances : graph -> InstanceSet.set

val member : Instance.instance -> graph -> bool

(* ------------------------------------------------------------------------- *)
(* Adding instances.                                                         *)
(* ------------------------------------------------------------------------- *)

val add : graph -> Instance.instance -> graph

(* ------------------------------------------------------------------------- *)
(* Looking up theory instances by package name.                              *)
(* ------------------------------------------------------------------------- *)

val lookup : graph -> PackageName.name -> InstanceSet.set

(* ------------------------------------------------------------------------- *)
(* Finding matching theory instances.                                        *)
(* ------------------------------------------------------------------------- *)

val match :
    graph ->
    {requires : InstanceSet.set,
     interpretation : Interpretation.interpretation,
     package : PackageName.name} ->
    InstanceSet.set

(***
(* ------------------------------------------------------------------------- *)
(* Installing theory packages.                                               *)
(* ------------------------------------------------------------------------- *)

val install :
    graph ->
    {
***)

end
