(* ========================================================================= *)
(* THEORY SUMMARIES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Summary =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory summary.                                                 *)
(* ------------------------------------------------------------------------- *)

type summary

val requires : summary -> Context.context

val provides : summary -> Context.context

val fromThmSet : ThmSet.set -> summary

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val ppWithShow : Show.show -> summary Print.pp

val pp : summary Print.pp

val toTextFile :
    {show : Show.show,
     summary : summary,
     filename : string} -> unit

end
