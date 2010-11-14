(* ========================================================================= *)
(* SEQUENTS AND THEIR SYMBOLS                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Sequents =
sig

(* ------------------------------------------------------------------------- *)
(* A type of sequents and their symbols.                                     *)
(* ------------------------------------------------------------------------- *)

type sequents

val empty : sequents

val size : sequents -> int

val sequents : sequents -> SequentSet.set

val symbol : sequents -> Symbol.symbol

(* ------------------------------------------------------------------------- *)
(* Adding sequents.                                                          *)
(* ------------------------------------------------------------------------- *)

val add : sequents -> Sequent.sequent -> sequents

val addList : sequents -> Sequent.sequent list -> sequents

val addSet : sequents -> SequentSet.set -> sequents

val addThms : sequents -> Thms.thms -> sequents

val singleton : Sequent.sequent -> sequents

val fromList : Sequent.sequent list -> sequents

val fromSet : SequentSet.set -> sequents

val fromThms : Thms.thms -> sequents

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

val union : sequents -> sequents -> sequents

val unionList : sequents list -> sequents

(* ------------------------------------------------------------------------- *)
(* Substitutions.                                                            *)
(* ------------------------------------------------------------------------- *)

val sharingSubst :
    sequents -> TermSubst.subst -> sequents option * TermSubst.subst

val subst : TermSubst.subst -> sequents -> sequents option

(* ------------------------------------------------------------------------- *)
(* Rewrites.                                                                 *)
(* ------------------------------------------------------------------------- *)

val sharingRewrite :
    sequents -> TermRewrite.rewrite -> sequents option * TermRewrite.rewrite

val rewrite : TermRewrite.rewrite -> sequents -> sequents option

end
