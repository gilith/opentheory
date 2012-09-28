(* ========================================================================= *)
(* SEARCHING FOR HIGHER ORDER LOGIC SUBTERMS                                 *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature TermSearch =
sig

(* ------------------------------------------------------------------------- *)
(* Bottom-up term searching.                                                 *)
(* ------------------------------------------------------------------------- *)

type search

val new :
    {predicate : Term.term -> bool,
     leftToRight : bool} -> search

val leftToRight : search -> bool

(* ------------------------------------------------------------------------- *)
(* Searching for subterms.                                                   *)
(* ------------------------------------------------------------------------- *)

(* Terms *)

val sharingSearchTerm : Term.term -> search -> Term.term option * search

val searchTerm : search -> Term.term -> Term.term option

(* Term lists *)

val sharingSearchTermList :
    Term.term list -> search -> Term.term option * search

val searchTermList : search -> Term.term list -> Term.term option

(* Term sets *)

val sharingSearchTermAlphaSet :
    TermAlphaSet.set -> search -> Term.term option * search

val searchTermAlphaSet : search -> TermAlphaSet.set -> Term.term option

end
