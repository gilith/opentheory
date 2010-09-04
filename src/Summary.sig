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

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype summary' =
    Summary' of
      {requires : Sequents.sequents,
       provides : Sequents.sequents};

val mk : summary' -> summary

val dest : summary -> summary'

val requires : summary -> Sequents.sequents

val provides : summary -> Sequents.sequents

val fromThms : Thms.thms -> summary

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val ppWithShow : Show.show -> summary Print.pp

val pp : summary Print.pp

val toHtml : Show.show -> summary -> Html.block list

val toTextFile :
    {show : Show.show,
     summary : summary,
     filename : string} -> unit

end
