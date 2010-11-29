(* ========================================================================= *)
(* THEORY PACKAGE SUMMARIES                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageSummary =
sig

(* ------------------------------------------------------------------------- *)
(* A type tracking the source package of sequents.                           *)
(* ------------------------------------------------------------------------- *)

type sequentSource = PackageName.name SequentMap.map

(* ------------------------------------------------------------------------- *)
(* A type of theory package summaries.                                       *)
(* ------------------------------------------------------------------------- *)

type summary

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype summary' =
    Summary' of
      {summary : Summary.summary,
       requires : sequentSource,
       provides : sequentSource}

val mk : summary' -> summary

val dest : summary -> summary'

val summary : summary -> Summary.summary

(* ------------------------------------------------------------------------- *)
(* Check summary.                                                            *)
(* ------------------------------------------------------------------------- *)

val check : summary -> unit

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val toHtml : Show.show -> summary -> Html.block list

end
