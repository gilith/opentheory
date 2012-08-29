(* ========================================================================= *)
(* THEORY PACKAGE SUMMARIES                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

signature PackageSummary =
sig

(* ------------------------------------------------------------------------- *)
(* A type tracking the source package of sequents.                           *)
(* ------------------------------------------------------------------------- *)

type sequentSource = PackageNameVersion.nameVersion SequentMap.map

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

val requires : summary -> Sequents.sequents

val provides : summary -> Sequents.sequents

(* ------------------------------------------------------------------------- *)
(* Check summary.                                                            *)
(* ------------------------------------------------------------------------- *)

val check :
    {checkTheorems : bool} -> Summary.context -> Show.show -> summary -> unit

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val toHtml : Summary.context -> Show.show -> summary -> Html.block list

end
