(* ========================================================================= *)
(* THEORY NODES USED IN PACKAGES                                             *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageNode =
sig

(* ------------------------------------------------------------------------- *)
(* Types of package theory nodes.                                            *)
(* ------------------------------------------------------------------------- *)

datatype node =
    Article of Interpretation.interpretation * {filename : string}
  | Package of Interpretation.interpretation * PackageName.name
  | Union

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val destArticle : node -> {filename : string} option

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val destPackage : node -> PackageName.name option

end
