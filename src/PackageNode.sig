(* ========================================================================= *)
(* THEORY NODES USED IN PACKAGES                                             *)
(* Copyright (c) 2010 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

signature PackageNode =
sig

(* ------------------------------------------------------------------------- *)
(* Types of package theory nodes.                                            *)
(* ------------------------------------------------------------------------- *)

datatype node =
    Article of
      {interpretation : Interpretation.interpretation,
       filename : string}
  | Package of
      {interpretation : Interpretation.interpretation,
       package : PackageName.name}
  | Union

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val article : node -> {filename : string} option

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val package : node -> PackageName.name option

end
