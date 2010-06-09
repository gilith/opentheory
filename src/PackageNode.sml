(* ========================================================================= *)
(* THEORY NODES USED IN PACKAGES                                             *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageNode :> PackageNode =
struct

open Useful;

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
  | Union;

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun article node =
    case node of
      Article {filename = f, ...} => SOME {filename = f}
    | _ => NONE;

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun package node =
    case node of
      Package {package = p, ...} => SOME p
    | _ => NONE;

end
