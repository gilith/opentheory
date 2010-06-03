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
    Article of Interpretation.interpretation * {filename : string}
  | Package of Interpretation.interpretation * PackageName.name
  | Union;

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun destArticle node =
    case node of
      Article (_,f) => SOME f
    | _ => NONE;

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun destPackage node =
    case node of
      Package (_,n) => SOME n
    | _ => NONE;

end
