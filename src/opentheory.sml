(* ========================================================================= *)
(* THE OPENTHEORY PROGRAM FOR PROCESSING THEORY PACKAGES                     *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

open Useful;

val () =
    let
      val args = CommandLine.arguments ()

      val () = Tool.main args
    in
      OS.Process.exit OS.Process.success
    end
    handle Error s => die (Tool.name^" failed:\n" ^ s)
         | Bug s => die ("BUG found in "^Tool.name^" program:\n" ^ s)
         | e => die (Tool.name^" exception:\n"^ exnMessage e);
