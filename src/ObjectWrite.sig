(* ========================================================================= *)
(* WRITING OBJECTS TO COMMANDS                                               *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectWrite =
sig

(* ------------------------------------------------------------------------- *)
(* Writing objects to a stream of commands.                                  *)
(* ------------------------------------------------------------------------- *)

val toCommandStream : ObjectProvSet.set -> Command.command Stream.stream

(* ------------------------------------------------------------------------- *)
(* Writing objects to text files.                                            *)
(* ------------------------------------------------------------------------- *)

val toTextFile : {filename : string} -> ObjectProvSet.set -> unit

end
