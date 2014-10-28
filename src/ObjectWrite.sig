(* ========================================================================= *)
(* WRITING OBJECTS TO COMMANDS                                               *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectWrite =
sig

(* ------------------------------------------------------------------------- *)
(* Writing objects to a stream of commands.                                  *)
(* ------------------------------------------------------------------------- *)

val toCommandStream :
    ArticleVersion.version -> ObjectExport.export ->
    Command.command Stream.stream

(* ------------------------------------------------------------------------- *)
(* Writing objects to text files.                                            *)
(* ------------------------------------------------------------------------- *)

val toTextFile :
    {version : ArticleVersion.version,
     export : ObjectExport.export,
     filename : string} -> unit

end
