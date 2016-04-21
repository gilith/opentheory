(* ========================================================================= *)
(* PACKAGE INTERPRETATIONS                                                   *)
(* Copyright (c) 2016 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageInterpretation :> PackageInterpretation =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "int";

(* ------------------------------------------------------------------------- *)
(* Interpretation filenames.                                                 *)
(* ------------------------------------------------------------------------- *)

fun mkFilename {base} =
    let
      val filename =
          OS.Path.joinBaseExt
            {base = base,
             ext = SOME fileExtension}
    in
      {filename = filename}
    end;

fun destFilename {filename} =
    let
      val {base,ext} = OS.Path.splitBaseExt (OS.Path.file filename)
    in
      case ext of
        SOME x => if x = fileExtension then SOME {base = base} else NONE
      | NONE => NONE
    end;

fun isFilename file = Option.isSome (destFilename file);

fun normalizeFilename {filename} =
    let
      val base = OS.Path.base (OS.Path.file filename)
    in
      mkFilename {base = base}
    end;

(* ------------------------------------------------------------------------- *)
(* A type of theory package interpretations.                                 *)
(* ------------------------------------------------------------------------- *)

datatype interpretation =
    Interpretation of
      {rewrites : Interpretation.rewrite list,
       filenames : {filename : string} list};

(* ------------------------------------------------------------------------- *)
(* Interpretation files.                                                     *)
(* ------------------------------------------------------------------------- *)

fun filenames (Interpretation {filenames = x, ...}) = x;

fun filenamesList ints = List.concat (List.map filenames ints);

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

local
  fun compareFilename (f1,f2) =
      let
        val {filename = s1} = f1
        and {filename = s2} = f2
      in
        String.compare (s1,s2)
      end;

  val compareRewrites = lexCompare Interpretation.compareRewrite;

  val compareFilenames = lexCompare compareFilename;
in
  fun compare (int1,int2) =
      let
        val Interpretation {rewrites = r1, filenames = f1} = int1
        and Interpretation {rewrites = r2, filenames = f2} = int2
      in
        case compareRewrites (r1,r2) of
          LESS => LESS
        | EQUAL => compareFilenames (f1,f2)
        | GREATER => GREATER
      end;
end;

local
  fun equalFilename f1 f2 =
      let
        val {filename = s1} = f1
        and {filename = s2} = f2
      in
        s1 = s2
      end;

  val equalRewrites = listEqual Interpretation.equalRewrite;

  val equalFilenames = listEqual equalFilename;
in
  fun equal int1 int2 =
      let
        val Interpretation {rewrites = r1, filenames = f1} = int1
        and Interpretation {rewrites = r2, filenames = f2} = int2
      in
        equalRewrites r1 r2 andalso equalFilenames f1 f2
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Realizing the theory package interpretation.                              *)
(* ------------------------------------------------------------------------- *)

fun realizeFilename {directory} {filename} =
    let
      val f = OS.Path.concat (directory,filename)
    in
      Interpretation.fromTextFileRewriteList {filename = f}
    end;

fun realize dir int =
    let
      val Interpretation {rewrites = r, filenames = fl} = int

      val rl = List.map (realizeFilename dir) fl
    in
      Interpretation.fromRewriteList (List.concat (r :: rl))
    end;

end
