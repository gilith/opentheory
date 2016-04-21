(* ========================================================================= *)
(* PACKAGE INTERPRETATIONS                                                   *)
(* Copyright (c) 2016 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageInterpretation :> PackageInterpretation =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory package interpretations.                                 *)
(* ------------------------------------------------------------------------- *)

datatype interpretation =
    Interpretation of
      {rewrites : Interpretation.rewrite list,
       filenames : {filename : string} list};

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
