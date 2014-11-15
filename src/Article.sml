(* ========================================================================= *)
(* ARTICLES OF PROOF IN HIGHER ORDER LOGIC                                   *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Article :> Article =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val fileExtension = "art";

(* ------------------------------------------------------------------------- *)
(* Article filenames.                                                        *)
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
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype article =
    Article of
      {savable : bool,
       thms : ObjectThms.thms,
       inference : Inference.inference};

fun new {savable} =
    let
      val thms = ObjectThms.new {savable = savable}
      and inference = Inference.empty
    in
      Article
        {savable = savable,
         thms = thms,
         inference = inference}
    end;

val empty = new {savable = true};

fun savable (Article {savable = x, ...}) = x;

fun objects (Article {thms = x, ...}) = x;

fun thms art = ObjectThms.thms (objects art);

fun inference (Article {inference = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Merging articles.                                                         *)
(* ------------------------------------------------------------------------- *)

fun union art1 art2 =
    let
      val Article {savable = sav1, thms = ths1, inference = _} = art1
      and Article {savable = sav2, thms = ths2, inference = _} = art2

      val savable = sav1 andalso sav2

      val thms = ObjectThms.union ths1 ths2

      val inference = Inference.empty
    in
      Article
        {savable = savable,
         thms = thms,
         inference = inference}
    end;

local
  fun add (art2,art1) = union art1 art2;
in
  fun unionList arts = List.foldl add empty arts;
end;

(* ------------------------------------------------------------------------- *)
(* Article summaries.                                                        *)
(* ------------------------------------------------------------------------- *)

fun summary art = Summary.fromThms (thms art);

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun fromTextFile {savable,import,interpretation,filename} =
    let
      val Article
            {savable = importSavable,
             thms = importThms,
             inference = _} = import

(*OpenTheoryDebug
      val () =
          if not savable orelse importSavable then ()
          else
            let
              val bug =
                  "Article.fromTextFile: " ^
                  "savable article cannot use unsavable import"
            in
              raise Bug bug
            end
*)
      val parameters =
          {import = importThms,
           interpretation = interpretation,
           savable = savable}

      val state =
          ObjectRead.executeTextFile
            {parameters = parameters,
             filename = filename}

      val stack = ObjectRead.stack state
      and dict = ObjectRead.dict state

      val () =
          let
            val n = ObjectStack.size stack
          in
            if n = 0 then ()
            else
              let
                val msg =
                    Print.toString Print.ppPrettyInt n ^ " object" ^
                    (if n = 1 then "" else "s") ^
                    " left on the stack by " ^ filename
              in
                warn msg
              end
          end

      val () =
          let
            val n = ObjectDict.size dict
          in
            if n = 0 then ()
            else
              let
                val msg =
                    Print.toString Print.ppPrettyInt n ^ " object" ^
                    (if n = 1 then "" else "s") ^
                    " left in the dictionary by " ^ filename
              in
                warn msg
              end
          end

      val inf = ObjectRead.inference state
      and exp = ObjectRead.export state

      val ths = ObjectThms.fromExport exp
    in
      Article
        {savable = savable,
         thms = ths,
         inference = inf}
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Article.fromTextFile:\n" ^ err);
*)

fun toTextFile {article,version,filename} =
    let
      val Article {savable, thms, inference = _} = article

(*OpenTheoryDebug
      val () =
          if savable then ()
          else raise Error "Article.toTextFile: unsavable"
*)
      val exp = ObjectThms.toExport thms

      val exp =
          case ObjectExport.eliminateUnwanted exp of
            NONE => exp
          | SOME exp => exp

      val exp =
          case ObjectExport.setVersion version exp of
            NONE => exp
          | SOME exp => exp

      val exp =
          case ObjectExport.compress exp of
            NONE => exp
          | SOME exp => exp

      val () = ObjectExport.checkClash exp

      val () =
          ObjectWrite.toTextFile
            {version = version,
             export = exp,
             filename = filename}
    in
      ()
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Article.toTextFile:\n" ^ err);
*)

end
