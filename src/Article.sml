(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Article :> Article =
struct

open Useful Syntax Rule;

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype article =
    Article of
      {thms : ThmSet.set,
       saved : objectSet option};

val empty =
    Article
      {thms = ThmSet.empty,
       saved = SOME emptyObjectSet};

fun append art1 art2 =
    let
      val Article {thms = thms1, saved = saved1} = art1
      and Article {thms = thms2, saved = saved2} = art2

      val thms = ThmSet.union thms1 thms2

      val saved =
          case (saved1,saved2) of
            (SOME s1, SOME s2) => SOME (unionObjectSet s1 s2)
          | _ => NONE
    in
      Article
        {thms = thms,
         saved = saved}
    end;

fun saved (Article {thms = x, ...}) = x;

fun prove article = findAlpha (saved article);

val summarize = Summary.fromThms o saved;

fun isSavable (Article {saved,...}) = Option.isSome saved;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

local
  (* Comment lines *)

  fun isComment l =
      case List.find (not o Char.isSpace) l of
        NONE => true
      | SOME #"#" => true
      | _ => false;
in
  fun executeTextFile {savable,known,interpretation,filename} =
      let
        (* Estimating parse error line numbers *)

        val lines = Stream.fromTextFile {filename = filename}

        val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
      in
        (let
           (* The character stream *)

           val chars = Stream.filter (not o isComment) chars

           val chars = Parse.everything Parse.any chars

           (* The command stream *)

           val commands = Parse.everything spacedCommandParser chars
         in
           executeCommands savable known interpretation commands
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in article file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

fun fromTextFile {savable,known,interpretation,filename} =
    let
      val State {stack,dict,saved} =
          executeTextFile
            {savable = savable,
             known = known,
             filename = filename,
             interpretation = interpretation}

      val saved = theoremsSaved saved

      val saved =
          let
            val n = sizeStack stack
          in
            if n = 0 then saved
            else
              let
                val () = warn (Int.toString n ^ " object" ^
                               (if n = 1 then "" else "s") ^
                               " left on the stack by " ^ filename)

                val saved' = theoremsStack stack
                val saved' = unionTheorems saved saved'

                val n = sizeTheorems saved
                val n' = sizeTheorems saved' - n
              in
                if n = 0 then
                  let
                    val () =
                        if n' = 0 then ()
                        else
                          warn ("saving " ^ Int.toString n' ^ " theorem" ^
                                (if n' = 1 then "" else "s") ^
                                " left on the stack by " ^ filename)
                  in
                    saved'
                  end
                else
                  let
                    val () =
                        if n' = 0 then ()
                        else
                          warn (Int.toString n' ^ " unsaved theorem" ^
                                (if n' = 1 then "" else "s") ^
                                " left on the stack by " ^ filename)
                  in
                    saved
                  end
              end
          end

      val () =
          if sizeTheorems saved > 0 then ()
          else warn ("no theorems saved or left on the stack by " ^ filename)

      val () =
          let
            val n = sizeDict dict
          in
            if n = 0 then ()
            else
              warn (Int.toString n ^ " object" ^
                    (if n = 1 then "" else "s") ^
                    " left in the dictionary by " ^ filename)
          end

      val saved = toObjectSetTheorems saved

      val thms = toThmSetObjectSet saved

      val saved = if savable then SOME saved else NONE
    in
      Article
        {thms = thms,
         saved = saved}
    end
    handle Error err => raise Error ("Article.fromTextFile: " ^ err);

fun toTextFile {article,filename} =
    let
      val Article {saved,...} = article

      val saved =
          case saved of
            SOME s => toListObjectSet s
          | NONE => raise Error "unsavable"

      val (objs,saved) = reduceObject saved

      val saved = fromListObjectSet saved

(*OpenTheoryTrace3
      val () = Print.trace ppObjectSet "Article.toTextFile: objs" objs
      val () = Print.trace ppObjectSet "Article.toTextFile: saved" saved
*)

      val commands = generate saved objs

      val lines = Stream.map (fn c => commandToString c ^ "\n") commands
    in
      Stream.toTextFile {filename = filename} lines
    end
    handle Error err => raise Error ("Article.toTextFile: " ^ err);

end
