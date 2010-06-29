(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Article :> Article =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype article =
    Article of
      {savable : bool,
       saved : ObjectThms.thms};

val empty =
    let
      val savable = true
      and saved = ObjectThms.empty
    in
      Article
        {savable = savable,
         saved = saved}
    end;

fun savable (Article {savable = x, ...}) = x;

fun saved (Article {saved = x, ...}) = x;

fun thms art = ObjectThms.thms (saved art);

(* ------------------------------------------------------------------------- *)
(* Merging articles.                                                         *)
(* ------------------------------------------------------------------------- *)

fun union art1 art2 =
    let
      val Article {savable = sav1, saved = ths1} = art1
      and Article {savable = sav2, saved = ths2} = art2

      val savable = sav1 andalso sav2

      val saved = ObjectThms.union ths1 ths2
    in
      Article
        {savable = savable,
         saved = saved}
    end;

local
  fun add (art2,art1) = union art1 art2;
in
  fun unionList arts =
      case arts of
        [] => empty
      | art :: arts => List.foldl add art arts;
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun fromTextFile {savable,import,interpretation,filename} =
    let
      val Article {savable = importSavable, saved = importSaved} = import

      val _ = not savable orelse importSavable orelse
              raise Error "savable article cannot use unsavable import"

      val parameters =
          {import = importSaved,
           interpretation = interpretation,
           savable = savable}

      val state = ObjectRead.initial parameters

      val state = ObjectRead.executeTextFile {filename = filename} state

      val stack = ObjectRead.stack state
      and dict = ObjectRead.dict state
      and exp = ObjectRead.export state

      val () =
          let
            val n = ObjectStack.size stack
          in
            if n = 0 then ()
            else
              warn (Int.toString n ^ " object" ^
                    (if n = 1 then "" else "s") ^
                    " left on the stack by " ^ filename)
          end

      val () =
          let
            val n = ObjectDict.size dict
          in
            if n = 0 then ()
            else
              warn (Int.toString n ^ " object" ^
                    (if n = 1 then "" else "s") ^
                    " left in the dictionary by " ^ filename)
          end

      val saved = ObjectThms.fromExport exp
    in
      Article
        {savable = savable,
         saved = saved}
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Article.fromTextFile: " ^ err);
*)

fun toTextFile {article,filename} =
    let
      val Article {savable,saved} = article

      val _ = savable orelse raise Error "unsavable"

      val exp = ObjectThms.toExport saved
    in
      ObjectWrite.toTextFile {export = exp, filename = filename}
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Article.toTextFile: " ^ err);
*)

end
