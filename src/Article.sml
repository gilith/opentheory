(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Article :> Article =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun extract acc seen objs =
      case objs of
        [] => acc
      | obj :: objs =>
        let
          val id = ObjectProv.id obj
        in
          if IntSet.member id seen then extract acc seen objs
          else
            let
              val seen = IntSet.add seen id
            in
              case ObjectProv.provenance obj of
                ObjectProv.Pnull => extract acc seen objs
              | ObjectProv.Pcall _ => extract acc seen objs
              | ObjectProv.Preturn objR => extract acc seen (objR :: objs)
              | ObjectProv.Pcons (objH,objT) =>
                extract acc seen (objH :: objT :: objs)
              | ObjectProv.Pref objR => extract acc seen (objR :: objs)
              | ObjectProv.Pthm _ =>
                let
                  val acc = ObjectProvSet.add acc obj
                in
                  extract acc seen objs
                end
            end
        end;

in
  val thmObjects = extract ObjectProvSet.empty IntSet.empty;
end;

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype article =
    Article of
      {savable : bool,
       saved : ObjectThms.thms};

fun new {savable} =
    Article
      {savable = savable,
       saved = ObjectThms.empty};

fun saved (Article {saved = x, ...}) = ObjectThms.toThmSet x;

fun summarize article = Summary.fromThms (saved article);

fun savable (Article {savable = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun appendTextFile {known,interpretation,filename} article =
    let
      val Article {savable = knownSavable, saved = knownSaved} = known
      and Article {savable, saved = initialSaved} = article

      val _ = not savable orelse knownSavable orelse
              raise Error "savable article cannot use unsavable known"

      val state = ObjectRead.initial initialSaved

      val state =
          ObjectRead.executeTextFile
            {savable = savable,
             known = knownSaved,
             interpretation = interpretation,
             filename = filename}
          state

      val stack = ObjectRead.stack state
      and dict = ObjectRead.dict state
      and saved = ObjectRead.saved state

      val saved =
          let
            val n = ObjectStack.size stack
          in
            if n = 0 then saved
            else
              let
                val () = warn (Int.toString n ^ " object" ^
                               (if n = 1 then "" else "s") ^
                               " left on the stack by " ^ filename)

                val objs = thmObjects (ObjectStack.objects stack)

                val n =
                    let
                      val {thms = t, ...} = ObjectThms.size initialSaved
                      and {thms = t', ...} = ObjectThms.size saved
                    in
                      t' - t
                    end

                val n' = ObjectProvSet.size objs
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
                    ObjectThms.addSet saved objs
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
          let
            val {thms = n, ...} = ObjectThms.size saved
          in
            if n > 0 then ()
            else warn ("no theorems saved or left on the stack by " ^ filename)
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
    in
      Article
        {savable = savable,
         saved = saved}
    end
    handle Error err => raise Error ("Article.appendTextFile: " ^ err);

fun fromTextFile {savable,known,interpretation,filename} =
    let
      val article = new {savable = savable}

      val article =
          appendTextFile
            {known = known,
             interpretation = interpretation,
             filename = filename}
            article
    in
      article
    end
    handle Error err => raise Error ("Article.fromTextFile: " ^ err);

fun toTextFile {article,filename} =
    let
      val Article {savable,saved} = article

      val _ = savable orelse raise Error "unsavable"

      val objs = ObjectThms.toObjectSet saved
    in
      ObjectWrite.toTextFile {filename = filename} objs
    end
    handle Error err => raise Error ("Article.toTextFile: " ^ err);

end
