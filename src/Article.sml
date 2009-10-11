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

val empty =
    let
      val savable = true

      val saved = ObjectThms.empty
    in
      Article
        {savable = savable,
         saved = saved}
    end;

fun saved (Article {saved = x, ...}) = ObjectThms.toThmSet x;

fun savable (Article {savable = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Appending articles.                                                       *)
(* ------------------------------------------------------------------------- *)

fun append art1 art2 =
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
  fun add (art2,art1) = append art1 art2;
in
  fun concat arts =
      case arts of
        [] => empty
      | art :: arts => List.foldl add art arts;
end;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

fun fromTextFile {savable,known,simulations,interpretation,filename} =
    let
      val Article {savable = knownSavable, saved = knownSaved} = known

      val _ = not savable orelse knownSavable orelse
              raise Error "savable article cannot use unsavable known"

      val parameters =
          {known = knownSaved,
           simulations = simulations,
           interpretation = interpretation,
           savable = savable}

      val state = ObjectRead.initial parameters

      val state = ObjectRead.executeTextFile {filename = filename} state

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

                val {thms = n, ...} = ObjectThms.size saved

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
    handle Error err => raise Error ("Article.fromTextFile: " ^ err);

fun toTextFile {article,filename} =
    let
      val Article {savable,saved} = article

      val _ = savable orelse raise Error "unsavable"

      val objs = ObjectThms.objects saved
    in
      ObjectWrite.toTextFile {filename = filename} objs
    end
    handle Error err => raise Error ("Article.toTextFile: " ^ err);

end
