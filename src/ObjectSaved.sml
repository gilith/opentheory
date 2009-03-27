(* ========================================================================= *)
(* SAVED THEOREM OBJECTS                                                     *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure ObjectSaved :> ObjectSaved =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of saved theorem objects.                                          *)
(* ------------------------------------------------------------------------- *)

datatype saved = Saved of ObjectThms.thms;

val empty = Saved ObjectThms.empty;

fun thms (Saved t) = t;

fun union (Saved thms1) (Saved thms2) = Saved (ObjectThms.union thms1 thms2);

fun add saved obj =
    let
      val Saved thms = saved
    in
      case ObjectProv.object obj of
        Object.Othm th =>
        let
          val seq = Syntax.sequent th
        in
          case ObjectThms.search thms seq of
            SOME _ =>
            let
              val () = warn ("saving duplicate theorem:\n" ^
                             Syntax.thmToString th)
            in
              saved
            end
          | NONE =>
            let
              val thms = ObjectThms.add thms obj
            in
              Saved thms
            end
        end
      | _ => raise Error "ObjectSaved.add: not an Othm object"
    end;

fun search (Saved thms) seq =
    case ObjectThms.search thms seq of
      SOME (th,_) => SOME th
    | NONE => NONE;

end
