(* ========================================================================= *)
(* THEOREMS CONTAINED IN A SET OF OBJECTS                                    *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure ObjectThms :> ObjectThms =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of object set theorems.                                            *)
(* ------------------------------------------------------------------------- *)

datatype thms = Thms of ObjectProv.object SequentMap.map * IntSet.set;

val empty = Thms (SequentMap.new (), IntSet.empty);

fun size (Thms (seqs,_)) = SequentMap.size seqs;

fun add thms obj =
    let
      val Thms (seqs,ids) = thms
      and ObjectProv.Object {id, object = ob, provenance = prov, ...} = obj
    in
      if IntSet.member id ids then thms
      else
        let
          val ids = IntSet.add ids id
        in
          case prov of
            ObjectProv.Pnull => Thms (seqs,ids)
          | ObjectProv.Pcall _ => Thms (seqs,ids)
          | ObjectProv.Preturn objR => add (Thms (seqs,ids)) objR
          | ObjectProv.Pcons (objH,objT) =>
            let
              val thms = Thms (seqs,ids)
              val thms = add thms objH
            in
              add thms objT
            end
          | ObjectProv.Pref objR => add (Thms (seqs,ids)) objR
          | ObjectProv.Pthm _ =>
            let
              val seq = Syntax.sequent (Object.destOthm ob)

              val ins =
                  case SequentMap.peek seqs seq of
                    NONE => true
                  | SOME (ObjectProv.Object {id = id', ...}) => id < id'

              val seqs = if ins then SequentMap.insert seqs (seq,obj) else seqs
            in
              Thms (seqs,ids)
            end
        end
    end;

local
  fun choose (obj1,obj2) =
      let
        val ObjectProv.Object {id = id1, ...} = obj1
        and ObjectProv.Object {id = id2, ...} = obj2

        val obj = if id1 < id2 then obj1 else obj2
      in
        SOME obj
      end;
in
  fun union thms1 thms2 =
      let
        val Thms (seqs1,ids1) = thms1
        and Thms (seqs2,ids2) = thms2

        val seqs = SequentMap.union choose seqs1 seqs2

        val ids = IntSet.union ids1 ids2
      in
        Thms (seqs,ids)
      end;
end;

fun search (Thms (seqs,_)) seq =
    case SequentMap.peek seqs seq of
      NONE => NONE
    | SOME obj =>
      let
        val ob = ObjectProv.object obj
        val th = Rule.alpha seq (Object.destOthm ob)
      in
        SOME (th,[obj])
      end;

local
  fun add (_,obj,set) = ObjectProvSet.add set obj;
in
  fun toObjectSet (Thms (seqs,_)) =
      SequentMap.foldl add ObjectProvSet.empty seqs;
end;

end
