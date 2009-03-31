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

datatype thms =
    Thms of
      {objs : ObjectProvSet.set,
       seqs : (Thm.thm * ObjectProv.object) SequentMap.map,
       seen : IntSet.set};

val empty =
    let
      val objs = ObjectProvSet.empty
      val seqs = SequentMap.new ()
      val seen = IntSet.empty
    in
      Thms
        {objs = objs,
         seqs = seqs,
         seen = seen}
    end;

local
  fun adds objA seqs seen objs =
      case objs of
        [] => (seqs,seen)
      | obj :: objs =>
        let
          val id = ObjectProv.id obj
        in
          if IntSet.member id seen then adds objA seqs seen objs
          else
            let
              val seen = IntSet.add seen id
            in
              case ObjectProv.provenance obj of
                ObjectProv.Pnull => adds objA seqs seen objs
              | ObjectProv.Pcall _ => adds objA seqs seen objs
              | ObjectProv.Preturn objR => adds objA seqs seen (objR :: objs)
              | ObjectProv.Pcons (objH,objT) =>
                adds objA seqs seen (objH :: objT :: objs)
              | ObjectProv.Pref objR => adds objA seqs seen (objR :: objs)
              | ObjectProv.Pthm _ =>
                let
                  val th =
                      case ObjectProv.object obj of
                        Object.Othm th => th
                      | _ => raise Bug "ObjectThms.add: bad thm"

                  val seq = Syntax.sequent th

                  val seqs =
                      if SequentMap.inDomain seq seqs then seqs
                      else SequentMap.insert seqs (seq,(th,objA))
                in
                  adds objA seqs seen objs
                end
            end
        end;
in
  fun add thms obj =
      let
        val Thms {objs,seqs,seen} = thms

        val objs = ObjectProvSet.add objs obj
        and (seqs,seen) = adds obj seqs seen [obj]
      in
        Thms
          {objs = objs,
           seqs = seqs,
           seen = seen}
      end;
end;

fun search (Thms {seqs,...}) seq =
    case SequentMap.peek seqs seq of
      NONE => NONE
    | SOME (th,obj) =>
      let
        val th = Rule.alpha seq th
      in
        SOME (th,obj)
      end;

local
  fun add (_,(th,_),set) = ThmSet.add set th;
in
  fun toThmSet (Thms {seqs,...}) =
      SequentMap.foldl add ThmSet.empty seqs;
end;

fun toObjectSet (Thms {objs,...}) = objs;

end
