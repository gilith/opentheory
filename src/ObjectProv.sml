(* ========================================================================= *)
(* OPENTHEORY OBJECTS THAT TRACK THEIR PROVENANCE                            *)
(* Copyright (c) 2009-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure ObjectProv :> ObjectProv =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

type id = int;

val newId : unit -> id =
    let
      val counter = ref 0
    in
      fn () =>
         let
           val ref count = counter
           val () = counter := count + 1
(*OpenTheoryTrace1
           val () = if count mod 1000 = 0 then trace "." else ()
*)
         in
           count
         end
    end;

(* ------------------------------------------------------------------------- *)
(* Object provenance.                                                        *)
(*                                                                           *)
(* Invariants *in order of priority*                                         *)
(*                                                                           *)
(* 1. The provenance of an Ocall object is the object that became the call   *)
(*    argument.                                                              *)
(*                                                                           *)
(* 2. Objects do not contain theorems iff they have provenance Pnull.        *)
(*    [Objects that do not contain theorems can be easily constructed.]      *)
(*                                                                           *)
(* 3. The provenance of a return value is the object that became the return  *)
(*    value.                                                                 *)
(*                                                                           *)
(* 4. If a theorem can be inferred from theorem-containing objects, then the *)
(*    provenance of the theorem is these objects.                            *)
(*    [This will happen most often when simulating an inference rule and     *)
(*    making use of the call argument - resulting in a singleton list.]      *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Object of
      {id : id,
       object : Object.object,
       provenance : provenance,
       call : object option}

and provenance =
    Pnull
  | Pcall of object
  | Preturn of object
  | Pcons of object * object
  | Pref of object
  | Pthm of inference

and inference =
    Isaved
  | Isimulated
  | Istack of object
  | Iaxiom;

fun compare (Object {id = i1, ...}, Object {id = i2, ...}) =
    Int.compare (i1,i2);

fun mk {object,provenance,call} =
    let
      val id = newId ()
    in
      Object
        {id = id,
         object = object,
         provenance = provenance,
         call = call}
    end;

fun id (Object {id = x, ...}) = x;

fun object (Object {object = x, ...}) = x;

fun provenance (Object {provenance = x, ...}) = x;

fun call (Object {call = x, ...}) = x;

fun callStack obj =
    case call obj of
      NONE => []
    | SOME c => c :: callStack c;

fun destCallProvenance p =
    case p of
      Pcall obj => obj
    | _ => raise Error "ObjectProv.destCallProvenance";

fun destCall (Object {object = f, provenance = a, ...}) =
    let
      val f = Object.destOcall f
      and a = destCallProvenance a
    in
      (f,a)
    end;

fun parentsInference inf =
    case inf of
      Isaved => []
    | Isimulated => []
    | Istack obj => [obj]
    | Iaxiom => [];

fun parentsProvenance prov =
    case prov of
      Pnull => []
    | Pcall obj => [obj]
    | Preturn obj => [obj]
    | Pcons (objH,objT) => [objH,objT]
    | Pref obj => [obj]
    | Pthm inf => parentsInference inf;

fun parents (Object {provenance = prov, call = c, ...}) =
    let
      val pars = parentsProvenance prov
    in
      case c of
        SOME obj => obj :: pars
      | NONE => pars
    end;

fun containsThms obj =
    case obj of
      Object {object = Object.Ocall _, ...} =>
      raise Bug "ObjectProv.containsThms: Ocall"
    | Object {provenance = Pnull, ...} => false
    | _ => true;

(* ------------------------------------------------------------------------- *)
(* Mapping with state over objects.                                          *)
(* ------------------------------------------------------------------------- *)

local
  infix ==

  val op== = Portable.pointerEqual;

  fun genMaps lr {preDescent,postDescent} =
      let
        fun mapsObj obj acc =
            let
              val {descend,result} = preDescent obj acc
            in
              if not descend then result
              else
                let
                  val (obj,acc) = result

                  val Object {id, object = ob, provenance = prov, call} = obj

                  val (prov',call',acc) =
                      if lr then
                        let
                          val (call',acc) = Sharing.mapsOption mapsObj call acc
                          val (prov',acc) = mapsProv prov acc
                        in
                          (prov',call',acc)
                        end
                      else
                        let
                          val (prov',acc) = mapsProv prov acc
                          val (call',acc) = Sharing.mapsOption mapsObj call acc
                        in
                          (prov',call',acc)
                        end

                  val obj =
                      if prov' == prov andalso call' == call then obj
                      else
                        Object
                          {id = id,
                           object = ob,
                           provenance = prov',
                           call = call'}
                in
                  postDescent obj acc
                end
            end

        and mapsProv prov acc =
            case prov of
              Pnull => (prov,acc)
            | Pcall obj => mapsProv1 prov acc Pcall obj
            | Preturn obj => mapsProv1 prov acc Preturn obj
            | Pcons (objH,objT) =>
              let
                val (objH',objT',acc) =
                    if lr then
                      let
                        val (objH',acc) = mapsObj objH acc
                        val (objT',acc) = mapsObj objT acc
                      in
                        (objH',objT',acc)
                      end
                    else
                      let
                        val (objT',acc) = mapsObj objT acc
                        val (objH',acc) = mapsObj objH acc
                      in
                        (objH',objT',acc)
                      end

                val prov' =
                    if objH' == objH andalso objT' == objT then prov
                    else Pcons (objH',objT')
              in
                (prov',acc)
              end
            | Pref obj => mapsProv1 prov acc Pref obj
            | Pthm inf =>
              let
                val (inf',acc) = mapsInf inf acc

                val prov' = if inf' == inf then prov else Pthm inf'
              in
                (prov',acc)
              end

        and mapsProv1 prov acc con obj =
            let
              val (obj',acc) = mapsObj obj acc
              val prov' = if obj' == obj then prov else con obj'
            in
              (prov',acc)
            end

        and mapsInf inf acc =
            case inf of
              Isaved => (inf,acc)
            | Isimulated => (inf,acc)
            | Istack obj =>
              let
                val (obj',acc) = mapsObj obj acc
                val inf' = if obj' == obj then inf else Istack obj'
              in
                (inf',acc)
              end
            | Iaxiom => (inf,acc);
      in
        mapsObj
      end;
in
  fun maps pre_post obj acc = genMaps true pre_post obj acc;

  fun revMaps pre_post obj acc = genMaps false pre_post obj acc;
end;

(* ------------------------------------------------------------------------- *)
(* Generating commands to keep the call stack consistent.                    *)
(* ------------------------------------------------------------------------- *)

fun addAlignCalls prevCall call cmds =
    case prevCall of
      NONE =>
      let
        val _ = not (Option.isSome call) orelse
                raise Bug "ObjectProv.alignCalls: top level to nested"
      in
        cmds
      end
    | SOME (Object {id, object = ob, call = prevCall, ...}) =>
      let
        val aligned =
            case call of
              NONE => false
            | SOME (Object {id = id', ...}) => id = id'
      in
        if aligned then cmds
        else
          let
            val n =
                case ob of
                  Object.Ocall n => n
                | _ => raise Bug "ObjectProv.alignCalls: bad call"

            val cmds =
                Command.Return :: Command.Name n :: Command.Error :: cmds
          in
            addAlignCalls prevCall call cmds
          end
      end;

fun alignCalls {prevCall,call} = addAlignCalls prevCall call [];

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp level obj =
    let
      val Object {id, object = ob, provenance = prov, call} = obj
      val level = level - 1
    in
      if level = ~1 then Print.ppInt id
      else
        Print.blockProgram Print.Consistent 2
          [Print.addString "Object",
           Print.addBreak 1,
           Print.record
             [("id", Print.ppInt id),
              ("object", Object.pp ob),
              ("provenance", ppProvenance level prov),
              ("call", Print.ppOption (pp level) call)]]
    end

and ppProvenance level prov =
    case prov of
      Pnull => Print.addString "Pnull"
    | Pcall obj =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pcall",
         Print.addBreak 1,
         pp level obj]
    | Preturn obj =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Preturn",
         Print.addBreak 1,
         pp level obj]
    | Pcons objH_objT =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pcons",
         Print.addBreak 1,
         Print.ppPair (pp level) (pp level) objH_objT]
    | Pref obj =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pref",
         Print.addBreak 1,
         pp level obj]
    | Pthm inf =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pthm",
         Print.addBreak 1,
         ppInference level inf]

and ppInference level inf =
    case inf of
      Isaved => Print.addString "Isaved"
    | Isimulated => Print.addString "Isimulated"
    | Istack obj =>
      Print.blockProgram Print.Consistent 1
        [Print.addString "(Istack",
         Print.addBreak 1,
         pp level obj,
         Print.addString ")"]
    | Iaxiom => Print.addString "Iaxiom";
end

structure ObjectProvOrdered =
struct type t = ObjectProv.object val compare = ObjectProv.compare end

structure ObjectProvSet =
struct

local
  structure S = ElementSet (ObjectProvOrdered);
in
  open S;
end;

local
  fun add (obj,set) =
      let
        val ob = ObjectProv.object obj
        val th = Object.destOthm ob
      in
        ThmSet.add set th
      end;
in
  fun toThmSet set = foldl add ThmSet.empty set;
end;

local
  fun ancs set [] = set
    | ancs set (obj :: objs) =
      if member obj set then ancs set objs
      else ancs (add set obj) (ObjectProv.parents obj @ objs);

  fun addAncestors (obj,set) = ancs set [obj];
in
  fun ancestors obj = addAncestors (obj,empty);

  val ancestorSet = foldl addAncestors empty;
end;

local
  fun toStrm NONE = Stream.Nil
    | toStrm (SOME iter) = Stream.Cons (readIterator iter, toDelay iter)

  and toDelay iter () = toStrm (advanceIterator iter)
in
  fun toStream s = toStrm (mkIterator s);
end;

val pp = Print.ppBracket "{" "}" (Print.ppMap size Print.ppInt);

(* ------------------------------------------------------------------------- *)
(* Reducing object sets.                                                     *)
(* ------------------------------------------------------------------------- *)

local
  open Useful;

  fun improve refs obj =
      let
        val ObjectProv.Object {id, object = ob, provenance = prov, call} = obj

        fun better rid =
            rid < id andalso
            case prov of
              ObjectProv.Preturn obj =>
              let
                val ObjectProv.Object {id = rid', ...} = obj
              in
                rid < rid'
              end
            | ObjectProv.Pref obj =>
              let
                val ObjectProv.Object {id = rid', ...} = obj
              in
                rid < rid'
              end
            | _ => true

        val obj' =
            case prov of
              ObjectProv.Pnull => NONE
            | ObjectProv.Pcall _ => NONE
            | _ =>
              case ObjectMap.peek refs ob of
                NONE => NONE
              | SOME (robj as ObjectProv.Object {id = rid, ...}) =>
                if not (better rid) then NONE
                else
                  let
                    val prov = ObjectProv.Pref robj

                    val obj =
                        ObjectProv.Object
                          {id = id,
                           object = ob,
                           provenance = prov,
                           call = call}
                  in
                    SOME obj
                  end

(*OpenTheoryTrace4
        val ppObject = ObjectProv.pp 1
        val ppImprovement = Print.ppOp2 " -->" ppObject ppObject
        val () =
            case obj' of
              SOME x => Print.trace ppImprovement "Article.improve" (obj,x)
            | NONE => ()
*)
      in
        obj'
      end;

  fun preReduce obj acc =
      let
        val (reqd,refs) = acc

        val (descend,obj) =
            case peek reqd obj of
              SOME obj => (false,obj)
            | NONE => (true, Option.getOpt (improve refs obj, obj))
      in
        {descend = descend,
         result = (obj,acc)}
      end;

  fun postReduce obj (reqd,refs) =
      let
        val ObjectProv.Object {id, object = ob, ...} = obj

        val reqd = add reqd obj

        val insertRefs =
            case ObjectMap.peek refs ob of
              NONE => true
            | SOME (ObjectProv.Object {id = id', ...}) => id < id'

        val refs = if insertRefs then ObjectMap.insert refs (ob,obj) else refs
      in
        (obj,(reqd,refs))
      end;

  val reduceObject =
      ObjectProv.maps {preDescent = preReduce, postDescent = postReduce};

  val reduceObjectList = Sharing.maps reduceObject;

(*OpenTheoryDebug
  val checkReduced =
      let
        fun check (obj,obs) =
            let
              val ObjectProv.Object objData = obj
              val {id, object = ob, provenance = prov, call} = objData
            in
              case prov of
                ObjectProv.Pnull => obs
              | ObjectProv.Pcall _ => obs
              | ObjectProv.Preturn _ => obs
              | _ =>
                case ObjectMap.peek obs ob of
                  NONE => ObjectMap.insert obs (ob,obj)
                | SOME obj' =>
                  let
                    val ObjectProv.Object {id = id', ...} = obj'
                  in
                    case prov of
                      ObjectProv.Pref (ObjectProv.Object {id = rid, ...}) =>
                      if rid = id' then obs
                      else raise Error "does not reference initial instance"
                    | _ => raise Error "is not a reference"
                  end
                  handle Error err =>
                    let
                      val ppObject = ObjectProv.pp 1
                      val () = Print.trace ppObject "initial" obj'
                      val () = Print.trace ppObject "duplicate" obj
                      val () =
                          if member obj' (ancestors obj) then
                            trace "duplicate depends on initial\n"
                          else
                            trace "duplicate does not depend on initial\n"
                    in
                      raise Error ("duplicate " ^ err)
                    end
            end
      in
        fn reqd =>
           let
             val acc = ObjectMap.new ()
             val acc = foldl check acc reqd
           in
             ()
           end
      end;
*)
in
  fun compress objs =
      let
        val reqd = empty
        val refs = ObjectMap.new ()

        val (objs,(_,refs)) = reduceObjectList objs (reqd,refs)

        val (objs,(reqd,refs')) = reduceObjectList objs (reqd,refs)

(*OpenTheoryDebug
        val _ = Portable.pointerEqual (refs,refs') orelse
                raise Error "references changed"
        val () = checkReduced reqd
*)
      in
        objs
      end
(*OpenTheoryDebug
      handle Error err => raise Bug ("ObjectProvSet.compress: " ^ err);
*)
end;

end

structure ObjectProvMap = KeyMap (ObjectProvOrdered)
