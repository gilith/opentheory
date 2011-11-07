(* ========================================================================= *)
(* OBJECT STORE                                                              *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectStore :> ObjectStore =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of object stores.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype store =
    Store of
      {filter : ObjectData.data -> bool,
       data : Object.object ObjectDataMap.map,
       seen : IntSet.set};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun new {filter} =
    let
      val data = ObjectDataMap.new ()
      and seen = IntSet.empty
    in
      Store
        {filter = filter,
         data = data,
         seen = seen}
    end;

(* ------------------------------------------------------------------------- *)
(* Adding objects.                                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun addUnseen store obj =
      let
        val Store {filter,data,seen} = store

        val i = Object.id obj
      in
        if IntSet.member i seen then NONE
        else
          let
            val seen = IntSet.add seen i

            val store =
                Store
                  {filter = filter,
                   data = data,
                   seen = seen}
          in
            SOME store
          end
      end;

  fun preDescent obj store =
      case addUnseen store obj of
        NONE => {descend = false, result = store}
      | SOME store => {descend = true, result = store};

  fun addObj obj store =
      let
        val Store {filter,data,seen} = store

        val d = Object.data obj
      in
        if not (filter d) then store
        else
          let
            val improvement =
                case ObjectDataMap.peek data d of
                  NONE => true
                | SOME obj' => Object.id obj < Object.id obj'
          in
            if not improvement then store
            else
              let
                val data = ObjectDataMap.insert data (d,obj)
              in
                Store
                  {filter = filter,
                   data = data,
                   seen = seen}
              end
          end
      end;

  fun addUnseenObj (obj,store) =
      case addUnseen store obj of
        NONE => store
      | SOME store => addObj obj store;

  fun postDescent obj store =
      let
        val store = List.foldl addUnseenObj store (Object.definitions obj)

        val store = addObj obj store
      in
        store
      end;
in
  val add =
      Object.foldl
        {preDescent = preDescent,
         postDescent = postDescent};
end;

(* ------------------------------------------------------------------------- *)
(* Looking up objects.                                                       *)
(* ------------------------------------------------------------------------- *)

fun peek store d =
    let
      val Store {data,...} = store
    in
      ObjectDataMap.peek data d
    end;

fun get store d =
    case peek store d of
      SOME obj => obj
    | NONE => raise Error "ObjectStore.get";

(* ------------------------------------------------------------------------- *)
(* Using the store to construct objects.                                     *)
(* ------------------------------------------------------------------------- *)

local
  val savable = {savable = true};
in
  fun build d store =
      case peek store d of
        SOME obj => (obj,store)
      | NONE =>
        let
          val (cmd,ds) = ObjectData.command d

          val (objs,store) = maps build ds store

          val obj =
              case Object.mkCommand savable cmd objs of
                [x] => x
              | _ => raise Bug "ObjectStore.build: not a unique object"

          val store = add store obj
        in
          (obj,store)
        end
end;

end
