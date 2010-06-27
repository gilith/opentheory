(* ========================================================================= *)
(* EXPORTED THEOREM OBJECTS                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectExport :> ObjectExport =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of exported theorem objects.                                       *)
(* ------------------------------------------------------------------------- *)

datatype export = Export of Thm.thm ObjectProvMap.map;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty = Export (ObjectProvMap.new ());

fun null (Export m) = ObjectProvMap.null m;

fun size (Export m) = ObjectProvMap.size m;

fun insert (Export m) obj_th = Export (ObjectProvMap.insert m obj_th);

fun toMap (Export m) = m;

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

type refs = ObjectProv.object ObjectMap.map;

val emptyRefs : refs = ObjectMap.new ();

fun improveRefs refs obj : refs =
    let
      val ob = ObjectProv.object obj

      val imp =
          case ObjectMap.peek refs ob of
            NONE => true
          | SOME obj' => ObjectProv.id obj < ObjectProv.id obj'
    in
      if imp then ObjectMap.insert refs (ob,obj) else refs
    end;

local
  type state = IntSet.set * refs;

  val initial : state = (IntSet.empty,emptyRefs);

  fun preDescent obj (acc : state) =
      let
        val (seen,refs) = acc

        val i = ObjectProv.id obj
      in
        if IntSet.member i seen then {descend = false, result = acc}
        else {descend = true, result = (IntSet.add seen i, refs)}
      end;

  fun postDescent obj (acc : state) =
      let
        val (seen,refs) = acc

        val refs = improveRefs refs obj
      in
        (seen,refs)
      end;

  fun advance (obj,_,acc) =
      ObjectProv.foldl
        {preDescent = preDescent,
         postDescent = postDescent} acc obj;
in
  fun toRefs (Export m) =
      let
        val (_,refs) = ObjectProvMap.foldl advance initial m
      in
        refs
      end;
end;

local
  type state = ObjectProv.object IntMap.map;

  val initial : state = IntMap.new ();

  fun preDescent refs obj (acc : state) =
      case IntMap.peek acc (ObjectProv.id obj) of
        SOME obj => {descend = false, result = (obj,acc)}
      | NONE =>
        let
          val obj =
              case ObjectMap.peek refs (ObjectProv.object obj) of
                SOME obj => obj
              | NONE => raise Bug "ObjectExport.compressRefs.preDescent"
        in
          {descend = true, result = (obj,acc)}
        end;

  fun postDescent obj obj' (acc : state) =
      let
        val acc = IntMap.insert acc (ObjectProv.id obj, obj')
      in
        (obj',acc)
      end;

  fun advance refs (obj,th,(acc,exp)) =
      let
        val (obj,acc) =
            ObjectProv.maps
              {preDescent = preDescent refs,
               postDescent = postDescent} obj acc

        val exp = insert exp (obj,th)
      in
        (acc,exp)
      end;
in
  fun compressRefs refs (Export m) =
      let
        val (_,exp) = ObjectProvMap.foldl (advance refs) (initial,empty) m
      in
        exp
      end;
end;

fun compress exp =
    let
      val refs = toRefs exp
    in
      compressRefs refs exp
    end;

end
