(* ========================================================================= *)
(* EXPORT SETS OF THEOREM OBJECTS                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectExport :> ObjectExport =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of export sets of theorem objects.                                 *)
(* ------------------------------------------------------------------------- *)

datatype export =
    Export of
      {thms : ObjectThmSet.set,
       savable : bool};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun new {savable} =
    let
      val size = 0
      and thms = ObjectThmSet.empty
    in
      Export
        {thms = thms,
         savable = savable}
    end;

val empty = new {savable = true};

fun savable (Export {savable = x, ...}) = x;

fun toSet (Export {thms = x, ...}) = x;

fun null exp = ObjectThmSet.null (toSet exp);

fun size exp = ObjectThmSet.size (toSet exp);

fun add exp th =
    let
      val Export {thms,savable} = exp

      val thms = ObjectThmSet.add thms th
    in
      Export
        {thms = thms,
         savable = savable}
    end;

fun foldl f b exp = ObjectThmSet.foldl f b (toSet exp);

fun foldr f b exp = ObjectThmSet.foldr f b (toSet exp);

fun toList exp = ObjectThmSet.toList (toSet exp);

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

fun union exp1 exp2 =
    let
      val Export {thms = ths1, savable = sav1} = exp1
      and Export {thms = ths2, savable = sav2} = exp2

      val sav = sav1 andalso sav2

      val ths = ObjectThmSet.union ths1 ths2
    in
      Export
        {thms = ths,
         savable = sav}
    end;

local
  fun uncurriedUnion (thms1,thms2) = union thms1 thms2;
in
  fun unionList thmsl =
      case thmsl of
        [] => empty
      | thms :: thmsl => List.foldl uncurriedUnion thms thmsl;
end;

(* ------------------------------------------------------------------------- *)
(* Mapping over exported theorem objects.                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun addThm f (th,(unchanged,ths,acc)) =
      let
        val (th',acc) = f th acc

        val (unchanged,th) =
            case th' of
              NONE => (unchanged,th)
            | SOME x => (false,x)

        val ths = ObjectThmSet.add ths th
      in
        (unchanged,ths,acc)
      end;
in
  fun maps f exp acc =
      let
        val Export {thms,savable} = exp

        val (thms',acc) = ObjectThmSet.maps f thms acc

        val exp' =
            case thms' of
              NONE => NONE
            | SOME thms => SOME (Export {thms = thms, savable = savable})
      in
        (exp',acc)
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

fun eliminateUnwanted exp =
    let
      val Export {savable,...} = exp

      val elim = ObjectUnwanted.new {savable = savable}

      val (exp',_) = maps ObjectThm.sharingEliminateUnwanted exp elim
    in
      exp'
    end;

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

type refs = Object.object ObjectDataMap.map;

val emptyRefs : refs = ObjectDataMap.new ();

fun improveRefs refs obj : refs =
    let
      val ob = Object.data obj

      val imp =
          case ObjectDataMap.peek refs ob of
            NONE => true
          | SOME obj' => Object.id obj < Object.id obj'
    in
      if imp then ObjectDataMap.insert refs (ob,obj) else refs
    end;

local
  type state = IntSet.set * refs;

  val initial : state = (IntSet.empty,emptyRefs);

  fun preDescent obj (acc : state) =
      let
        val (seen,refs) = acc

        val i = Object.id obj
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

  val addObj =
      Object.foldl
        {preDescent = preDescent,
         postDescent = postDescent};

  fun addThm (th,acc) =
      let
        val ObjectThm.Thm {proof,hyp,concl} = th

        val acc = addObj acc proof

        val acc = addObj acc hyp

        val acc = addObj acc concl
      in
        acc
      end;
in
  fun toRefs exp =
      let
        val (_,refs) = foldl addThm initial exp
      in
        refs
      end;
end;

local
  type state = Object.object IntMap.map;

  val initial : state = IntMap.new ();

  fun preDescent refs objI (acc : state) =
      let
        val i = Object.id objI

        val objI' = IntMap.peek acc i
      in
        case objI' of
          SOME objR =>
          let
            val objR' = if Object.equalId i objR then NONE else objI'
          in
            {descend = false, result = (objR',acc)}
          end
        | NONE =>
          let
            val obI = Object.data objI

            val objJ' = ObjectDataMap.peek refs obI

            val objJ =
                case objJ' of
                  NONE => raise Bug "ObjectExport.compressRefs.preDescent"
                | SOME obj => obj

            val j = Object.id objJ
          in
            if j = i then {descend = true, result = (NONE,acc)}
            else
              let
                val objR' = IntMap.peek acc j
              in
                case objR' of
                  NONE =>
                  let
                    val acc = IntMap.insert acc (i,objJ)
                  in
                    {descend = true, result = (objJ',acc)}
                  end
                | SOME objR =>
                  let
                    val acc = IntMap.insert acc (i,objR)
                  in
                    {descend = false, result = (objR',acc)}
                  end
              end
          end
      end;

  fun postDescent objI objR' (acc : state) =
      let
        val i = Object.id objI
      in
        case objR' of
          NONE =>
          let
            val acc = IntMap.insert acc (i,objI)
          in
            (objR',acc)
          end
        | SOME objR =>
          let
            val acc =
                case IntMap.peek acc i of
                  NONE => acc
                | SOME objJ => IntMap.insert acc (Object.id objJ, objR)

            val acc = IntMap.insert acc (i,objR)
          in
            (objR',acc)
          end
      end;

  fun compressObj refs =
      Object.maps
        {preDescent = preDescent refs,
         postDescent = postDescent,
         savable = true};

  fun compressThm refs = ObjectThm.maps (compressObj refs);
in
  fun compressRefs refs exp =
      let
        val (exp',_) = maps (compressThm refs) exp initial
      in
        exp'
      end;
end;

fun compress exp =
    let
      val refs = toRefs exp
    in
      compressRefs refs exp
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap size (Print.ppBracket "export{" "}" Print.ppInt);

end
