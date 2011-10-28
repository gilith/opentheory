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

datatype thm =
    Thm of
      {proof : ObjectProv.object,
       hyp : ObjectProv.object,
       concl : ObjectProv.object};

fun toThm th =
    let
      val Thm {proof,hyp,concl} = th

      val t = ObjectProv.destThm proof
      and seq = ObjectProv.destSequent (hyp,concl)
    in
      Rule.alpha seq t
    end;

(* ------------------------------------------------------------------------- *)
(* A type of exported theorem lists.                                         *)
(* ------------------------------------------------------------------------- *)

datatype export =
    Export of
      {size : int,
       thms : thm list,
       savable : bool};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun new {savable} =
    let
      val size = 0
      and thms = []
    in
      Export
        {size = size,
         thms = thms,
         savable = savable}
    end;

fun size (Export {size = x, ...}) = x;

fun thms (Export {thms = x, ...}) = x;

fun null exp = size exp = 0;

fun add exp th =
    let
      val Export {size,thms,savable} = exp

      val size = size + 1
      and thms = th :: thms
    in
      Export
        {size = size,
         thms = thms,
         savable = savable}
    end;

fun foldr f b exp = List.foldl f b (thms exp);

fun toList exp = List.rev (thms exp);

fun foldl f b exp = List.foldl f b (toList exp);

fun maps f =
    let
      fun mapsThms l acc =
          case l of
            [] => (NONE,acc)
          | h :: t =>
            let
              val (t',acc) = mapsThms t acc

              val (h',acc) = f h acc

              val unchanged = true

              val (unchanged,t) =
                  case t' of
                    NONE => (unchanged,t)
                  | SOME x => (false,x)

              val (unchanged,h) =
                  case h' of
                    NONE => (unchanged,h)
                  | SOME x => (false,x)

              val l' = if unchanged then NONE else SOME (h :: t)
            in
              (l',acc)
            end
    in
      fn exp => fn acc =>
         let
           val Export {size,thms,savable} = exp

           val (thms',acc) = mapsThms thms acc
         in
           case thms' of
             NONE => (NONE,acc)
           | SOME thms =>
             let
               val exp = Export {size = size, thms = thms, savable = savable}
             in
               (SOME exp, acc)
             end
         end
    end;

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun eliminateThm th elim =
      let
        val Thm {proof,hyp,concl} = th

        val (proof',elim) = ObjectUnwanted.sharingEliminate proof elim

        val (hyp',elim) = ObjectUnwanted.sharingEliminate hyp elim

        val (concl',elim) = ObjectUnwanted.sharingEliminate concl elim

        val unchanged = true

        val (unchanged,proof) =
            case proof' of
              NONE => (unchanged,proof)
            | SOME obj => (false,obj)

        val (unchanged,hyp) =
            case hyp' of
              NONE => (unchanged,hyp)
            | SOME obj => (false,obj)

        val (unchanged,concl) =
            case concl' of
              NONE => (unchanged,concl)
            | SOME obj => (false,obj)

        val th' =
            if unchanged then NONE
            else SOME (Thm {proof = proof, hyp = hyp, concl = concl})
      in
        (th',elim)
      end;
in
  fun eliminateUnwanted exp =
      let
        val Export {savable,...} = exp

        val elim = ObjectUnwanted.new {savable = savable}

        val (exp',_) = maps eliminateThm exp elim
      in
        exp'
      end;
end;

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

  val addObj =
      ObjectProv.foldl
        {preDescent = preDescent,
         postDescent = postDescent};

  fun addThm (th,acc) =
      let
        val Thm {proof,hyp,concl} = th

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
  type state = ObjectProv.object IntMap.map;

  val initial : state = IntMap.new ();

  fun preDescent refs objI (acc : state) =
      let
        val i = ObjectProv.id objI

        val objI' = IntMap.peek acc i
      in
        case objI' of
          SOME objR =>
          let
            val objR' = if ObjectProv.equalId i objR then NONE else objI'
          in
            {descend = false, result = (objR',acc)}
          end
        | NONE =>
          let
            val obI = ObjectProv.object objI

            val objJ' = ObjectMap.peek refs obI

            val objJ =
                case objJ' of
                  NONE => raise Bug "ObjectExport.compressRefs.preDescent"
                | SOME obj => obj

            val j = ObjectProv.id objJ
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
        val i = ObjectProv.id objI
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
                | SOME objJ => IntMap.insert acc (ObjectProv.id objJ, objR)

            val acc = IntMap.insert acc (i,objR)
          in
            (objR',acc)
          end
      end;

  fun advance refs (obj,th,(acc,exp)) =
      let
        val (obj',acc) =
            ObjectProv.maps
              {preDescent = preDescent refs,
               postDescent = postDescent,
               savable = true} obj acc

        val obj = Option.getOpt (obj',obj)

        val exp = insert exp (obj,th)
      in
        (acc,exp)
      end;
in
  fun compressRefs refs exp =
      let
        val (_,exp) = foldl (advance refs) (initial,empty) exp
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

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap size (Print.ppBracket "export{" "}" Print.ppInt);

end
