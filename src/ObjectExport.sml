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
(* Symbols.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun addThm (th,sym) =
      let
        val ObjectThm.Thm {proof = _, hyp, concl} = th

        val sym = ObjectSymbol.addObject sym hyp

        val sym = ObjectSymbol.addObject sym concl
      in
        sym
      end;
in
  val symbol = foldl addThm ObjectSymbol.empty;
end;

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

fun eliminateUnwanted exp =
    let
      val () =
          if savable exp then ()
          else raise Bug "ObjectExport.eliminateUnwanted: unsavable"

      val elim = ObjectUnwanted.empty

      val (exp',_) = maps ObjectThm.sharingEliminateUnwanted exp elim
    in
      exp'
    end;

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  type state = Object.object option IntMap.map * ObjectStore.store;

  val initial : state =
      let
        val seen = IntMap.new ()

        and store =
            let
              fun filter d =
                  case d of
                    ObjectData.TypeOp _ => true
                  | ObjectData.Type _ => true
                  | ObjectData.Const _ => true
                  | ObjectData.Var _ => true
                  | ObjectData.Term _ => true
                  | _ => false
            in
              ObjectStore.new {filter = filter}
            end
      in
        (seen,store)
      end;

  fun hiddenRefl obj =
      let
        val th = Object.destThm obj

        val (l,r) = Term.destEq (Thm.concl th)

        val () =
            if Term.alphaEqual l r then ()
            else raise Error "ObjectExport.hiddenRefl: not alpha equal"

        val () =
            case Object.provenance obj of
              Object.Special {command = Command.Refl, ...} =>
              raise Error "ObjectExport.hiddenRefl: already a refl"
            | _ => ()
      in
        l
      end;

  fun preDescent obj (acc : state) =
      let
        val (seen,store) = acc

        val i = Object.id obj

        val obj'' = IntMap.peek seen i
      in
        case obj'' of
          SOME obj' => {descend = false, result = (obj',acc)}
        | NONE =>
          case total hiddenRefl obj of
            NONE => {descend = true, result = (NONE,acc)}
          | SOME tm =>
            let
(*OpenTheoryTrace4
              val () = Print.trace Object.pp
                         "ObjectExport.compressProofs: obj" obj

              val () = Print.trace Object.ppProvenance
                         "ObjectExport.compressProofs: provenance"
                           (Object.provenance obj)
*)

              val store = ObjectStore.add store obj

              val (objT,store) = ObjectStore.build (ObjectData.Term tm) store

              val objR' = SOME (Object.mkRefl {savable = true} objT)

              val seen = IntMap.insert seen (i,objR')

              val acc = (seen,store)
            in
              {descend = false, result = (objR',acc)}
            end
      end;

  fun postDescent obj obj' acc =
      let
        val (seen,store) = acc

        val i = Object.id obj

        val seen = IntMap.insert seen (i,obj')

        val acc = (seen,store)
      in
        (obj',acc)
      end;

  val compressObj =
      Object.maps
        {preDescent = preDescent,
         postDescent = postDescent,
         savable = true};

  fun compressThm th acc =
      let
        val ObjectThm.Thm {proof,hyp,concl} = th

        val (proof',acc) = compressObj proof acc

        val th' =
            case proof' of
              NONE => NONE
            | SOME proof =>
              SOME (ObjectThm.Thm {proof = proof, hyp = hyp, concl = concl})
      in
        (th',acc)
      end;
in
  fun compressProofs exp =
      let
        val (exp',_) = maps compressThm exp initial
      in
        exp'
      end;
end;

local
  type state = Object.object IntMap.map;

  val toRefs =
      let
        val initial =
            let
              fun filter d =
                  case d of
                    ObjectData.Num _ => false
                  | ObjectData.Name _ => false
                  | _ => true
            in
              ObjectStore.new {filter = filter}
            end

        fun add (th,store) = ObjectThm.addStore store th;
      in
        foldl add initial
      end;

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
            val (objJ,_) = ObjectStore.build (Object.data objI) refs

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
                    {descend = true, result = (SOME objJ, acc)}
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
  fun compressRefs exp =
      let
        val refs = toRefs exp

        val (exp',_) = maps (compressThm refs) exp initial
      in
        exp'
      end;
end;

fun compress exp =
    let
      val unchanged = true

      val (unchanged,exp) =
          case compressProofs exp of
            NONE => (unchanged,exp)
          | SOME exp => (false,exp)

      val (unchanged,exp) =
          case compressRefs exp of
            NONE => (unchanged,exp)
          | SOME exp => (false,exp)
    in
      if unchanged then NONE else SOME exp
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap size (Print.ppBracket "export{" "}" Print.ppInt);

end
