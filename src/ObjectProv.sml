(* ========================================================================= *)
(* OPENTHEORY OBJECTS THAT TRACK THEIR PROVENANCE                            *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectProv :> ObjectProv =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of objects that track their provenance.                            *)
(* ------------------------------------------------------------------------- *)

type id = int;

datatype object =
    Object of
      {id : id,
       object : object'}

and object' =
    Object' of
      {object : Object.object,
       provenance : provenance}

and provenance =
    Default
  | Special of
      {command : Command.command,
       arguments : object list,
       result : int};

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

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

fun id (Object {id = x, ...}) = x;

fun equalId i obj = i = id obj;

fun compare (Object {id = i1, ...}, Object {id = i2, ...}) =
    Int.compare (i1,i2);

(* ------------------------------------------------------------------------- *)
(* A type of provenances.                                                    *)
(* ------------------------------------------------------------------------- *)

fun isDefaultProvenance prov =
    case prov of
      Default => true
    | Special _ => false;

fun parentsProvenance prov =
    case prov of
      Default => []
    | Special {arguments,...} => arguments;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun object' (Object' {object = x, ...}) = x;

fun provenance' (Object' {provenance = x, ...}) = x;

fun mk obj' =
    let
      val id = newId ()
    in
      Object
        {id = id,
         object = obj'}
    end;

fun dest (Object {id = _, object = x}) = x;

fun object obj = object' (dest obj);

fun provenance obj = provenance' (dest obj);

fun isDefault obj = isDefaultProvenance (provenance obj);

fun allDefault objs = List.all isDefault objs;

fun parents obj = parentsProvenance (provenance obj);

(* ------------------------------------------------------------------------- *)
(* Constructing objects from commands.                                       *)
(* ------------------------------------------------------------------------- *)

fun mkProv ob prov = mk (Object' {object = ob, provenance = prov});

fun mkDefault ob = mkProv ob Default;

(* Special commands *)

fun mkNum i = mkDefault (Object.Num i);

fun mkName n = mkDefault (Object.Name n);

(* Regular commands *)

fun mkAbsTerm {savable} objV objB =
    let
      val obV = object objV
      and obB = object objB

      val ob =
          let
            val v = Object.destVar obV
            and b = Object.destTerm obB
          in
            Object.Term (Term.mkAbs (v,b))
          end

      val args = [objV,objB]

      val prov =
          if not savable orelse allDefault args then Default
          else
            Special
              {command = Command.AbsTerm,
               arguments = args,
               result = 0}
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAbsTerm: " ^ err);
*)

fun mkAppTerm {savable} objF objA =
    let
      val obF = object objF
      and obA = object objA

      val ob =
          let
            val f = Object.destTerm obF
            and a = Object.destTerm obA
          in
            Object.Term (Term.mkApp (f,a))
          end

      val args = [objF,objA]

      val prov =
          if not savable orelse allDefault args then Default
          else
            Special
              {command = Command.AppTerm,
               arguments = args,
               result = 0}
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAppTerm: " ^ err);
*)

fun mkAxiom {savable} objH objC seq =
    let
      val ob = Object.Thm (Thm.axiom seq)

      val args = [objH,objC]

      val prov =
          if not savable orelse allDefault args then Default
          else
            Special
              {command = Command.Axiom,
               arguments = args,
               result = 0}
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAppTerm: " ^ err);
*)

fun mkCons {savable} objH objT =
    let
      val obH = object objH
      and obT = object objT

      val ob =
          let
            val l = Object.destList obT
          in
            Object.List (obH :: l)
          end

      val args = [objH,objT]

      val prov =
          if not savable orelse allDefault args then Default
          else
            Special
              {command = Command.Cons,
               arguments = args,
               result = 0}
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkCons: " ^ err);
*)

fun mkConst c = mkDefault (Object.Const c);

fun mkConstTerm {savable} objC objT =
    let
      val obC = object objC
      and obT = object objT

      val ob =
          let
            val c = Object.destConst obC
            and ty = Object.destType obT
          in
            Object.Term (Term.mkConst (c,ty))
          end

      val args = [objC,objT]

      val prov =
          if not savable orelse allDefault args then Default
          else
            Special
              {command = Command.ConstTerm,
               arguments = args,
               result = 0}
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkConstTerm: " ^ err);
*)

val mkNil = mkDefault (Object.List []);

fun mkOpType {savable} objO objL =
    let
      val obO = object objO
      and obL = object objL

      val ob =
          let
            val ot = Object.destTypeOp obO
            and tys = Object.destTypes obL
          in
            Object.mkOpType (ot,tys)
          end

      val args = [objO,objL]

      val prov =
          if not savable orelse allDefault args then Default
          else
            Special
              {command = Command.OpType,
               arguments = args,
               result = 0}
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkOpType: " ^ err);
*)

fun mkTypeOp ot = mkDefault (Object.TypeOp ot);

fun mkVar {savable} objN objT =
    let
      val obN = object objN
      and obT = object objT

      val ob =
          let
            val n = Object.destName obN
            and ty = Object.destType obT
          in
            Object.Var (Var.mk (n,ty))
          end

      val args = [objN,objT]

      val prov =
          if not savable orelse allDefault args then Default
          else
            Special
              {command = Command.Var,
               arguments = args,
               result = 0}
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkVar: " ^ err);
*)

fun mkVarTerm {savable} objV =
    let
      val obV = object objV

      val ob =
          let
            val v = Object.destVar obV
          in
            Object.Term (Term.mkVar v)
          end

      val args = [objV]

      val prov =
          if not savable orelse allDefault args then Default
          else
            Special
              {command = Command.VarTerm,
               arguments = args,
               result = 0}
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkVarTerm: " ^ err);
*)

fun mkVarType objN =
    let
      val obN = object objN

      val ob =
          let
            val n = Object.destName obN
          in
            Object.mkVarType n
          end
    in
      mkDefault ob
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkVarType: " ^ err);
*)

(***
(* ------------------------------------------------------------------------- *)
(* Building objects.                                                         *)
(* ------------------------------------------------------------------------- *)

fun build savable srch =
    let
      fun bld ob =
          case ob of
            Object.List (obH :: obT) =>
            let
              val objH = bld obH

              val objT = bld (Object.List obT)
            in
              mkCons objH objT
            end
          | Object.Thm th =>
            let
              val inf = srch th
            in
              mkThm savable th inf
            end
          | Object.Call _ => raise Error "cannot build an Ocall object"
          | _ =>
            let
              val prov = Pnull
            in
              mk ob prov
            end
    in
      bld
    end
(*OpenTheoryDebug
    handle Error err => raise Bug ("ObjectProv.build: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Updating provenance (for compression).                                    *)
(* ------------------------------------------------------------------------- *)

fun updateProvenance obj prov =
    let
      val Object {id = i, object = ob, ...} = obj
    in
      Object {id = i, object = ob, provenance = prov}
    end;

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

                  val prov = provenance obj

                  val (prov',acc) = mapsProv prov acc

                  val obj =
                      if prov' == prov then obj
                      else updateProvenance obj prov'
                in
                  postDescent obj acc
                end
            end

        and mapsProv prov acc =
            case prov of
              Pnull => (prov,acc)
            | Pcall obj => mapsProv1 prov acc Pcall obj
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
              Ialpha obj => mapsInf1 inf acc Ialpha obj
            | Isimulated obj => mapsInf1 inf acc Isimulated obj
            | Iaxiom => (inf,acc)

        and mapsInf1 inf acc con obj =
            let
              val (obj',acc) = mapsObj obj acc

              val inf' = if obj' == obj then inf else con obj'
            in
              (inf',acc)
            end;
      in
        mapsObj
      end;
in
  fun maps pre_post obj acc = genMaps true pre_post obj acc;

  fun revMaps pre_post obj acc = genMaps false pre_post obj acc;
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp level obj =
    let
      val Object {id, object = ob, provenance = prov} = obj

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
              ("provenance", ppProvenance level prov)]]
    end

and ppProvenance level prov =
    case prov of
      Pnull => Print.addString "Pnull"
    | Pcall obj =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pcall",
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
      Ialpha obj =>
      Print.blockProgram Print.Consistent 1
        [Print.addString "(Ialpha",
         Print.addBreak 1,
         pp level obj,
         Print.addString ")"]
    | Isimulated obj =>
      Print.blockProgram Print.Consistent 1
        [Print.addString "(Isimulated",
         Print.addBreak 1,
         pp level obj,
         Print.addString ")"]
    | Iaxiom => Print.addString "Iaxiom";
***)
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

(***
local
  fun add (obj,set) =
      let
        val ob = ObjectProv.object obj

        val th = Object.destThm ob
      in
        ThmSet.add set th
      end;
in
  fun toThmSet set = foldl add ThmSet.empty set;
end;

val greatestId =
    let
      fun someId obj = SOME (ObjectProv.id obj)
    in
      firstr someId
    end;

local
  fun ancs set [] = set
    | ancs set (obj :: objs) =
      if member obj set then ancs set objs
      else ancs (add set obj) (ObjectProv.parents obj @ objs);

  fun addAncestors (obj,set) = ancs set [obj];
in
  fun ancestorsObject obj = addAncestors (obj,empty);

  val ancestors = foldl addAncestors empty;
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
        val id = ObjectProv.id obj
        and ob = ObjectProv.object obj
        and prov = ObjectProv.provenance obj

        fun better rid =
            rid < id andalso
            case prov of
              ObjectProv.Pref obj => rid < ObjectProv.id obj
            | _ => true

        val obj' =
            case prov of
              ObjectProv.Pnull => NONE
            | ObjectProv.Pcall _ => NONE
            | _ =>
              case ObjectMap.peek refs ob of
                NONE => NONE
              | SOME robj =>
                if not (better (ObjectProv.id robj)) then NONE
                else
                  let
                    val prov = ObjectProv.Pref robj

                    val obj = ObjectProv.updateProvenance obj prov
                  in
                    SOME obj
                  end

(*OpenTheoryTrace4
        val ppObject = ObjectProv.pp 1
        val ppImprovement = Print.ppOp2 " -->" ppObject ppObject
        val () =
            case obj' of
              SOME x => Print.trace ppImprovement "ObjectProvSet.improve" (obj,x)
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
        val ob = ObjectProv.object obj

        val reqd = add reqd obj

        val insertRefs =
            case ObjectMap.peek refs ob of
              NONE => true
            | SOME robj => ObjectProv.id obj < ObjectProv.id robj

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
              val id = ObjectProv.id obj
              and ob = ObjectProv.object obj
              and prov = ObjectProv.provenance obj
            in
              case prov of
                ObjectProv.Pnull => obs
              | ObjectProv.Pcall _ => obs
              | _ =>
                case ObjectMap.peek obs ob of
                  NONE => ObjectMap.insert obs (ob,obj)
                | SOME obj' =>
                  let
                    val id' = ObjectProv.id obj'
                  in
                    case prov of
                      ObjectProv.Pref robj =>
                      let
                        val rid = ObjectProv.id robj
                      in
                        if rid = id' then obs
                        else raise Error "does not reference initial instance"
                      end
                    | _ => raise Error "is not a reference"
                  end
                  handle Error err =>
                    let
                      val ppObject = ObjectProv.pp 1
                      val () = Print.trace ppObject "initial" obj'
                      val () = Print.trace ppObject "duplicate" obj
                      val () =
                          if member obj' (ancestorsObject obj) then
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
  fun compressList objs =
      let
(*OpenTheoryDebug
        val objsOrig = objs
*)

        val reqd = empty

        val refs = ObjectMap.new ()

        val (objs,(_,refs)) = reduceObjectList objs (reqd,refs)

        val (objs,(reqd,refs')) = reduceObjectList objs (reqd,refs)

(*OpenTheoryDebug
        val _ = Portable.pointerEqual (refs,refs') orelse
                raise Error "references changed"

        val () = checkReduced reqd

        val _ = lexCompare ObjectProv.compare (objsOrig,objs) = EQUAL orelse
                raise Error "object list changed"
*)
      in
        objs
      end
(*OpenTheoryDebug
      handle Error err => raise Bug ("ObjectProvSet.compressList: " ^ err);
*)
end;

fun compress objs =
    let
      val objs = toList objs

      val objs = compressList objs

      val objs = fromList objs
    in
      objs
    end;

(* ------------------------------------------------------------------------- *)
(* Computing dependencies.                                                   *)
(* ------------------------------------------------------------------------- *)

val stackUses =
    let
      fun add (obj,acc) = addList acc (ObjectProv.stackUses obj)
    in
      foldr add empty
    end;

val toGreatestCallList =
    let
      fun findCalls (obj,(calls,objs)) =
          let
            val calls =
                case ObjectProv.provenance obj of
                  ObjectProv.Pcall _ => delete calls obj
                | ObjectProv.Pthm (ObjectProv.Isimulated cobj) =>
                  add calls cobj
                | _ => calls

            val call = greatestId calls

            val objs = ({greatestCall = call}, obj) :: objs
          in
            (calls,objs)
          end
    in
      foldr findCalls (empty,[])
    end;

val toGreatestUseList =
    let
      fun findUses (obj,(uses,objs)) =
          let
            val uses = if member obj uses then delete uses obj else uses

            val uses = addList uses (ObjectProv.stackUses obj)

            val uses =
                case ObjectProv.provenance obj of
                  ObjectProv.Pthm (ObjectProv.Isimulated objC) =>
                  add uses objC
                | _ => uses

            val uid = greatestId uses

            val objs = ({greatestUse = uid}, obj) :: objs
          in
            (uses,objs)
          end
    in
      foldr findUses (empty,[])
    end;
***)

end

structure ObjectProvMap = KeyMap (ObjectProvOrdered)
