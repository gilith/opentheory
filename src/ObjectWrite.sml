(* ========================================================================= *)
(* WRITING OBJECTS TO COMMANDS                                               *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectWrite :> ObjectWrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun revAppend [] s = s ()
  | revAppend (h :: t) s = revAppend t (K (Stream.Cons (h,s)));

fun revConcat strm =
    case strm of
      Stream.Nil => Stream.Nil
    | Stream.Cons (h,t) => revAppend h (revConcat o t);

(* ------------------------------------------------------------------------- *)
(* Minimal dictionaries.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype minDict =
    MinDict of
      {nextKey : int,
       refs : int ObjectMap.map,
       keys : int ObjectMap.map};

fun nullMinDict (MinDict {keys,...}) = ObjectMap.null keys;

local
  fun storable ob =
      case ob of
        Object.Num _ => false
      | Object.Name _ => false
      | Object.TypeOp _ => true
      | Object.Type _ => true
      | Object.Const _ => true
      | Object.Var _ => true
      | Object.Term _ => true
      | Object.Thm _ => true
      | Object.List l => not (null l);

  fun registerReference (ob,refs) =
      let
        val p = ObjectMap.peek refs ob

        val known = Option.isSome p

        val k = Option.getOpt (p,0)

        val refs = ObjectMap.insert refs (ob, k + 1)
      in
        (known,refs)
      end;

  fun registerDefault refs obs =
      case obs of
        [] => refs
      | ob :: obs =>
        if not (storable ob) then registerDefault refs obs
        else
          let
            val (known,refs) = registerReference (ob,refs)

            val obs = if known then obs else snd (Object.command ob) @ obs
          in
            registerDefault refs obs
          end;

  fun registerSpecial (obj,refs) =
      let
        val ob = ObjectProv.object obj
      in
        if not (storable ob) then refs
        else
          case ObjectMap.peek refs ob of
            SOME k => ObjectMap.insert refs (k + 1)
          | NONE =>
            case ObjectProv.provenance obj of
              ObjectProv.Default => registerDefault refs [ob]
            | ObjectProv.Special {arguments = args, generated = gen, ...} =>
              let
                val refs = List.foldl registerSpecial refs args

                val (known,refs) = registerReference (ob,refs)

(*OpenTheoryDebug
                val _ = not known orelse
                        raise Bug "ObjectWrite.registerSpecial: redundancy"
*)

                val refs = List.foldl registerGenerated refs gen
              in
                refs
              end
      end;

  fun registerThm (obj,th,refs) =
      let
        val refs = ob = ObjectProv.object obj
      in
        if not (storable ob) then refs
        else
          case ObjectMap.peek refs ob of
            SOME k => ObjectMap.insert refs (k + 1)
          | NONE =>
            case ObjectProv.provenance obj of
              ObjectProv.Default => registerDefault refs [ob]
            | ObjectProv.Special {arguments = args, generated = gen, ...} =>
              let
                val refs = List.foldl registerSpecial refs args

                val (known,refs) = registerReference (ob,refs)

(*OpenTheoryDebug
                val _ = not known orelse
                        raise Bug "ObjectWrite.registerSpecial: redundancy"
*)

                val refs = List.foldl registerGenerated refs gen
              in
                refs
              end
      end;
in
  fun newMinDict objs =
      let
        val nextKey = 1

        val refs = ObjectMap.new ()

        val refs = ObjectProvSet.foldl register refs objs

        val refs = ObjectMap.filter (fn (_,n) => n >= 2) refs

        val keys = ObjectMap.new ()
      in
        MinDict
          {nextKey = nextKey,
           refs = refs,
           keys = keys}
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Bug ("ObjectWrite.newMinDict: " ^ err);
*)
end;

local
  fun isKey (MinDict {keys,...}) ob = ObjectMap.inDomain ob keys;

  fun addKey dict cmds ob =
      let
        val MinDict {nextKey,refs,keys} = dict

(*OpenTheoryDebug
        val _ = not (ObjectMap.inDomain ob keys) orelse
                raise Error "deja vu ob"
*)
      in
        case ObjectMap.peek refs ob of
          NONE => (dict,cmds)
        | SOME n =>
          let
            val key = nextKey
            val nextKey = nextKey + 1
            val keys = ObjectMap.insert keys (ob,key)
            val refs = ObjectMap.insert refs (ob, n - 1)
            val dict =
                MinDict {nextKey = nextKey, refs = refs, keys = keys}
            val cmds = [Command.Def, Command.Num key] @ cmds
          in
            (dict,cmds)
          end
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("addKey: " ^ err);
*)

  fun useKey dict cmds ob =
      let
        val MinDict {nextKey,refs,keys} = dict
      in
        case ObjectMap.peek keys ob of
          NONE => raise Error "no such key"
        | SOME key =>
          let
            val cmds = Command.Num key :: cmds
          in
            case ObjectMap.peek refs ob of
              NONE => raise Error "no such object"
            | SOME n =>
              if n = 1 then
                let
                  val refs = ObjectMap.delete refs ob
                  val keys = ObjectMap.delete keys ob
                  val dict =
                      MinDict {nextKey = nextKey, refs = refs, keys = keys}
                  val cmds = Command.Remove :: cmds
                in
                  (dict,cmds)
                end
              else
                let
                  val refs = ObjectMap.insert refs (ob, n - 1)
                  val dict =
                      MinDict {nextKey = nextKey, refs = refs, keys = keys}
                  val cmds = Command.Ref :: cmds
                in
                  (dict,cmds)
                end
          end
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("useKey: " ^ err);
*)

  fun generateDeep (ob,(dict,cmds)) =
      if isKey dict ob then useKey dict cmds ob
      else
        let
          val (cmd,pars) = Object.toCommand ob
          val (dict,cmds) = foldl generateDeep (dict,cmds) pars
          val cmds = cmd :: cmds
        in
          addKey dict cmds ob
        end;
in
  fun generateMinDict stack dict cmds obj =
      let
        val ob = ObjectProv.object obj
      in
        case ObjectProv.provenance obj of
          ObjectProv.Pnull =>
          let
            val (dict,cmds) = generateDeep (ob,(dict,cmds))

            val stack = ObjectStack.push stack obj
          in
            (stack,dict,cmds)
          end
        | ObjectProv.Pcall objA =>
          let
            val n =
                case ob of
                  Object.Call n => n
                | _ => raise Error "Pcall: bad call"

            val cmds = Command.Call :: Command.Name n :: cmds

            val (stack,objA') = ObjectStack.pop1 stack

            val stack = ObjectStack.push stack obj

            val stack = ObjectStack.push stack objA

(*OpenTheoryDebug
            val _ = ObjectProv.id objA = ObjectProv.id objA' orelse
                    raise Error "Pcall: wrong call argument"
*)
          in
            (stack,dict,cmds)
          end
        | ObjectProv.Pcons (objH,objT) =>
          let
            val cmds = Command.Cons :: cmds

            val (dict,cmds) = addKey dict cmds ob

            val (stack,objH',objT') = ObjectStack.pop2 stack

(*OpenTheoryDebug
            val _ = ObjectProv.id objH = ObjectProv.id objH' orelse
                    raise Error "Pcons: wrong head value"

            val _ = ObjectProv.id objT = ObjectProv.id objT' orelse
                    raise Error "Pcons: wrong tail value"
*)

            val stack = ObjectStack.push stack obj
          in
            (stack,dict,cmds)
          end
        | ObjectProv.Pref _ =>
          let
            val (dict,cmds) = useKey dict cmds ob

            val stack = ObjectStack.push stack obj
          in
            (stack,dict,cmds)
          end
        | ObjectProv.Pthm inf =>
          let
            val (dict,cmds) =
                case inf of
                  ObjectProv.Ialpha iobj =>
                  let
                    val iob = ObjectProv.object iobj

                    val (dict,cmds) = useKey dict cmds iob

                    val (dict,cmds) = generateDeep (ob,(dict,cmds))

                    val cmds = Command.Pop :: Command.Pop :: cmds
                  in
                    useKey dict cmds ob
                  end
                | _ => generateDeep (ob,(dict,cmds))

            val stack = ObjectStack.push stack obj
          in
            (stack,dict,cmds)
          end
      end
(*OpenTheoryDebug
      handle Error err =>
        let
          val ppObject = ObjectProv.pp 1

          val ppStack = Print.ppMap ObjectStack.objects (Print.ppList ppObject)

          val () = Print.trace ppStack
                     "ObjectWrite.generateMinDict: stack" stack

          val () = Print.trace ppObject
                     "ObjectWrite.generateMinDict: obj" obj
      in
        raise Bug ("ObjectWrite.generateMinDict: " ^ err)
      end;
*)
end;

(* ------------------------------------------------------------------------- *)
(* Writing objects to a stream of commands.                                  *)
(* ------------------------------------------------------------------------- *)

fun toCommandStream export =
    let
      fun gen (greatestUse,obj) (stack,dict) =
          let
            val (stack,cmds) = ObjectStack.alignUses greatestUse stack

            val (stack,dict,cmds) =
                generateMinDict stack dict cmds obj

            val cmds =
                if not (ObjectProvSet.member obj saved) then cmds
                else
                  let
                    val cmds = Command.Save :: cmds
(*OpenTheoryDebug
                    val _ = ObjectProv.isThm obj orelse
                            raise Error "can only save Othm objects"
*)
                  in
                    cmds
                  end
          in
            (cmds,(stack,dict))
          end

      fun finish (stack,dict) =
          let
(*OpenTheoryDebug
            val _ = nullMinDict dict orelse
                    raise Error "nonempty dict"
*)
            val (stack,cmds) =
                ObjectStack.alignUses {greatestUse = NONE} stack

(*OpenTheoryDebug
            val _ = ObjectStack.null stack orelse
                    raise Error "nonempty stack"
*)
          in
            if null cmds then Stream.Nil else Stream.singleton cmds
          end

      val objs = ObjectProvSet.ancestors saved

      val stack = ObjectStack.empty
      val dict = newMinDict objs

      val (uses,objs) = ObjectProvSet.toGreatestUseList objs

(*OpenTheoryDebug
      val _ = ObjectProvSet.null uses orelse
              raise Error "start requires a use"
*)

      val strm = Stream.fromList objs
      val strm = Stream.maps gen finish (stack,dict) strm
    in
      revConcat strm
    end
(*OpenTheoryDebug
    handle Error err =>
      raise Bug ("ObjectWrite.toCommandStream: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Writing objects to text files.                                            *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {export,filename} =
    let
(*OpenTheoryTrace3
      val () = Print.trace ObjectProvSet.pp
                 "ObjectWrite.toTextFile: uncompressed objs" objs
*)

      val objs = ObjectProvSet.compress objs

(*OpenTheoryTrace3
      val () = Print.trace ObjectProvSet.pp
                 "ObjectWrite.toTextFile: compressed objs" objs
*)

      val commands = toCommandStream objs

      val lines = Stream.map (fn c => Command.toString c ^ "\n") commands
    in
      Stream.toTextFile {filename = filename} lines
    end
(*OpenTheoryDebug
    handle Error err =>
      raise Bug ("ObjectWrite.toTextFile: " ^ err);
*)

end
