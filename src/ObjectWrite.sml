(* ========================================================================= *)
(* WRITING OBJECTS TO COMMANDS                                               *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectWrite :> ObjectWrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Minimal dictionaries.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype minDict =
    MinDict of
      {refs : int ObjectMap.map,
       keys : int ObjectMap.map,
       nextKey : int};

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

  fun registerGenerated (ob,refs) =
      if ObjectMap.inDomain ob refs then refs
      else ObjectMap.insert refs (ob,0);

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
            SOME k => ObjectMap.insert refs (ob, k + 1)
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
        val (h,c) = Object.mkSequent (Thm.sequent th)

        val refs = registerSpecial (obj,refs)

        val refs = registerDefault refs [h,c]
      in
        refs
      end;
in
  fun newMinDict exp =
      let
        val refs = ObjectMap.new ()

        val refs = ObjectExport.foldl registerThm refs exp

        val refs = ObjectMap.filter (fn (_,n) => n >= 1) refs

        val keys = ObjectMap.new ()

        val nextKey = 1
      in
        MinDict
          {refs = refs,
           keys = keys,
           nextKey = nextKey}
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Bug ("ObjectWrite.newMinDict: " ^ err);
*)
end;

datatype task =
    ExpTask of ObjectProv.object * Thm.thm
  | ObjTask of ObjectProv.object
  | ObTask of Object.object
  | GenTask of Object.object list * int
  | CmdTask of Command.command;

local
  fun isKey (MinDict {keys,...}) ob = ObjectMap.inDomain ob keys;

  fun addKey cmds dict ob =
      let
        val MinDict {refs,keys,nextKey} = dict

        val pointless = ObjectMap.inDomain ob keys orelse
                        not (ObjectMap.inDomain ob refs)
      in
        if pointless then (cmds,dict)
        else
          let
            val key = nextKey

            val keys = ObjectMap.insert keys (ob,key)

            val nextKey = nextKey + 1

            val dict =
                MinDict
                  {refs = refs,
                   keys = keys,
                   nextKey = nextKey}

            val cmds = Command.Def :: Command.Num key :: cmds
          in
            (cmds,dict)
          end
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("addKey: " ^ err);
*)

  fun addGen (ob,(cmds,dict)) =
      let
        val (cmds,dict) = addKey cmds dict ob

        val cmds = Command.Pop :: cmds
      in
        (cmds,dict)
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("addGen: " ^ err);
*)

  fun useKey dict ob =
      let
        val MinDict {refs,keys,nextKey} = dict
      in
        case ObjectMap.peek keys ob of
          NONE => NONE
        | SOME key =>
          case ObjectMap.peek refs ob of
            NONE => raise Error "no such object"
          | SOME n =>
            if n = 1 then
              let
                val refs = ObjectMap.delete refs ob

                val keys = ObjectMap.delete keys ob

                val dict =
                    MinDict
                      {refs = refs,
                       keys = keys,
                       nextKey = nextKey}

                val cmds = [Command.Num key, Command.Remove]
              in
                SOME (cmds,dict)
              end
            else
              let
                val refs = ObjectMap.insert refs (ob, n - 1)

                val dict =
                    MinDict
                      {refs = refs,
                       keys = keys,
                       nextKey = nextKey}

                val cmds = [Command.Num key, Command.Ref]
              in
                SOME (cmds,dict)
              end
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("useKey: " ^ err);
*)
in
  fun generateMinDict (dict,work) =
      case work of
        [] =>
        let
(*OpenTheoryDebug
          val MinDict {refs,keys,...} = dict

          val _ = ObjectMap.null refs orelse
                  raise Error "nonempty refs"

          val _ = ObjectMap.null keys orelse
                  raise Error "nonempty keys"
*)
        in
          NONE
        end
      | task :: work =>
        case task of
          ExpTask (obj,th) =>
          let
            val (h,c) = Object.mkSequent (Thm.sequent th)

            val work =
                ObjTask obj ::
                ObTask h ::
                ObTask c ::
                CmdTask Command.Thm ::
                work
          in
            SOME ([],(dict,work))
          end
        | ObjTask obj =>
          let
            val ob = ObjectProv.object obj
          in
            case useKey dict ob of
              SOME (cmds,dict) => SOME (cmds,(dict,work))
            | NONE =>
              case ObjectProv.provenance obj of
                ObjectProv.Default =>
                let
                  val (cmd,args) = Object.command ob
                  and gen = [ob]
                  and res = 0

                  val work =
                      map ObTask args @
                      CmdTask cmd ::
                      GenTask (gen,res) ::
                      work
                in
                  SOME ([],(dict,work))
                end
              | ObjectProv.Special
                  {command = cmd,
                   arguments = args,
                   generated = gen,
                   result = res} =>
                let
                  val work =
                      map ObjTask args @
                      CmdTask cmd ::
                      GenTask (gen,res) ::
                      work
                in
                  SOME ([],(dict,work))
                end
          end
        | ObTask ob =>
          (case useKey dict ob of
             SOME (cmds,dict) => SOME (cmds,(dict,work))
           | NONE =>
             let
               val (cmd,args) = Object.command ob
               and gen = [ob]
               and res = 0

               val work =
                   map ObTask args @
                   CmdTask cmd ::
                   GenTask (gen,res) ::
                   work
             in
               SOME ([],(dict,work))
             end)
        | GenTask (gen,res) =>
(***
          if res = 0 then
            let
              val post = List.drop (gen, res + 1)

              val cmds = 
            in
              SOME (cmds,(dict,work))
            end
          else
***)
            let
              val (cmds,dict) = List.foldl addGen ([],dict) (rev gen)

              val (cmds',dict) =
                  case useKey dict (List.nth (gen,res)) of
                    SOME cmds_dict => cmds_dict
                  | NONE => raise Error "vanishing object"

              val cmds = List.revAppend (cmds,cmds')
            in
              SOME (cmds,(dict,work))
            end
        | CmdTask cmd =>
          let
            val cmds = [cmd]
          in
            SOME (cmds,(dict,work))
          end;
end;

(***
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
  fun generateMinDict dict cmds objTh =
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
***)

(* ------------------------------------------------------------------------- *)
(* Writing objects to a stream of commands.                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun generateStream dict_work () =
      case generateMinDict dict_work of
        NONE => Stream.Nil
      | SOME (cmds,dict_work) => Stream.Cons (cmds, generateStream dict_work);
in
  fun toCommandStream exp =
      let
        val dict = newMinDict exp
        and work = map ExpTask (ObjectExport.toList exp)

        val strm = generateStream (dict,work) ()
      in
        Stream.listConcat strm
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Bug ("ObjectWrite.toCommandStream: " ^ err);
*)
end;

(* ------------------------------------------------------------------------- *)
(* Writing objects to text files.                                            *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {export,filename} =
    let
      val commands = toCommandStream export

      val lines = Stream.map (fn c => Command.toString c ^ "\n") commands
    in
      Stream.toTextFile {filename = filename} lines
    end
(*OpenTheoryDebug
    handle Error err =>
      raise Bug ("ObjectWrite.toTextFile: " ^ err);
*)

end
