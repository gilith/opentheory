(* ========================================================================= *)
(* WRITING OBJECTS TO COMMANDS                                               *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectWrite :> ObjectWrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Objects complex enough to be worth storing in a dictionary.               *)
(* ------------------------------------------------------------------------- *)

fun storableObject ob =
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

(* ------------------------------------------------------------------------- *)
(* Minimal dictionaries.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype minDict =
    MinDict of
      {refs : int ObjectMap.map,
       keys : int ObjectMap.map,
       nextKey : int};

local
  fun registerGenerated (ob,refs) =
      if ObjectMap.inDomain ob refs then refs
      else ObjectMap.insert refs (ob,0);

  fun registerReference (ob,refs) =
      let
        val k = Option.getOpt (ObjectMap.peek refs ob, 0)
      in
        ObjectMap.insert refs (ob, k + 1)
      end;

  fun registerDefault (ob,refs) =
      if not (storableObject ob) then refs
      else if ObjectMap.inDomain ob refs then registerReference (ob,refs)
      else registerDefault' (ob,refs)

  and registerDefault' (ob,refs) =
      let
        val (_,args) = Object.command ob

        val refs = List.foldl registerDefault refs args
      in
        registerGenerated (ob,refs)
      end;

  stop;

  fun registerSpecial (obj,refs) =
      let
(*OpenTheoryTrace5
*)
          val () = Print.trace ObjectProv.pp
                     "ObjectWrite.registerSpecial: obj" obj
        val ob = ObjectProv.object obj
      in
        if not (storableObject ob) then refs
        else if ObjectMap.inDomain ob refs then registerReference (ob,refs)
        else
          case ObjectProv.provenance obj of
            ObjectProv.Default => registerDefault' (ob,refs)
          | ObjectProv.Special
              {arguments = args,
               generated = gen,
               result = res,
               ...} =>
            let
              val refs = List.foldl registerSpecial refs args

              val refs = List.foldl registerGenerated refs gen

              val refs = if res = 0 then refs else registerReference (ob,refs)
            in
              refs
            end
      end;

  fun registerThm (obj,th,refs) =
      let
        val (h,c) = Object.mkSequent (Thm.sequent th)

        val refs = registerSpecial (obj,refs)

        val refs = List.foldl registerDefault refs [h,c]
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

        val nextKey = 0
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

fun ppTask task =
    case task of
      ExpTask _ => Print.ppString "ExpTask"
    | ObjTask _ => Print.ppString "ObjTask"
    | ObTask _ => Print.ppString "ObTask"
    | GenTask _ => Print.ppString "GenTask"
    | CmdTask _ => Print.ppString "CmdTask";

local
  fun addKey cmds dict ob =
      let
        val MinDict {refs,keys,nextKey} = dict

        val pointless =
            not (storableObject ob) orelse
            ObjectMap.inDomain ob keys orelse
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
      if not (storableObject ob) then NONE
      else
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

  fun generateTask dict task work =
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
          ([],dict,work)
        end
      | ObjTask obj =>
        let
          val ob = ObjectProv.object obj
        in
          case useKey dict ob of
            SOME (cmds,dict) => (cmds,dict,work)
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
                ([],dict,work)
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
                ([],dict,work)
              end
        end
      | ObTask ob =>
        (case useKey dict ob of
           SOME (cmds,dict) => (cmds,dict,work)
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
             ([],dict,work)
           end)
      | GenTask (gen,res) =>
        if res = 0 then
          let
            val (ob,gen) = hdTl gen

            val (cmds,dict) = List.foldl addGen ([],dict) (rev gen)

            val (cmds,dict) = addKey cmds dict ob

            val cmds = rev cmds
          in
            (cmds,dict,work)
          end
        else
          let
            val (cmds,dict) = List.foldl addGen ([],dict) (rev gen)

            val (cmds',dict) =
                case useKey dict (List.nth (gen,res)) of
                  SOME cmds_dict => cmds_dict
                | NONE => raise Error "vanishing object"

            val cmds = List.revAppend (cmds,cmds')
          in
            (cmds,dict,work)
          end
      | CmdTask cmd =>
        let
          val cmds = [cmd]
        in
          (cmds,dict,work)
        end;
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
        let
(*OpenTheoryTrace1
          val () = Print.trace ppTask
                     "ObjectWrite.generateMinDict: task" task
*)
          val (cmds,dict,work) = generateTask dict task work
        in
          SOME (cmds,(dict,work))
        end
end;

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
(*OpenTheoryTrace1
        val () = Print.trace ObjectExport.pp
                   "ObjectWrite.toCommandStream: exp" exp
*)
        val dict = newMinDict exp
        and work = map ExpTask (ObjectExport.toList exp)

(*OpenTheoryTrace1
        val () = Print.trace (Print.ppList ppTask)
                   "ObjectWrite.toCommandStream: work" work
*)

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
