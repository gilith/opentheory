(* ========================================================================= *)
(* WRITING OBJECTS TO COMMANDS                                               *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectWrite :> ObjectWrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Minimal dictionaries.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype minDict =
    MinDict of
      {refs : int ObjectDataMap.map,
       keys : int ObjectDataMap.map,
       nextKey : int};

local
  fun registerGenerated (ob,refs) =
      let
(*OpenTheoryTrace5
        val () = Print.trace ObjectData.pp
                   "ObjectWrite.registerGenerated: ob" ob
*)
      in
        if ObjectDataMap.inDomain ob refs then refs
        else ObjectDataMap.insert refs (ob,0)
      end;

  fun registerReference (ob,refs) =
      let
(*OpenTheoryTrace5
        val () = Print.trace ObjectData.pp
                   "ObjectWrite.registerReference: ob" ob
*)
        val k = Option.getOpt (ObjectDataMap.peek refs ob, 0)
      in
        ObjectDataMap.insert refs (ob, k + 1)
      end;

  fun registerDefault (ob,refs) =
      if not (ObjectData.inDictionary ob) then refs
      else if ObjectDataMap.inDomain ob refs then registerReference (ob,refs)
      else registerDefault' (ob,refs)

  and registerDefault' (ob,refs) =
      let
(*OpenTheoryTrace5
        val () = Print.trace ObjectData.pp
                   "ObjectWrite.registerDefault': ob" ob
*)
        val (_,args) = ObjectData.command ob

        val refs = List.foldl registerDefault refs args
      in
        registerGenerated (ob,refs)
      end;

  fun registerSpecial (obj,refs) =
      let
(*OpenTheoryTrace5
        val () = Print.trace Object.pp
                   "ObjectWrite.registerSpecial: obj" obj
*)
        val ob = Object.data obj
      in
        if not (ObjectData.inDictionary ob) then refs
        else if ObjectDataMap.inDomain ob refs then registerReference (ob,refs)
        else
          case Object.provenance obj of
            Object.Default => registerDefault' (ob,refs)
          | Object.Special
              {arguments = args,
               generated = gen,
               result = res,
               ...} =>
            let
(*OpenTheoryTrace5
              val () = Print.trace (Print.ppList Object.pp)
                        "ObjectWrite.registerSpecial: args" args
*)
              val refs = List.foldl registerSpecial refs args

              val refs = List.foldl registerGenerated refs gen

              val refs = if res = 0 then refs else registerReference (ob,refs)
            in
              refs
            end
      end;

  fun registerThm (th,refs) =
      let
        val ObjectThm.Thm {proof,hyp,concl} = ObjectThm.dest th

        val refs = registerSpecial (proof,refs)

        val refs = registerSpecial (hyp,refs)

        val refs = registerSpecial (concl,refs)
      in
        refs
      end;
in
  fun newMinDict exp =
      let
        val refs = ObjectDataMap.new ()

        val refs = ObjectExport.foldl registerThm refs exp

        val refs = ObjectDataMap.filter (fn (_,n) => n >= 1) refs

        val keys = ObjectDataMap.new ()

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
    ExpTask of ObjectThm.thm
  | ObjTask of Object.object
  | ObTask of ObjectData.data
  | GenTask of ObjectData.data list * int
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
            not (ObjectData.inDictionary ob) orelse
            ObjectDataMap.inDomain ob keys orelse
            not (ObjectDataMap.inDomain ob refs)
      in
        if pointless then (cmds,dict)
        else
          let
            val key = nextKey

            val keys = ObjectDataMap.insert keys (ob,key)

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
      if not (ObjectData.inDictionary ob) then NONE
      else
        let
          val MinDict {refs,keys,nextKey} = dict
        in
          case ObjectDataMap.peek keys ob of
            NONE => NONE
          | SOME key =>
            case ObjectDataMap.peek refs ob of
              NONE => raise Error "no such object"
            | SOME n =>
              if n = 1 then
                let
                  val refs = ObjectDataMap.delete refs ob

                  val keys = ObjectDataMap.delete keys ob

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
                  val refs = ObjectDataMap.insert refs (ob, n - 1)

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
        ExpTask th =>
        let
          val ObjectThm.Thm {proof,hyp,concl} = ObjectThm.dest th

          val work =
              ObjTask proof ::
              ObjTask hyp ::
              ObjTask concl ::
              CmdTask Command.Thm ::
              work
        in
          ([],dict,work)
        end
      | ObjTask obj =>
        let
          val ob = Object.data obj
        in
          case useKey dict ob of
            SOME (cmds,dict) => (cmds,dict,work)
          | NONE =>
            case Object.provenance obj of
              Object.Default =>
              let
                val (cmd,args) = ObjectData.command ob
                and gen = [ob]
                and res = 0

                val work =
                    List.map ObTask args @
                    CmdTask cmd ::
                    GenTask (gen,res) ::
                    work
              in
                ([],dict,work)
              end
            | Object.Special
                {command = cmd,
                 arguments = args,
                 definitions = _,
                 generated = gen,
                 result = res} =>
              let
                val work =
                    List.map ObjTask args @
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
             val (cmd,args) = ObjectData.command ob
             and gen = [ob]
             and res = 0

             val work =
                 List.map ObTask args @
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

            val (cmds,dict) = List.foldl addGen ([],dict) (List.rev gen)

            val (cmds,dict) = addKey cmds dict ob

            val cmds = List.rev cmds
          in
            (cmds,dict,work)
          end
        else
          let
            val (cmds,dict) = List.foldl addGen ([],dict) (List.rev gen)

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

          val _ = ObjectDataMap.null refs orelse
                  raise Error "nonempty refs"

          val _ = ObjectDataMap.null keys orelse
                  raise Error "nonempty keys"
*)
        in
          NONE
        end
      | task :: work =>
        let
(*OpenTheoryTrace5
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
  fun toCommandStream vers exp =
      let
(*OpenTheoryTrace4
        val () = Print.trace ObjectExport.pp
                   "ObjectWrite.toCommandStream: exp" exp
*)
        val dict = newMinDict exp
        and work = List.map ExpTask (ObjectExport.toList exp)

(*OpenTheoryTrace4
        val () = Print.trace (Print.ppList ppTask)
                   "ObjectWrite.toCommandStream: work" work
*)
        val strm = generateStream (dict,work) ()

        val strm =
            if ArticleVersion.equal vers ArticleVersion.readDefault then strm
            else
              let
                val cmds =
                    [Command.Num (ArticleVersion.toInt vers),
                     Command.Version]
              in
                Stream.cons cmds (K strm)
              end

        val strm = Stream.listConcat strm

(*OpenTheoryDebug
        val strm =
            let
              fun check cmd =
                  if ArticleVersion.supported vers cmd then cmd
                  else
                    let
                      val bug =
                          "command " ^ Command.toString cmd ^
                          " is not supported in article version " ^
                          ArticleVersion.toString vers
                    in
                      raise Bug bug
                    end
            in
              Stream.map check strm
            end
*)
      in
        strm
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Bug ("ObjectWrite.toCommandStream: " ^ err);
*)
end;

(* ------------------------------------------------------------------------- *)
(* Writing objects to text files.                                            *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {version,export,filename} =
    let
      val commands = toCommandStream version export

      val lines = Stream.map (fn c => Command.toString c ^ "\n") commands
    in
      Stream.toTextFile {filename = filename} lines
    end
(*OpenTheoryDebug
    handle Error err =>
      raise Bug ("ObjectWrite.toTextFile: " ^ err);
*)

end
