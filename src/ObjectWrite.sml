(* ========================================================================= *)
(* WRITING OBJECTS TO COMMANDS                                               *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
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
        Object.Oerror => false
      | Object.Onum _ => false
      | Object.Oname _ => false
      | Object.Olist l => not (null l)
      | Object.Otype _ => true
      | Object.Oterm _ => true
      | Object.Othm _ => true
      | Object.Ocall _ => raise Bug "Article.storable: Ocall";

  fun registerTop refs ob =
      let
        val p = ObjectMap.peek refs ob
        val known = Option.isSome p
        val k = Option.getOpt (p,0)
        val refs = ObjectMap.insert refs (ob, k + 1)
      in
        (known,refs)
      end;

  fun registerDeep refs [] = refs
    | registerDeep refs (ob :: obs) =
      if not (storable ob) then registerDeep refs obs
      else
        let
          val (known,refs) = registerTop refs ob
          val obs = if known then obs else snd (Object.toCommand ob) @ obs
        in
          registerDeep refs obs
        end;

  fun register (obj,refs) =
      let
        val ob = ObjectProv.object obj
      in
        case ObjectProv.provenance obj of
          ObjectProv.Pnull => registerDeep refs [ob]
        | ObjectProv.Pcall _ => refs
        | ObjectProv.Preturn _ => refs
        | ObjectProv.Pcons _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ =
                not known orelse
                let
                  val () = Print.trace (ObjectProv.pp 1) "deja vu obj" obj
                  val k = ObjectMap.peek refs ob
                  val () = Print.trace (Print.ppOption Print.ppInt) "refs" k
                in
                  raise Bug "Article.register: Pcons"
                end
*)
          in
            refs
          end
        | ObjectProv.Pref _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ = known orelse raise Bug "Article.register: Pref"
*)
          in
            refs
          end
        | ObjectProv.Pthm _ => registerDeep refs [ob]
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
      handle Error err =>
        raise Bug ("Article.newMinDict: " ^ err);
end;

local
  fun isKey (MinDict {keys,...}) ob = ObjectMap.inDomain ob keys;

  fun addKey dict cmds ob =
      let
        val MinDict {nextKey,refs,keys} = dict
(*OpenTheoryDebug
        val _ = not (ObjectMap.inDomain ob keys) orelse
                raise Bug "Article.addKey"
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
      end;

  fun useKey dict cmds ob =
      let
        val MinDict {nextKey,refs,keys} = dict
      in
        case ObjectMap.peek keys ob of
          NONE => raise Bug "Article.useKey"
        | SOME key =>
          let
            val cmds = Command.Num key :: cmds
          in
            case ObjectMap.peek refs ob of
              NONE => raise Bug "generate"
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
      end;

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
  fun generateMinDict stacksize prevCall dict obj =
      let
        val ob = ObjectProv.object obj
        and call = ObjectProv.call obj
      in
        case ObjectProv.provenance obj of
          ObjectProv.Pnull =>
          let
            val cmds = ObjectProv.alignCalls {prevCall = prevCall, call = call}
            val (dict,cmds) = generateDeep (ob,(dict,cmds))
            val stacksize = stacksize + (if Option.isSome call then 0 else 1)
          in
            (stacksize,call,dict,cmds)
          end
        | ObjectProv.Pcall _ =>
          let
            val cmds = ObjectProv.alignCalls {prevCall = prevCall, call = call}

            val n =
                case ob of
                  Object.Ocall n => n
                | _ => raise Error "Pcall: bad call"

            val cmds = Command.Call :: Command.Name n :: cmds

            val call = SOME obj
          in
            (stacksize,call,dict,cmds)
          end
        | ObjectProv.Preturn robj =>
          let
            val rcall = ObjectProv.call robj

            val cmds = ObjectProv.alignCalls {prevCall = prevCall, call = rcall}

            val rcobj =
                case rcall of
                  SOME rcobj => rcobj
                | NONE => raise Error "Preturn: no call"

            val n =
                case ObjectProv.object rcobj of
                  Object.Ocall n => n
                | _ => raise Error "Preturn: bad call"

            val cmds = Command.Return :: Command.Name n :: cmds
          in
            (stacksize,call,dict,cmds)
          end
        | ObjectProv.Pcons _ =>
          let
            val cmds = ObjectProv.alignCalls {prevCall = prevCall, call = call}

            val cmds = Command.Cons :: cmds

            val (dict,cmds) = addKey dict cmds ob

            val stacksize = stacksize - (if Option.isSome call then 0 else 1)
          in
            (stacksize,call,dict,cmds)
          end
        | ObjectProv.Pref _ =>
          let
            val cmds = ObjectProv.alignCalls {prevCall = prevCall, call = call}

            val (dict,cmds) = useKey dict cmds ob

            val stacksize = stacksize + (if Option.isSome call then 0 else 1)
          in
            (stacksize,call,dict,cmds)
          end
        | ObjectProv.Pthm _ =>
          let
            val cmds = ObjectProv.alignCalls {prevCall = prevCall, call = call}

            val (dict,cmds) = generateDeep (ob,(dict,cmds))

            val stacksize = stacksize + (if Option.isSome call then 0 else 1)
          in
            (stacksize,call,dict,cmds)
          end
      end
      handle Error err =>
        raise Bug ("ObjectWrite.generateMinDict: " ^ err);
end;

(* ------------------------------------------------------------------------- *)
(* Writing objects to a stream of commands.                                  *)
(* ------------------------------------------------------------------------- *)

fun toCommandStream saved =
    let
      fun gen obj (stacksize,call,dict) =
          let
            val (stacksize,call,dict,cmds) =
                generateMinDict stacksize call dict obj

            val cmds =
                if not (ObjectProvSet.member obj saved) then cmds
                else Command.Save :: cmds
          in
            (cmds,(stacksize,call,dict))
          end

      fun finish (stacksize,call,dict) =
          let
(*OpenTheoryDebug
            val _ = nullMinDict dict orelse raise Error "nonempty dict"
*)
            val cmds = ObjectProv.alignCalls {prevCall = call, call = NONE}
            val cmds = funpow stacksize (cons Command.Pop) cmds
          in
            if null cmds then Stream.Nil else Stream.singleton cmds
          end

      val objs = ObjectProvSet.ancestorSet saved

      val stacksize = 0
      val call = NONE
      val dict = newMinDict objs

      val strm = ObjectProvSet.toStream objs
      val strm = Stream.maps gen finish (stacksize,call,dict) strm
    in
      revConcat strm
    end
    handle Error err =>
      raise Bug ("ObjectWrite.toCommandStream: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Writing objects to text files.                                            *)
(* ------------------------------------------------------------------------- *)

fun toTextFile {filename} objs =
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
    handle Error err => raise Error ("ObjectWrite.toTextFile: " ^ err);

end
