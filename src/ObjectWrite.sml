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
      | Object.Oint _ => false
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
                  Object.Ocall n => n
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
        | ObjectProv.Preturn objR =>
          let
            val (stack,objR') = ObjectStack.pop1 stack

            val (stack,n) = ObjectStack.popCall stack

            val stack = ObjectStack.push stack obj

            val cmds = Command.Return :: Command.Name n :: cmds

(*OpenTheoryDebug
            val _ = ObjectProv.id objR = ObjectProv.id objR' orelse
                    raise Error "Preturn: wrong return value"
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
        | ObjectProv.Pthm _ =>
          let
            val (dict,cmds) = generateDeep (ob,(dict,cmds))

            val stack = ObjectStack.push stack obj
          in
            (stack,dict,cmds)
          end
      end
      handle Error err =>
        raise Bug ("ObjectWrite.generateMinDict: " ^ err);
end;

(* ------------------------------------------------------------------------- *)
(* Writing objects to a stream of commands.                                  *)
(* ------------------------------------------------------------------------- *)

fun startingCall obj =
    let
      val call = ObjectProv.call obj
    in
      case ObjectProv.provenance obj of
        ObjectProv.Preturn robj => ObjectProv.call robj
      | _ => call
    end;

fun alignStartingCall stack obj =
    let
      val call = startingCall obj
    in
      ObjectStack.alignCalls {call = call} stack
    end;

fun toCommandStream saved =
    let
      val objs = ObjectProvSet.ancestorSet saved

      val stackUses = ObjectProvSet.stackUses objs

      fun gen obj (stack,dict) =
          let
            val (stack,cmds) = alignStartingCall stack obj

            val (stack,dict,cmds) =
                generateMinDict stack dict cmds obj

            val cmds =
                if not (ObjectProvSet.member obj saved) then cmds
                else Command.Save :: cmds

            val (cmds,stack) =
                if Object.isOcall (ObjectProv.object obj) orelse
                   ObjectProvSet.member obj stackUses
                then (cmds,stack)
                else (Command.Pop :: cmds, ObjectStack.pop stack 1)
          in
            (cmds,(stack,dict))
          end

      fun finish (stack,dict) =
          let
(*OpenTheoryDebug
            val _ = nullMinDict dict orelse raise Error "nonempty dict"
*)
            val (stack,cmds) = ObjectStack.alignCalls {call = NONE} stack
            val cmds = funpow (ObjectStack.size stack) (cons Command.Pop) cmds
          in
            if null cmds then Stream.Nil else Stream.singleton cmds
          end

      val stack = ObjectStack.empty
      val dict = newMinDict objs

      val strm = ObjectProvSet.toStream objs
      val strm = Stream.maps gen finish (stack,dict) strm
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
