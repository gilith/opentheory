(* ========================================================================= *)
(* OBJECT STACKS                                                             *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure ObjectStack :> ObjectStack =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of object stacks.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype stack =
    Stack of
      {size : int,
       objects : ObjectProv.object list,
       thms : ObjectThms.thms list,
       simulation : ObjectSimulation.simulation,
       call : (ObjectProv.object * stack) option};

val empty =
    let
      val size = 0

      val objects = []

      val thms = []

      val sim = ObjectSimulation.empty

      val call = NONE
    in
      Stack
        {size = size,
         objects = objects,
         thms = thms,
         simulation = sim,
         call = call}
    end;

fun size (Stack {size = x, ...}) = x;

fun null stack = size stack = 0;

fun frameSize (Stack {size = n, call, ...}) =
    n - (case call of NONE => 0 | SOME (_,stack) => size stack + 1);

fun objects (Stack {objects = x, ...}) = x;

fun topThms l =
    case l of
      [] => ObjectThms.empty
    | ths :: _ => ths;

fun thms (Stack {thms = t, ...}) = topThms t;

fun symbol stack = ObjectThms.symbol (thms stack);

fun push stack obj =
    let
      val Stack {size,objects,thms,simulation,call} = stack

      val size = size + 1

      val objects = obj :: objects

      val ths = ObjectThms.add (topThms thms) obj

      val thms = ths :: thms

      val call =
          if not (Object.isOcall (ObjectProv.object obj)) then call
          else SOME (obj,stack)
    in
      Stack
        {size = size,
         objects = objects,
         thms = thms,
         simulation = simulation,
         call = call}
    end;

fun pop stack n =
    if n > frameSize stack then raise Error "ObjectStack.pop: empty frame"
    else
      let
        val Stack {size,objects,thms,simulation,call} = stack

        val size = size - n

        val objects = List.drop (objects,n)

        val thms = List.drop (thms,n)
      in
        Stack
          {size = size,
           objects = objects,
           thms = thms,
           simulation = simulation,
           call = call}
      end;

fun peek stack n =
    let
      val Stack {size,objects,...} = stack
    in
      if n >= size then raise Error "ObjectStack.peek: bad index"
      else List.nth (objects,n)
    end;

fun pop1 stack =
    (pop stack 1,
     peek stack 0);

fun pop2 stack =
    (pop stack 2,
     peek stack 1,
     peek stack 0);

fun popCall (Stack {call,...}) =
    case call of
      NONE => raise Error "ObjectStack.popCall: top level"
    | SOME (obj,stack) =>
      let
        val ObjectProv.Object {object = ob, ...} = obj
      in
        (stack, Object.destOcall ob)
      end;

fun topCall (Stack {call,...}) =
    case call of
      NONE => NONE
    | SOME (obj,_) => SOME obj;

fun callStack stack =
    case topCall stack of
      NONE => []
    | SOME obj => obj :: ObjectProv.callStack obj;

fun search (Stack {thms,...}) seq = ObjectThms.search (topThms thms) seq;

(* ------------------------------------------------------------------------- *)
(* Generating commands to keep the call stack consistent.                    *)
(* ------------------------------------------------------------------------- *)

fun addAlignCalls call stack cmds =
    case topCall stack of
      NONE =>
      let
        val _ = not (Option.isSome call) orelse
                raise Bug "ObjectStack.addAlignCalls: top level to nested"
      in
        (stack,cmds)
      end
    | SOME obj =>
      let
        val aligned =
            case call of
              NONE => false
            | SOME obj' => ObjectProv.id obj = ObjectProv.id obj'
      in
        if aligned then (stack,cmds)
        else
          let
(*OpenTheoryDebug
*)
            val _ = Object.isOcall (ObjectProv.object obj) orelse
                    raise Bug "ObjectStack.addAlignCalls: bad call"

            val (stack,n) = popCall stack

            val cmds =
                Command.Pop ::
                Command.Return ::
                Command.Name n ::
                Command.Error ::
                cmds
          in
            addAlignCalls call stack cmds
          end
      end;

fun alignCalls {call} stack = addAlignCalls call stack [];

(* ------------------------------------------------------------------------- *)
(* Building objects using data on a stack.                                   *)
(* ------------------------------------------------------------------------- *)

fun buildObject {savable} stack =
    let
      val call = if savable then topCall stack else NONE

      fun mkObj ob prov =
          let
            val prov = if savable then prov else ObjectProv.Pnull
          in
            ObjectProv.mk
              {object = ob,
               provenance = prov,
               call = call}
          end

      fun mkNullObj ob = mkObj ob ObjectProv.Pnull

      fun mkConsObj ob objH objT =
          let
            val isTh =
                ObjectProv.containsThms objH orelse
                ObjectProv.containsThms objT

            val prov =
                if isTh then ObjectProv.Pcons (objH,objT)
                else ObjectProv.Pnull
          in
            mkObj ob prov
          end

      fun mkThmObj ob th =
          let
            val objS =
                case search stack (Thm.sequent th) of
                  SOME (_,objS) => objS
                | NONE =>
                  raise Bug ("ObjectRead.buildObject: couldn't find theorem " ^
                             "on stack:\n" ^ Thm.toString th)

            val prov = ObjectProv.Pthm (ObjectProv.Istack objS)
          in
            mkObj ob prov
          end

      fun build ob =
          case ob of
            Object.Olist (obH :: obT) =>
            let
              val objH = build obH

              val objT = build (Object.Olist obT)
            in
              mkConsObj ob objH objT
            end
          | Object.Othm th => mkThmObj ob th
          | Object.Ocall _ =>
            raise Bug "ObjectRead.buildObject: cannot build Ocall obj"
          | _ => mkNullObj ob
    in
      build
    end;

(* ------------------------------------------------------------------------- *)
(* The stack is also used to keep track of simulated theorems.               *)
(* ------------------------------------------------------------------------- *)

fun symbolSimulation (Stack {simulation = sim, ...}) =
    ObjectSimulation.symbol sim;

fun addSimulation stack ths_obj =
    let
      val Stack {size, objects, thms, simulation = sim, call} = stack

      val sim = ObjectSimulation.add sim ths_obj
    in
      Stack
        {size = size,
         objects = objects,
         thms = thms,
         simulation = sim,
         call = call}
    end;

fun searchSimulation (Stack {simulation = sim, ...}) seq =
    ObjectSimulation.search sim seq;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun topCallToString stack =
    case topCall stack of
      NONE => "top level"
    | SOME obj =>
      let
        val (f,_) = ObjectProv.destCall obj
      in
        Name.toString f
      end;

end
