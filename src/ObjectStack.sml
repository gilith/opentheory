(* ========================================================================= *)
(* OBJECT STACKS                                                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
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
        val (f,_) = ObjectProv.destCall obj
      in
        (stack,f)
      end;

fun topCall (Stack {call,...}) =
    case call of
      NONE => NONE
    | SOME (obj,_) => SOME obj;

fun callStack (Stack {call,...}) =
    case call of
      NONE => []
    | SOME (obj,stack) => obj :: callStack stack;

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
            | SOME i => ObjectProv.equalId i obj
      in
        if aligned then (stack,cmds)
        else
          let
(*OpenTheoryDebug
            val _ = Object.isOcall (ObjectProv.object obj) orelse
                    raise Bug "ObjectStack.addAlignCalls: bad call"
*)

            val cmds =
                if frameSize stack > 0 then cmds
                else Command.Error :: cmds

            val (stack,n) = popCall stack

            val cmds =
                Command.Pop ::
                Command.Return ::
                Command.Name n ::
                cmds
          in
            addAlignCalls call stack cmds
          end
      end;

fun alignCalls {call} stack = addAlignCalls call stack [];

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
