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
       objects : ObjectProv.object list};

val empty =
    let
      val size = 0

      val objects = []
    in
      Stack
        {size = size,
         objects = objects}
    end;

fun size (Stack {size = x, ...}) = x;

fun null stack = size stack = 0;

fun push stack obj =
    let
      val Stack {size,objects} = stack

      val size = size + 1

      val objects = obj :: objects
    in
      Stack
        {size = size,
         objects = objects}
    end;

fun peek stack =
    let
      val Stack {objects = objs, ...} = stack
    in
      case objs of
        [] => raise Error "ObjectStack.peek: empty"
      | obj :: _ => obj
    end;

fun pop stack =
    let
      val Stack {size, objects = objs} = stack
    in
      case objs of
        [] => raise Error "ObjectStack.pop: empty"
      | obj :: objs =>
        let
          val size = size - 1

          val stack =
              Stack
                {size = size,
                 objects = objs}
        in
          (stack,obj)
        end
    end;

fun pop2 stack =
    let
      val Stack {size, objects = objs} = stack
    in
      case objs of
        [] => raise Error "ObjectStack.pop2: empty"
      | [_] => raise Error "ObjectStack.pop2: singleton"
      | obj0 :: obj1 :: objs =>
        let
          val size = size - 2

          val stack =
              Stack
                {size = size,
                 objects = objs}
        in
          (stack,obj1,obj0)
        end
    end;

end
