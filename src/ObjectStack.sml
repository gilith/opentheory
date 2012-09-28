(* ========================================================================= *)
(* OBJECT STACKS                                                             *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
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
       objects : Object.object list};

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

fun objects (Stack {objects = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Peeking.                                                                  *)
(* ------------------------------------------------------------------------- *)

fun peek stack =
    case objects stack of
      [] => raise Error "ObjectStack.peek: empty"
    | obj :: _ => obj;

(* ------------------------------------------------------------------------- *)
(* Pushing.                                                                  *)
(* ------------------------------------------------------------------------- *)

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

fun push2 stack obj0 obj1 =
    let
      val Stack {size,objects} = stack

      val size = size + 2

      val objects = obj1 :: obj0 :: objects
    in
      Stack
        {size = size,
         objects = objects}
    end;

fun push5 stack obj0 obj1 obj2 obj3 obj4 =
    let
      val Stack {size,objects} = stack

      val size = size + 5

      val objects = obj4 :: obj3 :: obj2 :: obj1 :: obj0 :: objects
    in
      Stack
        {size = size,
         objects = objects}
    end;

(* ------------------------------------------------------------------------- *)
(* Popping.                                                                  *)
(* ------------------------------------------------------------------------- *)

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
      | obj1 :: obj0 :: objs =>
        let
          val size = size - 2

          val stack =
              Stack
                {size = size,
                 objects = objs}
        in
          (stack,obj0,obj1)
        end
    end;

fun pop3 stack =
    let
      val Stack {size, objects = objs} = stack
    in
      case objs of
        [] => raise Error "ObjectStack.pop3: empty"
      | [_] => raise Error "ObjectStack.pop3: singleton"
      | [_,_] => raise Error "ObjectStack.pop3: doubleton"
      | obj2 :: obj1 :: obj0 :: objs =>
        let
          val size = size - 3

          val stack =
              Stack
                {size = size,
                 objects = objs}
        in
          (stack,obj0,obj1,obj2)
        end
    end;

fun pop5 stack =
    let
      val Stack {size, objects = objs} = stack
    in
      case objs of
        [] => raise Error "ObjectStack.pop5: empty"
      | [_] => raise Error "ObjectStack.pop5: singleton"
      | [_,_] => raise Error "ObjectStack.pop5: doubleton"
      | [_,_,_] => raise Error "ObjectStack.pop5: tripleton"
      | [_,_,_,_] => raise Error "ObjectStack.pop5: quadraton"
      | obj4 :: obj3 :: obj2 :: obj1 :: obj0 :: objs =>
        let
          val size = size - 5

          val stack =
              Stack
                {size = size,
                 objects = objs}
        in
          (stack,obj0,obj1,obj2,obj3,obj4)
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap objects (Print.ppList Object.pp);

end
