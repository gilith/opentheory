(* ========================================================================= *)
(* REWRITING OPENTHEORY OBJECTS                                              *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectRewrite :> ObjectRewrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of parameters for rewriting objects.                               *)
(* ------------------------------------------------------------------------- *)

type parameters =
     {apply : Object.object' -> Object.object option,
      savable : bool};

(* ------------------------------------------------------------------------- *)
(* Bottom-up object rewrites: return NONE for unchanged.                     *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    Rewrite of
      {parameters : parameters,
       seen : Object.object option IntMap.map};

fun new parameters =
    let
      val seen = IntMap.new ()
    in
      Rewrite
        {parameters = parameters,
         seen = seen}
    end;

val id =
    let
      val apply = K NONE
      and savable = true
    in
      new {apply = apply, savable = savable}
    end;

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

local
  fun rewriteObj apply savable =
      let
        fun preDescent obj seen =
            let
              val i = Object.id obj
            in
              case IntMap.peek seen i of
                NONE => {descend = true, result = (NONE,seen)}
              | SOME obj' => {descend = false, result = (obj',seen)}
            end

        fun postDescent obj0 obj1' seen =
            let
              val i = Object.id obj0

              val (unchanged,obj1) =
                  case obj1' of
                    NONE => (true,obj0)
                  | SOME obj => (false,obj)

              val (unchanged,obj2) =
                  case apply (Object.dest obj1) of
                    NONE => (unchanged,obj1)
                  | SOME obj => (false,obj)

              val obj2' = if unchanged then NONE else SOME obj2

              val seen = IntMap.insert seen (i,obj2')
            in
              (obj2',seen)
            end
      in
        Object.maps
          {preDescent = preDescent,
           postDescent = postDescent,
           savable = savable}
      end;
in
  fun sharingRewriteObject obj rewr =
      let
        val Rewrite {parameters,seen} = rewr

        val {apply,savable} = parameters

        val (obj',seen) = rewriteObj apply savable obj seen

        val rewr = Rewrite {parameters = parameters, seen = seen}
      in
        (obj',rewr)
      end;
end;

fun rewriteObject rewr obj =
    let
      val (obj',_) = sharingRewriteObject obj rewr
    in
      obj'
    end;

end
