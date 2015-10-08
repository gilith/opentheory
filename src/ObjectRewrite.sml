(* ========================================================================= *)
(* REWRITING OPENTHEORY OBJECTS                                              *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectRewrite :> ObjectRewrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Bottom-up object rewrites: return NONE for unchanged.                     *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    Rewrite of
      {tmRewr : TermRewrite.rewrite,
       apply : Object.object' -> Object.object option,
       seen : Object.object option IntMap.map};

fun new tmRewr apply =
    let
      val seen = IntMap.new ()
    in
      Rewrite
        {tmRewr = tmRewr,
         apply = apply,
         seen = seen}
    end;

val id = new TermRewrite.id (K NONE);

local
  fun rewriteObj apply =
      let
        fun preDescent obj (tmRewr,seen) =
            let
              val i = Object.id obj
            in
              case IntMap.peek seen i of
                SOME obj' =>
                {descend = false, result = (obj',(tmRewr,seen))}
              | NONE =>
                let
                  val Object.Object' {data,provenance} = Object.dest obj
                in
                  case provenance of
                    Object.Special _ =>
                    {descend = true, result = (NONE,(tmRewr,seen))}
                  | Object.Default =>
                    let
                      val (data',tmRewr) =
                          ObjectData.sharingRewrite data tmRewr

                      val obj' = Option.map Object.mkUnsavable data'

                      val seen = IntMap.insert seen (i,obj')
                    in
                      {descend = false, result = (obj',(tmRewr,seen))}
                    end
                end
            end

        fun postDescent obj0 obj1' (tmRewr,seen) =
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
              (obj2',(tmRewr,seen))
            end

        val savable = true
      in
        Object.maps
          {preDescent = preDescent,
           postDescent = postDescent,
           savable = savable}
      end;
in
  fun sharingRewrite obj rewr =
      let
        val Rewrite {tmRewr,apply,seen} = rewr

        val (obj',(tmRewr,seen)) = rewriteObj apply obj (tmRewr,seen)

        val rewr = Rewrite {tmRewr = tmRewr, apply = apply, seen = seen}
      in
        (obj',rewr)
      end;
end;

fun rewrite rewr obj =
    let
      val (obj',_) = sharingRewrite obj rewr
    in
      obj'
    end;

end
