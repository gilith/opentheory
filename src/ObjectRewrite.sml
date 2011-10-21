(* ========================================================================= *)
(* REWRITING OPENTHEORY OBJECTS                                              *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectRewrite :> ObjectRewrite =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Bottom-up object rewrites: return NONE for unchanged.                     *)
(* ------------------------------------------------------------------------- *)

datatype rewrite =
    Rewrite of
      {apply : ObjectProv.object' -> ObjectProv.object option,
       seen : ObjectProv.object option IntMap.map};

fun new apply =
    let
      val seen = IntMap.new ()
    in
      Rewrite
        {apply = apply,
         seen = seen}
    end;

val id = new (K NONE);

(* ------------------------------------------------------------------------- *)
(* Applying rewrites.                                                        *)
(* ------------------------------------------------------------------------- *)

local
  val savable = true;

  fun rewriteObj apply =
      let
        fun preDescent obj seen =
            let
              val i = ObjectProv.id obj
            in
              case IntMap.peek seen i of
                NONE => {descend = true, result = (NONE,seen)}
              | SOME obj' => {descend = false, result = (obj',seen)}
            end

        fun postDescent obj0 obj1 seen =
            let
              val i = ObjectProv.id obj0

              val (unchanged,obj2) =
                  case obj1 of
                    NONE => (true,obj0)
                  | SOME obj => (false,obj)

              val (unchanged,obj3) =
                  case apply (ObjectProv.dest obj2) of
                    NONE => (unchanged,obj2)
                  | SOME obj => (false,obj)

              val obj3' = if unchanged then NONE else SOME obj3

              val seen = IntMap.insert seen (i,obj3')
            in
              (obj3',seen)
            end
      in
        ObjectProv.maps
          {preDescent = preDescent,
           postDescent = postDescent,
           savable = savable}
      end;
in
  fun sharingRewriteObject obj rewr =
      let
        val Rewrite {apply,seen} = rewr

        val (obj',seen) = rewriteObj apply obj seen

        val rewr = Rewrite {apply = apply, seen = seen}
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
