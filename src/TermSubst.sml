(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TERMS                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure TermSubst :> TermSubst =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Capture-avoiding substitutions of type and term variables                 *)
(* ------------------------------------------------------------------------- *)

datatype subst = Subst of TypeSubst.subst * Term.term VarMap.map;

val empty = Subst (TypeSubst.empty, VarMap.new ());

fun addType n_ty (Subst (sty,stm)) = Subst (TypeSubst.add n_ty sty, stm);

fun add v_tm (Subst (sty,stm)) =
    let
      val (v,tm) = v_tm
      val _ = Type.equal (Var.typeOf v) (Term.typeOf tm) orelse
              raise Error "TermSubst.add: bad type"
    in
      Subst (sty, VarMap.insert stm v_tm)
    end;

fun singletonType n_ty = addType n_ty empty;

fun singleton v_tm = add v_tm empty;

fun addListType l sub = foldl (fn (n_ty,s) => addType n_ty s) sub l;

fun addList l sub = foldl (fn (v_tm,s) => add v_tm s) sub l;

fun fromListType l = addListType l empty;

fun fromList l = addList l empty;

fun null (Subst (sty,stm)) = TypeSubst.null sty andalso VarMap.null stm;

fun peekType (Subst (sty,_)) n = TypeSubst.peek sty n;

fun peek (Subst (_,stm)) v = VarMap.peek stm v;

fun toListType (Subst (sty,_)) = TypeSubst.toList sty;

fun toList (Subst (_,stm)) = VarMap.foldr (fn (v,tm,l) => (v,tm) :: l) [] stm;

(* ------------------------------------------------------------------------- *)
(* Normalization removes identity substitutions v |-> v.                     *)
(* ------------------------------------------------------------------------- *)

fun norm (Subst (sty,stm)) =
    let
      val sty = TypeSubst.norm sty

      val stm =
          let
            fun p (Var.Var (n,ty)) (Term.Var (Var.Var (n',ty'))) =
                Name.equal n n' andalso
                let
                  val ty = Option.getOpt (TypeSubst.subst sty ty, ty)
                  and ty' = Option.getOpt (TypeSubst.subst sty ty', ty')
                in
                  Type.equal ty ty'
                end
              | p _ _ = false

            fun f (v,tm,z) =
                if p v (Term.dest tm) then z else VarMap.insert z (v,tm)
          in
            VarMap.foldl f (VarMap.new ()) stm
          end
    in
      Subst (sty,stm)
    end;

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

fun substType (Subst (sty,_)) = TypeSubst.subst sty;

fun rawSharingSubst sty stm seen tm =

fun sharingSubst sub =
    if null sub then K NONE
    else
      let
        val Subst (sty,stm) = sub

        val stm =
            let
              fun f (v,tm,z) =
                  let
                    val v = Option.getOpt (Var.subst sty v, v)
                    val tm = Option.getOpt (typeSubst sty tm, tm)
                    val fv = Term.freeVars tm
                  in
                    VarMap.insert z (v,(tm,fv))
                  end
            in
              VarMap.foldl f (VarMap.new ()) stm
            end
      in
        rawSubst sty stm
      end;

local
  exception Capture of int;

  fun bvMin newDepth (v,b) =
      case VarMap.peek newDepth v of
        NONE => b
      | SOME d =>
        let
          val r = case b of NONE => true | SOME m => d < m
        in
          if r then SOME d else b
        end;

  fun substGen sty stm bv tm =
      case Term.dest tm of
        Term.Const (n,ty) =>
        (case TypeSubst.subst sty ty of
           SOME ty' => SOME (Term.mkConst (n,ty'))
         | NONE => NONE)
      | Term.Var v =>
        (case VarMap.peek (#oldToNew bv) v of
           SOME v' => if Var.equal v v' then NONE else SOME (Term.mkVar v')
         | NONE =>
           let
             val (changed,v) =
                 case Var.subst sty v of
                   SOME v => (true,v)
                 | NONE => (false,v)

             val (tm,fv) =
                 case VarMap.peek stm v of
                   SOME tm_fv => tm_fv
                 | NONE =>
                   let
                     val 
                   (if v == v' then tm else Term.mkVar v', VarSet.singleton v')
           in
             if #depth bv = 0 then tm
             else
               case VarSet.foldl (bvMin (#newDepth bv)) NONE fv of
                 NONE => tm
               | SOME d => raise Capture d
           end)
      | Term.Comb (a,b) =>
        let
          val a' = substGen sty stm bv a
          val b' = substGen sty stm bv b
        in
          if a' == a andalso b' == b then tm else Term.mkComb (a',b')
        end
      | Term.Abs (v,body) =>
        let
          val {depth,oldToNew,newDepth} = bv

          fun rename v' =
              if Option.isSome (VarMap.peek newDepth v') then
                rename (Var.variant v')
              else
                let
                  val oldToNew = VarMap.insert oldToNew (v,v')
                  val newDepth = VarMap.insert newDepth (v',depth)
                  val depth = depth + 1
                  val bv = {depth = depth, oldToNew = oldToNew,
                            newDepth = newDepth}
                  val body = substGen sty stm bv body
                in
                  (v',body)
                end
                handle c as Capture d =>
                  if d = depth then rename (Var.variant v') else raise c

          val (v',body') = rename (typeSubstVar sty v)
        in
          if v == v' andalso body == body' then tm else Term.mkAbs (v',body')
        end;

  fun rawSubst sty stm =
      let
        val info =
            {depth = 0,
             oldToNew = VarMap.new (),
             newDepth = VarMap.new ()}
      in
        substGen sty stm info
      end;

  fun typeSubst sty =
      let
        val stm = VarMap.new ()
      in
        rawSubst sty stm
      end;
in
  fun subst sub =
      if null sub then K NONE
      else
        let
          val Subst (sty,stm) = sub

          val stm =
              let
                fun f (v,tm,z) =
                    let
                      val v = Option.getOpt (Var.subst sty v, v)
                      val tm = Option.getOpt (typeSubst sty tm, tm)
                      val fv = Term.freeVars tm
                    in
                      VarMap.insert z (v,(tm,fv))
                    end
              in
                VarMap.foldl f (VarMap.new ()) stm
              end
        in
          rawSubst sty stm
        end;
end;

end
