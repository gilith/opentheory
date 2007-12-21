(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TERMS                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure TermSubst :> TermSubst =
struct

open Useful;

structure N = Name;
structure NS = NameSet;
structure Ty = Type;
structure TyU = TypeSubst;
structure V = Var;
structure VS = VarSet;
structure VM = VarMap;
structure T = Term;

infixr ==

val op== = Portable.pointerEqual;

(* ------------------------------------------------------------------------- *)
(* Capture-avoiding substitutions of type and term variables                 *)
(* ------------------------------------------------------------------------- *)

datatype subst = Subst of TyU.subst * T.term VM.map;

val empty = Subst (TyU.empty, VM.new ());

fun addType n_ty (Subst (sty,stm)) = Subst (TyU.add n_ty sty, stm);

fun add v_tm (Subst (sty,stm)) =
    let
      val ((_,ty),tm) = v_tm
      val _ = Ty.equal (T.typeOf tm) ty orelse
              raise Error "TermSubst.add: bad type"
    in
      Subst (sty, VM.insert stm v_tm)
    end;

fun singletonType n_ty = addType n_ty empty;

fun singleton v_tm = add v_tm empty;

fun addListType l sub = foldl (fn (n_ty,s) => addType n_ty s) sub l;

fun addList l sub = foldl (fn (v_tm,s) => add v_tm s) sub l;

fun fromListType l = addListType l empty;

fun fromList l = addList l empty;

fun null (Subst (sty,stm)) = TyU.null sty andalso VM.null stm;

fun peekType (Subst (sty,_)) n = TyU.peek sty n;

fun peek (Subst (_,stm)) v = VM.peek stm v;

fun norm (Subst (sty,stm)) =
    let
      val sty = TyU.norm sty
                
      val stm =
          let
            fun p (n,ty) (T.Var (n',ty')) =
                n = n' andalso Ty.equal (TyU.subst sty ty) (TyU.subst sty ty')
              | p _ _ = false

            fun f (v,tm,z) = if p v (T.dest tm) then z else VM.insert z (v,tm)
          in
            VM.foldl f (VM.new ()) stm
          end
    in
      Subst (sty,stm)
    end;

fun substType (Subst (sty,_)) = TyU.subst sty;

local
  exception Capture of int;

  fun typeSubstVar sty v : Var.var =
      let
        val (n,ty) = v
        val ty' = sty ty
      in
        if ty == ty' then v else (n,ty')
      end;

  fun bvMin newDepth (v,b) =
      case VM.peek newDepth v of
        NONE => b
      | SOME d =>
        let
          val r = case b of NONE => true | SOME m => d < m
        in
          if r then SOME d else b
        end;

  fun substGen sty stm bv tm =
      case T.dest tm of
        T.Const (n,ty) =>
        let
          val ty' = sty ty
        in
          if ty == ty' then tm else T.mkConst (n,ty')
        end
      | T.Var v =>
        (case VM.peek (#oldToNew bv) v of
           SOME v' => if v == v' then tm else T.mkVar v'
         | NONE =>
           let
             val v' = typeSubstVar sty v
             val (tm,fv) = 
                 case VM.peek stm v' of
                   SOME tm_fv => tm_fv
                 | NONE =>
                   (if v == v' then tm else T.mkVar v', VS.singleton v')
           in
             if #depth bv = 0 then tm
             else
               case VS.foldl (bvMin (#newDepth bv)) NONE fv of
                 NONE => tm
               | SOME d => raise Capture d
           end)
      | T.App (a,b) =>
        let
          val a' = substGen sty stm bv a
          val b' = substGen sty stm bv b
        in
          if a' == a andalso b' == b then tm else T.mkComb (a',b')
        end
      | T.Lam (v,body) =>
        let
          val {depth,oldToNew,newDepth} = bv
                                             
          fun rename v' =
              if Option.isSome (VM.peek newDepth v') then
                rename (Var.variant v')
              else
                let
                  val oldToNew = VM.insert oldToNew (v,v')
                  val newDepth = VM.insert newDepth (v',depth)
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
          if v == v' andalso body == body' then tm else T.mkAbs (v',body')
        end;

  fun rawSubst sty stm =
      let
        val info = {depth = 0, oldToNew = VM.new (), newDepth = VM.new ()}
      in
        substGen sty stm info
      end;

  val typeSubst = C rawSubst (VM.new ());
in
  fun subst sub =
      let
        val sub as Subst (sty,stm) = norm sub
                  
        val sty = TyU.subst sty

        val stm =
            let
              fun f (v,tm,z) =
                  let
                    val v = typeSubstVar sty v
                    val tm = typeSubst sty tm
                    val fv = T.freeVars tm
                  in
                    VM.insert z (v,(tm,fv))
                  end
            in
              VM.foldl f (VM.new ()) stm
            end
      in
        rawSubst sty stm
      end;
end;

fun toListType (Subst (sty,_)) = TyU.toList sty;

fun toList (Subst (_,stm)) = VM.foldr (fn (v,tm,l) => (v,tm) :: l) [] stm;

end
