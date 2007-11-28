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

fun add_type n_ty (Subst (sty,stm)) = Subst (TyU.add n_ty sty, stm);

fun add v_tm (Subst (sty,stm)) =
    let
      val ((_,ty),tm) = v_tm
      val _ = Ty.equal (T.type_of tm) ty orelse
              raise Error "TermSubst.add: bad type"
    in
      Subst (sty, VM.insert stm v_tm)
    end;

fun singleton_type n_ty = add_type n_ty empty;

fun singleton v_tm = add v_tm empty;

fun add_list_type l sub = foldl (fn (n_ty,s) => add_type n_ty s) sub l;

fun add_list l sub = foldl (fn (v_tm,s) => add v_tm s) sub l;

fun from_list_type l = add_list_type l empty;

fun from_list l = add_list l empty;

fun null (Subst (sty,stm)) = TyU.null sty andalso VM.null stm;

fun peek_type (Subst (sty,_)) n = TyU.peek sty n;

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

fun subst_type (Subst (sty,_)) = TyU.subst sty;

local
  exception Capture of int;

  fun type_subst_var sty v : Var.var =
      let
        val (n,ty) = v
        val ty' = sty ty
      in
        if ty == ty' then v else (n,ty')
      end;

  fun bv_min new_depth (v,b) =
      case VM.peek new_depth v of
        NONE => b
      | SOME d =>
        let
          val r = case b of NONE => true | SOME m => d < m
        in
          if r then SOME d else b
        end;

  fun subst_gen sty stm bv tm =
      case T.dest tm of
        T.Const (n,ty) =>
        let
          val ty' = sty ty
        in
          if ty == ty' then tm else T.mk_const (n,ty')
        end
      | T.Var v =>
        (case VM.peek (#old_to_new bv) v of
           SOME v' => if v == v' then tm else T.mk_var v'
         | NONE =>
           let
             val v' = type_subst_var sty v
             val (tm,fv) = 
                 case VM.peek stm v' of
                   SOME tm_fv => tm_fv
                 | NONE =>
                   (if v == v' then tm else T.mk_var v', VS.singleton v')
           in
             if #depth bv = 0 then tm
             else
               case VS.foldl (bv_min (#new_depth bv)) NONE fv of
                 NONE => tm
               | SOME d => raise Capture d
           end)
      | T.App (a,b) =>
        let
          val a' = subst_gen sty stm bv a
          val b' = subst_gen sty stm bv b
        in
          if a' == a andalso b' == b then tm else T.mk_comb (a',b')
        end
      | T.Lam (v,body) =>
        let
          val {depth,old_to_new,new_depth} = bv
                                             
          fun rename v' =
              if Option.isSome (VM.peek new_depth v') then
                rename (Var.variant v')
              else
                let
                  val old_to_new = VM.insert old_to_new (v,v')
                  val new_depth = VM.insert new_depth (v',depth)
                  val depth = depth + 1
                  val bv = {depth = depth, old_to_new = old_to_new,
                            new_depth = new_depth}
                  val body = subst_gen sty stm bv body
                in
                  (v',body)
                end
                handle c as Capture d =>
                  if d = depth then rename (Var.variant v') else raise c
                                                                        
          val (v',body') = rename (type_subst_var sty v)
        in
          if v == v' andalso body == body' then tm else T.mk_abs (v',body')
        end;

  fun raw_subst sty stm =
      let
        val info = {depth = 0, old_to_new = VM.new (), new_depth = VM.new ()}
      in
        subst_gen sty stm info
      end;

  val type_subst = C raw_subst (VM.new ());
in
  fun subst sub =
      let
        val sub as Subst (sty,stm) = norm sub
                  
        val sty = TyU.subst sty

        val stm =
            let
              fun f (v,tm,z) =
                  let
                    val v = type_subst_var sty v
                    val tm = type_subst sty tm
                    val fv = T.free_vars tm
                  in
                    VM.insert z (v,(tm,fv))
                  end
            in
              VM.foldl f (VM.new ()) stm
            end
      in
        raw_subst sty stm
      end;
end;

fun to_list_type (Subst (sty,_)) = TyU.to_list sty;

fun to_list (Subst (_,stm)) = VM.foldr (fn (v,tm,l) => (v,tm) :: l) [] stm;

end
