(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TERMS                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure TermSubst :> TermSubst =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Term substitution maps.                                                   *)
(* ------------------------------------------------------------------------- *)

type termSubstMap = Term.term VarMap.map;

type substMap = TypeSubst.substMap * termSubstMap;

val emptyTermMap : termSubstMap = VarMap.new ();

val emptyMap : substMap = (TypeSubst.emptyMap, emptyTermMap);

val nullTermMap : termSubstMap -> bool = VarMap.null;

(***
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
***)

(* ------------------------------------------------------------------------- *)
(* A capture-avoiding substitution function that preserves sharing.          *)
(* ------------------------------------------------------------------------- *)

fun mkAvoidSet var ty' =
    let
      fun addTy (v,avoid) =
          let
            val Var.Var (n,ty) = v
          in
            if Type.equal ty ty' then NameSet.add avoid n else avoid
          end

      fun freeVarsTy tm' seen' fvShare =
          let
            val i = Term.id tm'
          in
            case IntMap.peek seen' i of
              SOME avoid => (avoid,seen',fvShare)
            | NONE =>
              let
                val (fvs,fvShare) = Term.sharingFreeVars tm' fvShare
                val avoid = VarSet.foldl addTy NameSet.empty fvs
                val seen' = IntMap.insert seen' (i,avoid)
              in
                (avoid,seen',fvShare)
              end
          end

      fun mkAvoid tm tm' seen seen' fvShare =
          let
            val i = Term.id tm
          in
            case IntMap.peek seen i of
              SOME (free,avoid) => (free,avoid,seen,seen',fvShare)
            | NONE =>
              let
                val (free,avoid,seen,seen',fvShare) =
                    case Term.dest tm of
                      Term.Const _ =>
                      let
                        val free = false
                        val avoid = NameSet.empty
                      in
                        (free,avoid,seen,seen',fvShare)
                      end
                    | Term.Var v =>
                      if Var.equal var v then
                        let
                          val free = true
                          val avoid = NameSet.empty
                        in
                          (free,avoid,seen,seen',fvShare)
                        end
                      else
                        let
                          val free = false
                          val (avoid,seen',fvShare) =
                              freeVarsTy tm' seen' fvShare
                        in
                          (free,avoid,seen,seen',fvShare)
                        end
                    | Term.Comb (f,a) =>
                      let
                        val (f',a') = Term.destComb tm'

                        val (fFree,fAvoid,seen,seen',fvShare) =
                            mkAvoid f f' seen seen' fvShare

                        val (aFree,aAvoid,seen,seen',fvShare) =
                            mkAvoid a a' seen seen' fvShare

                        val free = fFree orelse aFree

                        val avoid = NameSet.union fAvoid aAvoid
                      in
                        (free,avoid,seen,seen',fvShare)
                      end
                    | Term.Abs (v,b) =>
                      let
                        val (v',b') = Term.destAbs tm'

                        val (free,avoid,seen,seen',fvShare) =
                            mkAvoid b b' seen seen' fvShare

                        val (free,avoid) =
                            if Var.equal var v then (false,avoid)
                            else
                              let
                                val avoid =
                                    if free then addTy (v',avoid) else avoid
                              in
                                (free,avoid)
                              end
                      in
                        (free,avoid,seen,seen',fvShare)
                      end

                val seen = IntMap.insert seen (i,(free,avoid))
              in
                (free,avoid,seen,seen',fvShare)
              end
          end

      val seen = IntMap.new ()
      val seen' = IntMap.new ()
    in
      fn tm => fn tm' => fn fvShare =>
         let
           val (_,avoid,seen,_,fvShare) = mkAvoid tm tm' seen seen' fvShare
         in
           (avoid,seen,fvShare)
         end
    end;

fun renameBoundVar avoidSeen var varTm' =
    let
      fun rename tm tm' seen =
          let
            val i = Term.id tm
          in
            case IntMap.peek avoidSeen i of
              SOME (false,_) => (tm',seen)
(*OpenTheoryDebug
            | NONE => raise Bug "TermSubst.renameBoundVar: unseen by mkAvoidSet"
*)
            | _ =>
              case IntMap.peek seen i of
                SOME tm' => (tm',seen)
              | NONE =>
                let
                  val (tm',seen) =
                      case Term.dest tm of
                        Term.Const _ => (tm',seen)
                      | Term.Var v =>
                        if Var.equal var v then (varTm',seen) else (tm',seen)
                      | Term.Comb (f,a) =>
                        let
                          val (f',a') = Term.destComb tm'
                          val (f',seen) = rename f f' seen
                          val (a',seen) = rename a a' seen
                          val tm' = Term.mkComb (f',a')
                        in
                          (tm',seen)
                        end
                      | Term.Abs (v,b) =>
                        let
(*OpenTheoryDebug
                          val _ = not (Var.equal v var) orelse
                                  raise Bug "TermSubst.renameBoundVar: bad free"
*)
                          val (v',b') = Term.destAbs tm'
                          val (b',seen) = rename b b' seen
                          val tm' = Term.mkAbs (v',b')
                        in
                          (tm',seen)
                        end

                  val seen = IntMap.insert seen (i,tm')
                in
                  (tm',seen)
                end
          end

      val seen = IntMap.new ()
    in
      fn tm => fn tm' =>
         let
           val (tm',_) = rename tm tm' seen
         in
           tm'
         end
    end;

fun avoidCapture vSub' v v' b b' fvShare =
    let
      val (avoid,avoidSeen,fvShare) =
          mkAvoidSet v (Var.typeOf v') b b' fvShare

      val v'' = Var.renameAvoiding avoid v'

      val b' =
          if not vSub' andalso Var.equal v'' v' then b'
          else renameBoundVar avoidSeen v (Term.mkVar v') b b'
    in
      (v'',b',fvShare)
    end;

fun rawSharingSubst stm tm tySub seen fvShare =
    let
      val i = Term.id tm
    in
      case IntMap.peek seen i of
        SOME tm' => (tm',tySub,seen,fvShare)
      | NONE =>
        case Term.dest tm of
          Term.Const (n,ty) =>
          let
            val (ty',tySub) = TypeSubst.sharingSubst ty tySub

            val tm' =
                case ty' of
                  NONE => NONE
                | SOME ty => SOME (Term.mkConst (n,ty))

            val seen = IntMap.insert seen (i,tm')
          in
            (tm',tySub,seen,fvShare)
          end
        | Term.Var v =>
          let
            val (v',tySub) = Var.sharingSubst v tySub

            val (changed,v') =
                case v' of
                  SOME v => (true,v)
                | NONE => (false,v)

            val tm' =
                case VarMap.peek stm v' of
                  NONE => if changed then SOME (Term.mkVar v') else NONE
                | tm' => tm'

            val seen = IntMap.insert seen (i,tm')
          in
            (tm',tySub,seen,fvShare)
          end
        | Term.Comb (f,a) =>
          let
            val (f',tySub,seen,fvShare) =
                rawSharingSubst stm f tySub seen fvShare

            val (a',tySub,seen,fvShare) =
                rawSharingSubst stm a tySub seen fvShare

            val tm' =
                case (f',a') of
                  (SOME f, SOME a) => SOME (Term.mkComb (f,a))
                | (SOME f, NONE) => SOME (Term.mkComb (f,a))
                | (NONE, SOME a) => SOME (Term.mkComb (f,a))
                | (NONE,NONE) => NONE

            val seen = IntMap.insert seen (i,tm')
          in
            (tm',tySub,seen,fvShare)
          end
        | Term.Abs (v,b) =>
          let
            val (v',tySub) = Var.sharingSubst v tySub

            val (b',tySub,seen,fvShare) =
                rawSharingSubst stm b tySub seen fvShare

            val (tm',fvShare) =
                case (v',b') of
                  (NONE,NONE) => (NONE,fvShare)
                | _ =>
                  let
                    val v' = Option.getOpt (v',v)
                    and b' = Option.getOpt (b',b)
                    val vSub' = VarMap.inDomain v' stm
                    val (v',b',fvShare) = avoidCapture vSub' v v' b b' fvShare
                    val tm' = SOME (Term.mkAbs (v',b'))
                  in
                    (tm',fvShare)
                  end

            val seen = IntMap.insert seen (i,tm')
          in
            (tm',tySub,seen,fvShare)
          end
    end;

(* ------------------------------------------------------------------------- *)
(* Capture-avoiding substitutions of type and term variables.                *)
(* ------------------------------------------------------------------------- *)

datatype subst =
    Subst of
      {tySub : TypeSubst.subst,
       stm : termSubstMap,
       seen : Term.term option IntMap.map};

val emptySeen : Term.term option IntMap.map = IntMap.new ();

val empty =
    let
      val tySub = TypeSubst.empty
      val stm = emptyTermMap
      val seen = emptySeen
    in
      Subst
        {tySub = tySub,
         stm = stm,
         seen = seen}
    end;

fun null (Subst {tySub,stm,...}) =
    TypeSubst.null tySub andalso nullTermMap stm;

local
  fun add (v,tm,(stm,tySub,seen,fvShare)) =
      let
        val (v',tySub) = Var.sharingSubst v tySub

        val (tm',tySub,seen,fvShare) =
            rawSharingSubst emptyTermMap tm tySub seen fvShare

        val v = Option.getOpt (v',v)

        val tm = Option.getOpt (tm',tm)

        val stm =
            if Term.equalVar v tm then stm
            else if VarMap.inDomain v stm then
              raise Error "TermSubst.newSharingSubst: bad subst"
            else VarMap.insert stm (v,tm)
      in
        (stm,tySub,seen,fvShare)
      end;
in
  fun mk (sty,stm) =
      let
        val tySub = TypeSubst.mk sty

        val seen = emptySeen

        val fvShare = Term.newSharingFreeVars

        val (stm,tySub,_,_) =
            VarMap.foldl add (emptyTermMap,tySub,seen,fvShare) stm
      in
        Subst
          {tySub = tySub,
           stm = stm,
           seen = seen}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

fun sharingSubstType ty sub =
    let
      val Subst {tySub,stm,seen} = sub
      val (ty',tySub) = TypeSubst.sharingSubstType ty tySub
      val sub = Subst {tySub = tySub, stm = stm, seen = seen}
    in
      (ty',sub)
    end;

end
