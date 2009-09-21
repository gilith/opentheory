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

val emptyTermMap : termSubstMap = VarMap.new ();

val nullTermMap : termSubstMap -> bool = VarMap.null;

val singletonTermMap : Var.var * Term.term -> termSubstMap = VarMap.singleton;

val fromListTermMap : (Var.var * Term.term) list -> termSubstMap =
    VarMap.fromList;

val ppTermMap =
    Print.ppMap VarMap.toList (Print.ppList (Print.ppPair Var.pp Term.pp));

(* ------------------------------------------------------------------------- *)
(* Type and term substitution maps.                                          *)
(* ------------------------------------------------------------------------- *)

type substMap = TypeSubst.substMap * termSubstMap;

val emptyMap : substMap = (TypeSubst.emptyMap, emptyTermMap);

(* ------------------------------------------------------------------------- *)
(* A capture-avoiding substitution function that preserves sharing.          *)
(* ------------------------------------------------------------------------- *)

fun mkAvoidSet var ty' =
    let
      fun addTy (v,avoid) =
          let
            val TypeTerm.Var (n,ty) = v
          in
            if Type.equal ty ty' then NameSet.add avoid n else avoid
          end

      fun freeVarsTy tm' seen' fvShare =
          let
            val i' = Term.id tm'
          in
            case IntMap.peek seen' i' of
              SOME avoid => (avoid,seen',fvShare)
            | NONE =>
              let
                val (fvs,fvShare) = Term.sharingFreeVars tm' fvShare
                val avoid = VarSet.foldl addTy NameSet.empty fvs
(*OpenTheoryTrace5
                val () = Print.trace Term.pp
                           "TermSubst.mkAvoidSet.freeVarsTy: tm'" tm'
                val () = Print.trace NameSet.pp
                           "TermSubst.mkAvoidSet.freeVarsTy: avoid" avoid
*)
                val seen' = IntMap.insert seen' (i',avoid)
              in
                (avoid,seen',fvShare)
              end
          end

      fun mkAvoid tm tm' seen2 seen' fvShare =
          let
(*OpenTheoryTrace5
            val () = Print.trace Term.pp
                       "TermSubst.mkAvoidSet.mkAvoid: tm" tm
            val () = Print.trace Term.pp
                       "TermSubst.mkAvoidSet.mkAvoid: tm'" tm'
*)
            val i2 = (Term.id tm, Term.id tm')
          in
            case IntPairMap.peek seen2 i2 of
              SOME (free,avoid) => (free,avoid,seen2,seen',fvShare)
            | NONE =>
              let
                val (free,avoid,seen2,seen',fvShare) =
                    case Term.dest tm of
                      TypeTerm.Const' _ =>
                      let
                        val free = false
                        val avoid = NameSet.empty
                      in
                        (free,avoid,seen2,seen',fvShare)
                      end
                    | TypeTerm.Var' v =>
                      if Var.equal var v then
                        let
                          val free = true
                          val avoid = NameSet.empty
                        in
                          (free,avoid,seen2,seen',fvShare)
                        end
                      else
                        let
                          val free = false
                          val (avoid,seen',fvShare) =
                              freeVarsTy tm' seen' fvShare
                        in
                          (free,avoid,seen2,seen',fvShare)
                        end
                    | TypeTerm.App' (f,a) =>
                      let
                        val (f',a') = Term.destApp tm'

                        val (fFree,fAvoid,seen2,seen',fvShare) =
                            mkAvoid f f' seen2 seen' fvShare

                        val (aFree,aAvoid,seen2,seen',fvShare) =
                            mkAvoid a a' seen2 seen' fvShare

                        val free = fFree orelse aFree

                        val avoid = NameSet.union fAvoid aAvoid
                      in
                        (free,avoid,seen2,seen',fvShare)
                      end
                    | TypeTerm.Abs' (v,b) =>
                      let
                        val (v',b') = Term.destAbs tm'

                        val (free,avoid,seen2,seen',fvShare) =
                            mkAvoid b b' seen2 seen' fvShare

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
                        (free,avoid,seen2,seen',fvShare)
                      end

                val seen2 = IntPairMap.insert seen2 (i2,(free,avoid))
              in
                (free,avoid,seen2,seen',fvShare)
              end
          end

(*OpenTheoryTrace4
      val () = Print.trace Var.pp "TermSubst.mkAvoidSet: var" var
      val () = Print.trace Type.pp "TermSubst.mkAvoidSet: ty'" ty'
*)
      val seen2 = IntPairMap.new ()
      val seen' = IntMap.new ()
    in
      fn tm => fn tm' => fn fvShare =>
         let
           val (_,avoid,seen2,_,fvShare) = mkAvoid tm tm' seen2 seen' fvShare
         in
           (avoid,seen2,fvShare)
         end
    end;

fun renameBoundVar avoidSeen2 var varTm' =
    let
      fun rename tm tm' seen2 =
          let
            val i2 = (Term.id tm, Term.id tm')
          in
            case IntPairMap.peek avoidSeen2 i2 of
              SOME (false,_) => (tm',seen2)
(*OpenTheoryDebug
            | NONE => raise Bug "TermSubst.renameBoundVar: unseen by mkAvoidSet"
*)
            | _ =>
              case IntPairMap.peek seen2 i2 of
                SOME tm' => (tm',seen2)
              | NONE =>
                let
                  val (tm',seen2) =
                      case Term.dest tm of
                        TypeTerm.Const' _ => (tm',seen2)
                      | TypeTerm.Var' v =>
                        if Var.equal var v then (varTm',seen2) else (tm',seen2)
                      | TypeTerm.App' (f,a) =>
                        let
                          val (f',a') = Term.destApp tm'
                          val (f',seen2) = rename f f' seen2
                          val (a',seen2) = rename a a' seen2
                          val tm' = Term.mkApp (f',a')
                        in
                          (tm',seen2)
                        end
                      | TypeTerm.Abs' (v,b) =>
                        let
(*OpenTheoryDebug
                          val _ = not (Var.equal v var) orelse
                                  raise Bug "TermSubst.renameBoundVar: bad free"
*)
                          val (v',b') = Term.destAbs tm'
                          val (b',seen2) = rename b b' seen2
                          val tm' = Term.mkAbs (v',b')
                        in
                          (tm',seen2)
                        end

                  val seen2 = IntPairMap.insert seen2 (i2,tm')
                in
                  (tm',seen2)
                end
          end

      val seen2 = IntPairMap.new ()
    in
      fn tm => fn tm' =>
         let
           val (tm',_) = rename tm tm' seen2
         in
           tm'
         end
    end;

fun avoidCapture vSub' v v' b b' fvShare =
    let
(*OpenTheoryTrace4
      val () = Print.trace Var.pp "TermSubst.avoidCapture: v" v
      val () = Print.trace Var.pp "TermSubst.avoidCapture: v'" v'
      val () = Print.trace Term.pp "TermSubst.avoidCapture: b" b
      val () = Print.trace Term.pp "TermSubst.avoidCapture: b'" b'
*)
      val (avoid,avoidSeen,fvShare) =
          mkAvoidSet v (Var.typeOf v') b b' fvShare

(*OpenTheoryTrace4
      val () = Print.trace NameSet.pp "TermSubst.avoidCapture: avoid" avoid
*)
      val v'' = Var.renameAvoiding avoid v'

      val b' =
          if not vSub' andalso Var.equal v'' v' then b'
          else renameBoundVar avoidSeen v (Term.mkVar v'') b b'
    in
      (v'',b',fvShare)
    end;

(*OpenTheoryDebug
local
  fun naiveSub stm tySub stm' (tm : Term.term) : Term.term =
      case Term.dest tm of
        TypeTerm.Const' (n,ty) =>
        let
          val ty = Option.getOpt (TypeSubst.subst tySub ty, ty)
        in
          Term.mkConst (n,ty)
        end
      | TypeTerm.Var' v =>
        (case VarMap.peek stm v of
           SOME tm => tm
         | NONE =>
           let
             val v = Option.getOpt (Var.subst tySub v, v)
           in
             case VarMap.peek stm' v of
               SOME tm => tm
             | NONE => Term.mkVar v
           end)
        | TypeTerm.App' (f,a) =>
          let
            val f = naiveSub stm tySub stm' f
            and a = naiveSub stm tySub stm' a
          in
            Term.mkApp (f,a)
          end
        | TypeTerm.Abs' (v,b) =>
          let
            val n = Name.newName ()
            val ty = Var.typeOf v
            val ty = Option.getOpt (TypeSubst.subst tySub ty, ty)
            val v' = Var.mk (n,ty)
            val stm = VarMap.insert stm (v, Term.mkVar v')
            val b = naiveSub stm tySub stm' b
          in
            Term.mkAbs (v',b)
          end;
in
  fun naiveSubst stm' tySub =
      let
        val stm = VarMap.new ()
      in
        naiveSub stm tySub stm'
      end;
end;
*)

fun rawSharingSubst stm tm tySub seen fvShare =
(*OpenTheoryDebug
    let
      val ret as (tm',_,_,_) = rawSharingSubst' stm tm tySub seen fvShare

      val tm' = Option.getOpt (tm',tm)

      val tm'' = naiveSubst stm tySub tm

      val () =
          if Term.alphaEqual tm' tm'' then ()
          else
            let
              val () = Print.trace ppTermMap "TermSubst.rawSharingSubst: stm" stm
              val () = Print.trace Term.pp "TermSubst.rawSharingSubst: tm" tm
              val () = Print.trace TypeSubst.pp
                         "TermSubst.rawSharingSubst: tySub" tySub
              val () = Print.trace Term.pp
                         "TermSubst.rawSharingSubst: reference result" tm''
              val () = Print.trace Term.pp
                         "TermSubst.rawSharingSubst: computed result" tm'
            in
              raise Bug "TermSubst.rawSharingSubst: wrong result"
            end
    in
      ret
    end
and rawSharingSubst' stm tm tySub seen fvShare =
*)
    let
      val i = Term.id tm
    in
      case IntMap.peek seen i of
        SOME tm' => (tm',tySub,seen,fvShare)
      | NONE =>
        case Term.dest tm of
          TypeTerm.Const' (n,ty) =>
          let
            val (ty',tySub) = TypeSubst.sharingSubst ty tySub

            val tm' =
                case ty' of
                  NONE => NONE
                | SOME ty => SOME (Term.mkConst (n,ty))

            val seen = IntMap.insert seen (i,tm')
(*OpenTheoryTrace4
            val () = Print.trace (Print.ppPair Term.pp (Print.ppOption Term.pp)) "TermSubst.rawSharingSubst: (tm,tm')" (tm,tm')
*)
          in
            (tm',tySub,seen,fvShare)
          end
        | TypeTerm.Var' v =>
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
(*OpenTheoryTrace4
            val () = Print.trace (Print.ppPair Term.pp (Print.ppOption Term.pp)) "TermSubst.rawSharingSubst: (tm,tm')" (tm,tm')
*)
          in
            (tm',tySub,seen,fvShare)
          end
        | TypeTerm.App' (f,a) =>
          let
            val (f',tySub,seen,fvShare) =
                rawSharingSubst stm f tySub seen fvShare

            val (a',tySub,seen,fvShare) =
                rawSharingSubst stm a tySub seen fvShare

            val tm' =
                case (f',a') of
                  (SOME f, SOME a) => SOME (Term.mkApp (f,a))
                | (SOME f, NONE) => SOME (Term.mkApp (f,a))
                | (NONE, SOME a) => SOME (Term.mkApp (f,a))
                | (NONE,NONE) => NONE

            val seen = IntMap.insert seen (i,tm')
(*OpenTheoryTrace4
            val () = Print.trace (Print.ppPair Term.pp (Print.ppOption Term.pp)) "TermSubst.rawSharingSubst: (tm,tm')" (tm,tm')
*)
          in
            (tm',tySub,seen,fvShare)
          end
        | TypeTerm.Abs' (v,b) =>
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
(*OpenTheoryTrace4
            val () = Print.trace (Print.ppPair Term.pp (Print.ppOption Term.pp)) "TermSubst.rawSharingSubst: (tm,tm')" (tm,tm')
*)
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
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val toStringTermMap = Print.toString ppTermMap;

val ppMap = Print.ppPair TypeSubst.ppMap ppTermMap;

val toStringMap = Print.toString ppMap;

fun pp (Subst {tySub,stm,...}) =
    Print.ppPair TypeSubst.pp ppTermMap (tySub,stm);

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

fun sharingSubstType ty sub =
    let
      val Subst {tySub,stm,seen} = sub
      val (ty',tySub) = TypeSubst.sharingSubst ty tySub
      val sub = Subst {tySub = tySub, stm = stm, seen = seen}
    in
      (ty',sub)
    end;

fun substType (Subst {tySub,...}) ty = TypeSubst.subst tySub ty;

fun sharingSubst tm sub =
    let
(*OpenTheoryTrace3
      val () = Print.trace pp "TermSubst.sharingSubst: sub" sub
*)
      val Subst {tySub,stm,seen} = sub
      val fvShare = Term.newSharingFreeVars
      val (tm',tySub,seen,_) = rawSharingSubst stm tm tySub seen fvShare
      val sub = Subst {tySub = tySub, stm = stm, seen = seen}
    in
      (tm',sub)
    end;

fun subst sub tm = fst (sharingSubst tm sub);

end
