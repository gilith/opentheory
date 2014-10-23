(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TERMS                                *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure TermSubst :> TermSubst =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Term substitution maps.                                                   *)
(* ------------------------------------------------------------------------- *)

type substMap = Term.term VarMap.map;

val emptyMap : substMap = VarMap.new ();

val nullMap : substMap -> bool = VarMap.null;

val singletonMap : Var.var * Term.term -> substMap = VarMap.singleton;

val normalizeMap =
    let
      fun pred (v,tm) = not (Term.equalVar v tm)
    in
      VarMap.filter pred
    end;

val fromListMap : (Var.var * Term.term) list -> substMap =
    VarMap.fromList;

val ppMap =
    Print.ppMap VarMap.toList (Print.ppList (Print.ppPair Var.pp Term.pp));

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
              val () = Print.trace ppMap "TermSubst.rawSharingSubst: stm" stm
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
       stm : substMap,
       seen : Term.term option IntMap.map};

fun mk sty stm =
    let
      val stm = normalizeMap stm
      and seen = IntMap.new ()
    in
      Subst
        {tySub = sty,
         stm = stm,
         seen = seen}
    end;

val mkMono = mk TypeSubst.empty;

fun dest (Subst {tySub, stm, seen = _}) = (tySub,stm);

val empty = mkMono emptyMap;

fun null (Subst {tySub,stm,...}) =
    TypeSubst.null tySub andalso nullMap stm;

fun typeSubst (Subst {tySub,...}) = tySub;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val toStringMap = Print.toString ppMap;

fun pp (Subst {tySub,stm,...}) =
    Print.ppPair TypeSubst.pp ppMap (tySub,stm);

val toString = Print.toString pp;

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

(* Types *)

fun sharingSubstType ty sub =
    let
      val Subst {tySub,stm,seen} = sub

      val (ty',tySub) = TypeSubst.sharingSubst ty tySub

      val sub = Subst {tySub = tySub, stm = stm, seen = seen}
    in
      (ty',sub)
    end;

fun substType (Subst {tySub,...}) ty = TypeSubst.subst tySub ty;

(* Variables *)

fun sharingSubstVar v sub =
    let
      val Subst {tySub,stm,seen} = sub

      val (v',tySub) = Var.sharingSubst v tySub

      val sub = Subst {tySub = tySub, stm = stm, seen = seen}
    in
      (v',sub)
    end;

fun substVar (Subst {tySub,...}) v = Var.subst tySub v;

(* Terms *)

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

fun subst sub tm =
    let
      val (tm',_) = sharingSubst tm sub
    in
      tm'
    end;

(* Term sets *)

local
  fun add (tm,(tms,unchanged,sub)) =
      let
        val (tm',sub) = sharingSubst tm sub

        val (tms,unchanged) =
            case tm' of
              SOME tm => (tm :: tms, false)
            | NONE => (tm :: tms, unchanged)
      in
        (tms,unchanged,sub)
      end;
in
  fun sharingSubstAlphaSet set sub =
      let
        val (tms,unchanged,sub) = TermAlphaSet.foldl add ([],true,sub) set

        val set' = if unchanged then NONE else SOME (TermAlphaSet.fromList tms)
      in
        (set',sub)
      end;
end;

fun substAlphaSet sub set =
    let
      val (set',_) = sharingSubstAlphaSet set sub
    in
      set'
    end;

(* Term substitution maps *)

local
  fun add (v,tm,(stm,unchanged,sub)) =
      let
        val (v',sub) = sharingSubstVar v sub

        val v = Option.getOpt (v',v)

        val () =
            if not (VarMap.inDomain v stm) then ()
            else raise Error "TermSubst.sharingSubstSubstMap: var collision"

        val (tm',sub) = sharingSubst tm sub

        val tm = Option.getOpt (tm',tm)

        val stm = VarMap.insert stm (v,tm)

        val unchanged =
            unchanged andalso
            not (Option.isSome v') andalso
            not (Option.isSome tm')
      in
        (stm,unchanged,sub)
      end;
in
  fun sharingSubstSubstMap stm sub =
      let
        val (stm,unchanged,sub) = VarMap.foldl add (emptyMap,true,sub) stm

        val stm' = if unchanged then NONE else SOME stm
      in
        (stm',sub)
      end;
end;

fun substSubstMap sub stm =
    let
      val (stm',_) = sharingSubstSubstMap stm sub
    in
      stm'
    end;

end
