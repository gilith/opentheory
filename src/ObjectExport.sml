(* ========================================================================= *)
(* EXPORT SETS OF THEOREM OBJECTS                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectExport :> ObjectExport =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of export sets of theorem objects.                                 *)
(* ------------------------------------------------------------------------- *)

datatype export =
    Export of
      {thms : ObjectThmSet.set,
       savable : bool};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun new {savable} =
    let
      val size = 0
      and thms = ObjectThmSet.empty
    in
      Export
        {thms = thms,
         savable = savable}
    end;

val empty = new {savable = true};

fun savable (Export {savable = x, ...}) = x;

fun toSet (Export {thms = x, ...}) = x;

fun null exp = ObjectThmSet.null (toSet exp);

fun size exp = ObjectThmSet.size (toSet exp);

fun add exp th =
    let
      val Export {thms,savable} = exp

      val thms = ObjectThmSet.add thms th
    in
      Export
        {thms = thms,
         savable = savable}
    end;

fun foldl f b exp = ObjectThmSet.foldl f b (toSet exp);

fun foldr f b exp = ObjectThmSet.foldr f b (toSet exp);

fun toList exp = ObjectThmSet.toList (toSet exp);

fun toThms exp =
    let
      val ths = List.map ObjectThm.thm (toList exp)
    in
      Thms.fromList ths
    end;

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

fun union exp1 exp2 =
    let
      val Export {thms = ths1, savable = sav1} = exp1
      and Export {thms = ths2, savable = sav2} = exp2

      val sav = sav1 andalso sav2

      val ths = ObjectThmSet.union ths1 ths2
    in
      Export
        {thms = ths,
         savable = sav}
    end;

local
  fun uncurriedUnion (thms1,thms2) = union thms1 thms2;
in
  fun unionList thmsl =
      case thmsl of
        [] => empty
      | thms :: thmsl => List.foldl uncurriedUnion thms thmsl;
end;

(* ------------------------------------------------------------------------- *)
(* Mapping over exported theorem objects.                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun addThm f (th,(unchanged,ths,acc)) =
      let
        val (th',acc) = f th acc

        val (unchanged,th) =
            case th' of
              NONE => (unchanged,th)
            | SOME x => (false,x)

        val ths = ObjectThmSet.add ths th
      in
        (unchanged,ths,acc)
      end;
in
  fun maps f exp acc =
      let
        val Export {thms,savable} = exp

        val (thms',acc) = ObjectThmSet.maps f thms acc

        val exp' =
            case thms' of
              NONE => NONE
            | SOME thms => SOME (Export {thms = thms, savable = savable})
      in
        (exp',acc)
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Symbols.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun addThm (th,sym) =
      let
        val ObjectThm.Thm {proof = _, hyp, concl} = th

        val sym = ObjectSymbol.addObject sym hyp

        val sym = ObjectSymbol.addObject sym concl
      in
        sym
      end;
in
  val symbol = foldl addThm ObjectSymbol.empty;
end;

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

fun eliminateUnwanted exp =
    let
      val () =
          if savable exp then ()
          else raise Bug "ObjectExport.eliminateUnwanted: unsavable"

      val elim = ObjectUnwanted.empty

      val (exp',_) = maps ObjectThm.sharingEliminateUnwanted exp elim
    in
      exp'
    end;

(* ------------------------------------------------------------------------- *)
(* Compression.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  datatype state =
      State of
        {cache : Object.object IntMap.map};

  val initialState =
      let
        val cache = IntMap.new ()
      in
        State
          {cache = cache}
      end;

  fun peekState (State {cache,...}) = IntMap.peek cache;

  fun insertState acc i_obj =
      let
        val State {cache} = acc

        val cache = IntMap.insert cache i_obj
      in
        State
          {cache = cache}
      end;

  fun ppState acc =
      let
        val State {cache} = acc
      in
        Print.consistentBlock 0
          [Print.ppString "State {",
           Print.ppBreak (Print.Break {size = 0, extraIndent = 2}),
           Print.inconsistentBlock 2
             [Print.ppString "cache =",
              Print.break,
              Print.ppPrettyInt (IntMap.size cache)],
           Print.breaks 0,
           Print.ppString "}"]
      end;

  val mkStore =
      let
        fun add (th,store) = ObjectThm.addStore store th;
      in
        foldl add ObjectStore.emptyDictionary
      end;

  fun preDescent store objI acc =
      let
        val i = Object.id objI

        val objI' = peekState acc i
      in
        case objI' of
          SOME objR =>
          let
            val objR' = if Object.equalId i objR then NONE else objI'
          in
            {descend = false, result = (objR',acc)}
          end
        | NONE =>
          let
            val (objJ,_) = ObjectStore.build (Object.data objI) store

            val j = Object.id objJ
          in
            if j = i then {descend = true, result = (NONE,acc)}
            else
              let
                val objR' = peekState acc j
              in
                case objR' of
                  NONE =>
                  let
                    val acc = insertState acc (i,objJ)
                  in
                    {descend = true, result = (SOME objJ, acc)}
                  end
                | SOME objR =>
                  let
                    val acc = insertState acc (i,objR)
                  in
                    {descend = false, result = (objR',acc)}
                  end
              end
          end
      end;

  fun postDescent objI objR' (acc : state) =
      let
        val i = Object.id objI
      in
        case objR' of
          NONE =>
          let
            val acc = insertState acc (i,objI)
          in
            (objR',acc)
          end
        | SOME objR =>
          let
            val acc =
                case peekState acc i of
                  NONE => acc
                | SOME objJ => insertState acc (Object.id objJ, objR)

            val acc = insertState acc (i,objR)
          in
            (objR',acc)
          end
      end;

  fun compressObj store =
      Object.maps
        {preDescent = preDescent store,
         postDescent = postDescent,
         savable = true};

  fun compressThm store = ObjectThm.maps (compressObj store);
in
  fun compress exp =
      let
        val store = mkStore exp

        val acc = initialState

        val (exp',acc) = maps (compressThm store) exp acc

(*OpenTheoryTrace4
        val () = Print.trace ObjectStore.pp
                   "ObjectExport.compress: refs" store

        val () = Print.trace ppState
                   "ObjectExport.compress: final state" acc
*)
      in
        exp'
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap size (Print.ppBracket "export{" "}" Print.ppInt);

(* ------------------------------------------------------------------------- *)
(* Branding theorems.                                                        *)
(* ------------------------------------------------------------------------- *)

local
  val savable = {savable = true};

  val unknownName = Name.mkGlobal "_"
  and xName = Name.mkGlobal "x"
  and yName = Name.mkGlobal "y";

  val emptyAbsRep =
      let
        val empty : Name.name TypeOpMap.map = TypeOpMap.new ()
      in
        (empty,empty)
      end;

  fun addAbsRep (c,abs_rep) =
      case Const.prov c of
        TypeTerm.UndefProvConst => abs_rep
      | TypeTerm.DefProvConst _ => abs_rep
      | TypeTerm.AbsProvConst ot =>
        let
          val (absM,repM) = abs_rep

          val absM = TypeOpMap.insert absM (ot, Const.name c)
        in
          (absM,repM)
        end
      | TypeTerm.RepProvConst ot =>
        let
          val (absM,repM) = abs_rep

          val repM = TypeOpMap.insert repM (ot, Const.name c)
        in
          (absM,repM)
        end;

  fun getAbsRep (absM,repM) ot =
      let
        val abs = Option.getOpt (TypeOpMap.peek absM ot, unknownName)
        and rep = Option.getOpt (TypeOpMap.peek repM ot, unknownName)
      in
        (abs,rep)
      end;

  fun internalAbsRep (absM,repM) ot =
      not (TypeOpMap.inDomain ot absM) andalso
      not (TypeOpMap.inDomain ot repM);

  val emptyStore : Object.object ObjectDataMap.map =
      ObjectDataMap.new ();

  fun peekStore (store : Object.object ObjectDataMap.map) d =
      ObjectDataMap.peek store d;

  fun addStore (store : Object.object ObjectDataMap.map) d_obj =
      ObjectDataMap.insert store d_obj;

  fun brandTerm n abs_rep =
      let
        fun mkData d store =
            case peekStore store d of
              SOME obj => (obj,store)
            | NONE =>
              let
                val (obj,store) =
                    case d of
                      ObjectData.TypeOp ot =>
                      if TypeOp.isUndef ot then mkStandard d store
                      else
                        let
                          val (obj,_,_,store) = mkTypeOp ot store
                        in
                          (obj,store)
                        end
                    | ObjectData.Const c =>
                      if Const.isUndef c then mkStandard d store
                      else mkConst c store
                    | _ => mkStandard d store

                val store = addStore store (d,obj)
              in
                (obj,store)
              end

        and mkStandard d store =
            let
              val (cmd,args) = ObjectData.command d

              val (args,store) = Useful.maps mkData args store

              val objs = Object.mkCommand savable cmd args

(*OpenTheoryDebug
              val () =
                  case objs of
                    [_] => ()
                  | _ => raise Bug "ObjectExport.brand.mkTerm.mkData"
*)
              val obj = hd objs
            in
              (obj,store)
            end

        and mkTypeOp ot store =
            case TypeOp.prov ot of
              TypeTerm.UndefProvOpTy => raise Bug "ObjectExport.brand.mkTypeOp"
            | TypeTerm.DefProvOpTy def =>
              let
                val TypeTerm.DefOpTy {pred,vars} = def

                val ty =
                    if internalAbsRep abs_rep ot then Type.bool
                    else Type.domainFun (Term.typeOf pred)

                val c = Term.mkConst (Const.mkUndef n, ty)

                val (objX,store) =
                    let
                      val tyVars = Type.typeVars ty

                      fun notTyVar v = not (NameSet.member v tyVars)
                    in
                      case List.filter notTyVar vars of
                        [] =>
                        let
                          val (objC,store) = mkTerm c store

(* ------------------------------------------------------------------------- *)
(* prove  |- c = c  (1)                                                      *)
(*    by  refl `c`                                                           *)
(* ------------------------------------------------------------------------- *)
                          val obj1 = Object.mkRefl savable objC
                        in
                          (obj1,store)
                        end
                      | v :: vs =>
                        let
                          val xTy =
                              let
                                fun toTy (w,t) = Type.mkFun (Type.mkVar w, t)
                              in
                                List.foldl toTy (Type.mkVar v) vs
                              end

                          val xId =
                              let
                                val x = Var.mk (xName,xTy)
                              in
                                Term.mkAbs (x, Term.mkVar x)
                              end

                          val y = Var.mk (yName, Term.typeOf xId)

                          val qcx =
                              let
                                val eqC = Term.rator (Term.mkRefl c)
                              in
                                Term.mkApp (Term.mkAbs (y,eqC), xId)
                              end

                          val (objQCX,store) = mkTerm qcx store

(* ------------------------------------------------------------------------- *)
(* prove  |- (\y. (=) c) (\x. x) = (=) c  (1)                                *)
(*    by  betaConv `(\y. (=) c) (\x. x)`                                     *)
(* ------------------------------------------------------------------------- *)
                          val obj1 = Object.mkBetaConv savable objQCX

                          val (objQ,objQC) =
                              Term.destApp (Thm.concl (Object.destThm obj1))

                          val (objQ,store) = mkTerm (Term.rator objQ) store

                          val (objQC,store) = mkTerm (Term.rator objQC) store

(* ------------------------------------------------------------------------- *)
(* prove  |- (=) = (=)  (2)                                                  *)
(*    by  refl `(=)`                                                         *)
(* ------------------------------------------------------------------------- *)
                          val obj2 = Object.mkRefl savable objQ

(* ------------------------------------------------------------------------- *)
(* prove  |- (=) ((\y. (=) c) (\x. x)) = (=) ((=) c)  (3)                    *)
(*    by  appThm (2) (1)                                                     *)
(* ------------------------------------------------------------------------- *)
                          val obj3 = Object.mkAppThm savable obj2 obj1

(* ------------------------------------------------------------------------- *)
(* prove  |- (\y. (=) c) (\x. x) = (\y. (=) c) (\x. x)  (4)                  *)
(*    by  refl `(\y. (=) c) (\x. x)`                                         *)
(* ------------------------------------------------------------------------- *)
                          val obj4 = Object.mkRefl savable objQCX

(* ------------------------------------------------------------------------- *)
(* prove  |- (\y. (=) c) (\x. x) = (\y. (=) c) (\x. x) <=>                   *)
(*           (=) c = (\y. (=) c) (\x. x)  (5)                                *)
(*    by  appThm (3) (4)                                                     *)
(* ------------------------------------------------------------------------- *)
                          val obj5 = Object.mkAppThm savable obj3 obj4

(* ------------------------------------------------------------------------- *)
(* prove  |- (=) c = (\y. (=) c) (\x. x)  (6)                                *)
(*    by  eqMp (5) (4)                                                       *)
(* ------------------------------------------------------------------------- *)
                          val obj6 = Object.mkEqMp savable obj5 obj4

                          val (objC,store) = mkTerm c store

(* ------------------------------------------------------------------------- *)
(* prove  |- c = c  (7)                                                      *)
(*    by  refl `c`                                                           *)
(* ------------------------------------------------------------------------- *)
                          val obj7 = Object.mkRefl savable objC

(* ------------------------------------------------------------------------- *)
(* prove  |- c = c <=> (\y. (=) c) (\x. x) c  (8)                            *)
(*    by  appThm (6) (7)                                                     *)
(* ------------------------------------------------------------------------- *)
                          val obj8 = Object.mkAppThm savable obj6 obj7

(* ------------------------------------------------------------------------- *)
(* prove  |- (\y. (=) c) (\x. x) c  (9)                                      *)
(*    by  eqMp (8) (7)                                                       *)
(* ------------------------------------------------------------------------- *)
                          val obj9 = Object.mkEqMp savable obj8 obj7
                        in
                          (obj9,store)
                        end
                    end

                val (abs,rep) = getAbsRep abs_rep ot

                val objV = Object.mkList savable (List.map Object.mkName vars)

                val (objT,objA,objR,_,_) =
                    Object.mkDefineTypeOp savable (TypeOp.name ot) abs rep
                      objV objX
              in
                (objT,objA,objR,store)
              end

        and mkConst c store =
            case Const.prov c of
              TypeTerm.UndefProvConst => raise Bug "ObjectExport.brand.mkConst"
            | TypeTerm.DefProvConst def =>
              let
                val TypeTerm.DefConst tm = def

                val tm = Term.mkConst (Const.mkUndef n, Term.typeOf tm)

                val (def,store) = mkTerm tm store

                val (obj,_) = Object.mkDefineConst savable (Const.name c) def
              in
                (obj,store)
              end
            | TypeTerm.AbsProvConst ot =>
              let
                val (_,obj,_,store) = mkTypeOp ot store
              in
                (obj,store)
              end
            | TypeTerm.RepProvConst ot =>
              let
                val (_,_,obj,store) = mkTypeOp ot store
              in
                (obj,store)
              end

        and mkTerm tm store = mkData (ObjectData.Term tm) store
      in
        mkTerm
      end;

  fun brandThm n abs_rep seq store =
      let
        val Sequent.Sequent {hyp,concl} = seq

        val hyp = TermAlphaSet.toList hyp

        val (hyp,store) = Useful.maps (brandTerm n abs_rep) hyp store

        val hyp = Object.mkList savable hyp

        val (concl,store) = brandTerm n abs_rep concl store

        val seq = Object.destSequent (hyp,concl)

        val proof = Object.mkAxiom savable hyp concl seq

        val th = ObjectThm.Thm {proof = proof, hyp = hyp, concl = concl}
      in
        (th,store)
      end;

  fun addThm n abs_rep (seq,(store,exp)) =
      let
        val (th,store) = brandThm n abs_rep seq store

        val exp = add exp th
      in
        (store,exp)
      end;
in
  fun brand n seqs =
      let
        val abs_rep = emptyAbsRep
        and cs = SymbolTable.consts (Sequents.symbol seqs)

        val abs_rep = ConstSet.foldl addAbsRep abs_rep cs

        val store = emptyStore
        and exp = new savable
        and sqs = Sequents.sequents seqs

        val (_,exp) = SequentSet.foldl (addThm n abs_rep) (store,exp) sqs

(*OpenTheoryTrace2
        val () = Print.trace pp "ObjectExport.brand: exp" exp
*)
      in
        exp
      end;
end;

end
