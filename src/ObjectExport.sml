(* ========================================================================= *)
(* EXPORT SETS OF THEOREM OBJECTS                                            *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectExport :> ObjectExport =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Combining alpha-equivalent theorem objects.                               *)
(* ------------------------------------------------------------------------- *)

fun mergeEq th1 th2 =
    let
      val ObjectThm.Thm {proof = p1, hyp = h1, concl = c1} = ObjectThm.dest th1
      and ObjectThm.Thm {proof = p2, hyp = h2, concl = c2} = ObjectThm.dest th2

      val pb = Object.id p1 <= Object.id p2
      and hb = Object.id h1 <= Object.id h2
      and cb = Object.id c1 <= Object.id c2
    in
      if pb andalso hb andalso cb then th1
      else if not pb andalso not hb andalso not cb then th2
      else
        ObjectThm.mk
          (ObjectThm.Thm
             {proof = if pb then p1 else p2,
              hyp = if hb then h1 else h2,
              concl = if cb then c1 else c2})
    end;

(* ------------------------------------------------------------------------- *)
(* A type of export sets of theorem objects.                                 *)
(* ------------------------------------------------------------------------- *)

datatype export =
    Export of
      {savable : bool,
       thms : ObjectThm.thm SequentMap.map};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun empty {savable} =
    let
      val thms = SequentMap.new ()
    in
      Export
        {savable = savable,
         thms = thms}
    end;

fun singleton {savable} th =
    let
      val seq = Thm.sequent (ObjectThm.thm th)

      val thms = SequentMap.singleton (seq,th)
    in
      Export
        {savable = savable,
         thms = thms}
    end;

fun savable (Export {savable = x, ...}) = x;

fun toMap (Export {thms = x, ...}) = x;

fun null exp = SequentMap.null (toMap exp);

fun size exp = SequentMap.size (toMap exp);

local
  fun inc ((_ : Sequent.sequent), (th : ObjectThm.thm), ths) = th :: ths;
in
  fun toList exp = SequentMap.foldr inc [] (toMap exp);
end;

fun toThms exp =
    let
      val ths = List.map ObjectThm.thm (toList exp)
    in
      Thms.fromList ths
    end;

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun merge ((_,th1),(_,th2)) = SOME (mergeEq th1 th2);
in
  fun union exp1 exp2 =
      let
        val Export {savable = sav1, thms = ths1} = exp1
        and Export {savable = sav2, thms = ths2} = exp2

        val sav = sav1 andalso sav2

        val ths = SequentMap.union merge ths1 ths2
      in
        Export
          {savable = sav,
           thms = ths}
      end;
end;

local
  val emptySavable = empty {savable = true};

  fun uncurriedUnion (thms1,thms2) = union thms1 thms2;
in
  fun unionList thmsl =
      case thmsl of
        [] => emptySavable
      | thms :: thmsl => List.foldl uncurriedUnion thms thmsl;
end;

(* ------------------------------------------------------------------------- *)
(* Adding theorem objects.                                                   *)
(* ------------------------------------------------------------------------- *)

local
  val singletonSavable = singleton {savable = true};
in
  fun add exp th = union exp ( th);

(* ------------------------------------------------------------------------- *)
(* Looking up theorem objects.                                               *)
(* ------------------------------------------------------------------------- *)

fun peek exp = SequentMap.peek (toMap exp);

fun member seq exp = SequentMap.inDomain seq (toMap exp);

(* ------------------------------------------------------------------------- *)
(* Mapping over exported theorem objects.                                    *)
(* ------------------------------------------------------------------------- *)

fun fold f b exp =
    let
      fun f' (_,th,acc) = f (th,acc)
    in
      SequentMap.foldl f' b (toMap exp)
    end;

local
  fun mapsThm f (seq,th,(unchanged,exp,acc)) =
      let
        val (th',acc) = f th acc

        val (unchanged,th) =
            case th' of
              NONE => (unchanged,th)
            | SOME th => (false,th)

        val exp = add exp x
      in
        (unchanged,exp,acc)
      end;
in
  fun maps f exp0 acc =
      let
        val unchanged = true
        and exp = new {savable = savable exp0}

        val (unchanged,exp,acc) = fold (mapsThm f) (unchanged,exp,acc) exp0

        val exp' = if unchanged then NONE else SOME exp
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
        val hyp = ObjectThm.hyp th
        and concl = ObjectThm.concl th

        val sym = ObjectSymbol.addObject sym hyp

        val sym = ObjectSymbol.addObject sym concl
      in
        sym
      end;
in
  val symbol = fold addThm ObjectSymbol.empty;
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
(* Convert to a given article version.                                       *)
(* ------------------------------------------------------------------------- *)

local
  val setVersionThm = ObjectThm.maps ObjectVersion.sharingConvert;
in
  fun setVersion version exp =
      let
        val acc = ObjectVersion.new version

        val (exp',_) = maps setVersionThm exp acc
      in
        exp'
      end;
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
        fold add ObjectStore.emptyDictionary
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
                    Object.mkDefineTypeOpLegacy savable
                      (TypeOp.name ot) abs rep objV objX
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

        val th =
            ObjectThm.mk
              (ObjectThm.Thm {proof = proof, hyp = hyp, concl = concl})
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
