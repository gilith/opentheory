(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYMBOL TABLES                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Symbol :> Symbol =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of symbol tables.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype symbol =
    Symbol of
      {opS : TypeOpSet.set,
       opM : TypeOp.typeOp NameMap.map,
       conS : ConstSet.set,
       conM : Const.const NameMap.map};

val empty =
    let
      val opS = TypeOpSet.empty
      val opM = NameMap.new ()
      val conS = ConstSet.empty
      val conM = NameMap.new ()
    in
      Symbol
        {opS = opS,
         opM = opM,
         conS = conS,
         conM = conM}
    end;

fun typeOps (Symbol {opS,...}) = opS;

fun consts (Symbol {conS,...}) = conS;

(* ------------------------------------------------------------------------- *)
(* Looking up entries.                                                       *)
(* ------------------------------------------------------------------------- *)

fun peekTypeOp (Symbol {opM,...}) n = NameMap.peek opM n;

fun peekConst (Symbol {conM,...}) n = NameMap.peek conM n;

fun knownTypeOp (Symbol {opM,...}) n = NameMap.inDomain n opM;

fun knownConst (Symbol {conM,...}) n = NameMap.inDomain n conM;

fun mkTypeOp syms n =
    let
      fun peek sym = peekTypeOp sym n
    in
      case List.mapPartial peek syms of
        [] => TypeOp.mkUndef n
      | ot :: ots =>
        if List.all (TypeOp.equal ot) ots then ot
        else raise Error "Symbol.mkTypeOp: duplicate type names"
    end;

fun mkConst syms n =
    let
      fun peek sym = peekConst sym n
    in
      case List.mapPartial peek syms of
        [] => Const.mkUndef n
      | c :: cs =>
        if List.all (Const.equal c) cs then c
        else raise Error "Symbol.mkConst: duplicate constant names"
    end;

(* ------------------------------------------------------------------------- *)
(* Adding entries.                                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun add (ot,m) =
      let
        val n = TypeOp.name ot
      in
        case NameMap.peek m n of
          NONE => NameMap.insert m (n,ot)
        | SOME ot' =>
          if TypeOp.equal ot ot' then m
          else raise Error "Symbol.addTypeOps: duplicate name"
      end;
in
  fun addTypeOp sym ot =
      let
        val Symbol {opS,opM,conS,conM} = sym
        val opM = add (ot,opM)
        val opS = TypeOpSet.add opS ot
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;

  fun addTypeOpSet sym s =
      let
        val Symbol {opS,opM,conS,conM} = sym
        val opM = TypeOpSet.foldl add opM s
        val opS = TypeOpSet.union opS s
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

local
  fun add (c,m) =
      let
        val n = Const.name c
      in
        case NameMap.peek m n of
          NONE => NameMap.insert m (n,c)
        | SOME c' =>
          if Const.equal c c' then m
          else raise Error "Symbol.addConsts: duplicate name"
      end;
in
  fun addConst sym c =
      let
        val Symbol {opS,opM,conS,conM} = sym
        val conM = add (c,conM)
        val conS = ConstSet.add conS c
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;

  fun addConstSet sym s =
      let
        val Symbol {opS,opM,conS,conM} = sym
        val conM = ConstSet.foldl add conM s
        val conS = ConstSet.union conS s
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

fun addSequent sym seq =
    let
      val sym = addTypeOpSet sym (Sequent.typeOps seq)
      val sym = addConstSet sym (Sequent.consts seq)
    in
      sym
    end;

fun addSequentSet sym seqs =
    let
      val sym = addTypeOpSet sym (SequentSet.typeOps seqs)
      val sym = addConstSet sym (SequentSet.consts seqs)
    in
      sym
    end;

(* ------------------------------------------------------------------------- *)
(* Merging symbol tables.                                                    *)
(* ------------------------------------------------------------------------- *)

local
  fun mergeTypeOps (ot1,ot2) =
      if TypeOp.equal ot1 ot2 then SOME ot2
      else raise Error ("Symbol.union: duplicate type op name" ^
                        Name.toString (TypeOp.name ot1));

  fun mergeConsts (c1,c2) =
      if Const.equal c1 c2 then SOME c2
      else raise Error ("Symbol.union: duplicate const name: " ^
                        Name.toString (Const.name c1));
in
  fun union sym1 sym2 =
      let
        val Symbol {opS = opS1, opM = opM1, conS = conS1, conM = conM1} = sym1
        and Symbol {opS = opS2, opM = opM2, conS = conS2, conM = conM2} = sym2

        val opM = NameMap.union mergeTypeOps opM1 opM2

        val conM = NameMap.union mergeConsts conM1 conM2

        val opS = TypeOpSet.union opS1 opS2

        val conS = ConstSet.union conS1 conS2
      in
        Symbol
          {opS = opS,
           opM = opM,
           conS = conS,
           conM = conM}
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Partition symbol table entries into undefined and defined.                *)
(* ------------------------------------------------------------------------- *)

fun partitionUndef sym =
    let
      val Symbol {opS,conS,...} = sym

      val sym1 = empty
      and sym2 = empty

      val (opS1,opS2) = TypeOpSet.partition TypeOp.isUndef opS

      val sym1 = addTypeOpSet sym1 opS1
      and sym2 = addTypeOpSet sym2 opS2

      val (conS1,conS2) = ConstSet.partition Const.isUndef conS

      val sym1 = addConstSet sym1 conS1
      and sym2 = addConstSet sym2 conS2
    in
      (sym1,sym2)
    end;

(* ------------------------------------------------------------------------- *)
(* Replacing type operators and constants with symbol table entries.         *)
(* These functions return NONE for unchanged.                                *)
(* ------------------------------------------------------------------------- *)

datatype sharingRedef =
    SharingRedef of
      {symbol : symbol,
       types : Type.ty option IntMap.map,
       terms : Term.term option IntMap.map};

fun newSharingRedef symbol =
    let
      val types = IntMap.new ()

      val terms = IntMap.new ()
    in
      SharingRedef
        {symbol = symbol,
         types = types,
         terms = terms}
    end;

fun redefTypeOp sym ot =
    case peekTypeOp sym (TypeOp.name ot) of
      NONE => NONE
    | SOME ot' => if TypeOp.equal ot ot' then NONE else SOME ot';

fun redefConst sym c =
    case peekConst sym (Const.name c) of
      NONE => NONE
    | SOME c' => if Const.equal c c' then NONE else SOME c';

fun sharingRedefType ty share =
    let
      val SharingRedef {types,...} = share

      val i = Type.id ty
    in
      case IntMap.peek types i of
        SOME ty' => (ty',share)
      | NONE =>
        let
          val typ = Type.dest ty

          val (typ',share) = sharingRedefType' typ share

          val ty' =
              case typ' of
                NONE => NONE
              | SOME typ => SOME (Type.mk typ)

          val SharingRedef {symbol,types,terms} = share

          val types = IntMap.insert types (i,ty')

          val share =
              SharingRedef
                {symbol = symbol,
                 types = types,
                 terms = terms}
        in
          (ty',share)
        end
    end

and sharingRedefType' ty share =
    case ty of
      TypeTerm.VarTy' _ => (NONE,share)
    | TypeTerm.OpTy' (ot,tys) =>
      let
        val SharingRedef {symbol,...} = share

        val ot' = redefTypeOp symbol ot

        val (tys',share) = sharingRedefTypeList tys share

        val ty' =
            case (ot',tys') of
              (NONE,NONE) => NONE
            | (SOME ot, NONE) => SOME (TypeTerm.OpTy' (ot,tys))
            | (NONE, SOME tys) => SOME (TypeTerm.OpTy' (ot,tys))
            | (SOME ot, SOME tys) => SOME (TypeTerm.OpTy' (ot,tys))
      in
        (ty',share)
      end

and sharingRedefTypeList tys share =
    case tys of
      [] => (NONE,share)
    | ty :: tys =>
      let
        val (ty',share) = sharingRedefType ty share

        val (tys',share) = sharingRedefTypeList tys share

        val result =
            case tys' of
              SOME tys => SOME (Option.getOpt (ty',ty) :: tys)
            | NONE =>
              case ty' of
                NONE => NONE
              | SOME ty => SOME (ty :: tys)
      in
        (result,share)
      end;

fun sharingRedefVar var share =
    let
      val (n,ty) = Var.dest var

      val (ty',share) = sharingRedefType ty share

      val var' =
          case ty' of
            NONE => NONE
          | SOME ty => SOME (Var.mk (n,ty))
    in
      (var',share)
    end;

fun sharingRedefTerm tm share =
    let
      val SharingRedef {terms,...} = share

      val i = Term.id tm
    in
      case IntMap.peek terms i of
        SOME tm' => (tm',share)
      | NONE =>
        let
          val tmp = Term.dest tm

          val (tmp',share) = sharingRedefTerm' tmp share

          val tm' =
              case tmp' of
                NONE => NONE
              | SOME tmp => SOME (Term.mk tmp)

          val SharingRedef {symbol,types,terms} = share

          val terms = IntMap.insert terms (i,tm')

          val share =
              SharingRedef
                {symbol = symbol,
                 types = types,
                 terms = terms}
        in
          (tm',share)
        end
    end

and sharingRedefTerm' tm share =
    case tm of
      TypeTerm.Const' (c,ty) =>
      let
        val SharingRedef {symbol,...} = share

        val c' = redefConst symbol c

        val (ty',share) = sharingRedefType ty share

        val tm' =
            case (c',ty') of
              (NONE,NONE) => NONE
            | (SOME c, NONE) => SOME (TypeTerm.Const' (c,ty))
            | (NONE, SOME ty) => SOME (TypeTerm.Const' (c,ty))
            | (SOME c, SOME ty) => SOME (TypeTerm.Const' (c,ty))
      in
        (tm',share)
      end
    | TypeTerm.Var' v =>
      let
        val (v',share) = sharingRedefVar v share

        val tm' =
            case v' of
              NONE => NONE
            | SOME v => SOME (TypeTerm.Var' v)
      in
        (tm',share)
      end
    | TypeTerm.App' (f,a) =>
      let
        val (f',share) = sharingRedefTerm f share

        val (a',share) = sharingRedefTerm a share

        val tm' =
            case (f',a') of
              (NONE,NONE) => NONE
            | (SOME f, NONE) => SOME (TypeTerm.App' (f,a))
            | (NONE, SOME a) => SOME (TypeTerm.App' (f,a))
            | (SOME f, SOME a) => SOME (TypeTerm.App' (f,a))
      in
        (tm',share)
      end
    | TypeTerm.Abs' (v,b) =>
      let
        val (v',share) = sharingRedefVar v share

        val (b',share) = sharingRedefTerm b share

        val tm' =
            case (v',b') of
              (NONE,NONE) => NONE
            | (SOME v, NONE) => SOME (TypeTerm.Abs' (v,b))
            | (NONE, SOME b) => SOME (TypeTerm.Abs' (v,b))
            | (SOME v, SOME b) => SOME (TypeTerm.Abs' (v,b))
      in
        (tm',share)
      end;

local
  fun redefAdd (tm,(changed,set,share)) =
      let
        val (tm',share) = sharingRedefTerm tm share

        val (changed,tm) =
            case tm' of
              SOME tm => (true,tm)
            | NONE => (changed,tm)

        val set = TermAlphaSet.add set tm
      in
        (changed,set,share)
      end;
in
  fun sharingRedefSequent seq share =
      let
        val Sequent.Sequent {hyp,concl} = seq

        val changed = false
        val hyp' = TermAlphaSet.empty
        val (changed,hyp',share) =
            TermAlphaSet.foldl redefAdd (changed,hyp',share) hyp

        val (concl',share) = sharingRedefTerm concl share

        val seq' =
            case (changed,concl') of
              (false,NONE) => NONE
            | (true,NONE) => SOME (Sequent.Sequent {hyp = hyp', concl = concl})
            | (false, SOME concl) =>
              SOME (Sequent.Sequent {hyp = hyp, concl = concl})
            | (true, SOME concl) =>
              SOME (Sequent.Sequent {hyp = hyp', concl = concl})
      in
        (seq',share)
      end;
end;

fun redefType sym ty =
    let
      val share = newSharingRedef sym

      val (ty',_) = sharingRedefType ty share
    in
      ty'
    end;

fun redefVar sym v =
    let
      val share = newSharingRedef sym

      val (v',_) = sharingRedefVar v share
    in
      v'
    end;

fun redefTerm sym tm =
    let
      val share = newSharingRedef sym

      val (tm',_) = sharingRedefTerm tm share
    in
      tm'
    end;

fun redefSequent sym seq =
    let
      val share = newSharingRedef sym

      val (seq',_) = sharingRedefSequent seq share
    in
      seq'
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppNameMap (name,nm) =
      let
        val ns = map fst (NameMap.toList nm)
      in
        if null ns then Print.skip
        else
          Print.sequence
            (Print.blockProgram Print.Inconsistent 2
               (Print.addString (name ^ ":") ::
                map (Print.sequence (Print.addBreak 1) o Name.pp) ns))
            Print.addNewline
      end;
in
  fun pp sym =
      let
        val Symbol {opM,conM,...} = sym
      in
        Print.blockProgram Print.Consistent 0
          [ppNameMap ("types",opM),
           ppNameMap ("consts",conM)]
      end;
end;

val toString = Print.toString pp;

end
