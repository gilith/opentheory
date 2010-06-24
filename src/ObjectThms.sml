(* ========================================================================= *)
(* SYMBOLS CONTAINED IN A SET OF THEOREM OBJECTS                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectThms :> ObjectThms =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of object set theorems.                                            *)
(* ------------------------------------------------------------------------- *)

datatype thms =
    Thms of
      {thms : ThmSet.set,
       symbol : Symbol.symbol,
       typeOps : ObjectProv.object NameMap.map,
       consts : ObjectProv.object NameMap.map,
       seqs : ObjectProv.object SequentMap.map};

val empty =
    let
      val thms = ThmSet.empty
      and symbol = Symbol.empty
      and typeOps = NameMap.new ()
      and consts = NameMap.new ()
      and seqs = SequentMap.new ()
    in
      Thms
        {thms = thms,
         symbol = symbol,
         typeOps = typeOps,
         consts = consts,
         seqs = seqs}
    end;

fun thms (Thms {thms = x, ...}) = x;

fun symbol (Thms {symbol = x, ...}) = x;

local
  fun addSyms ((_,th),sym) = Symbol.addSequent sym (Thm.sequent th);

  fun getSyms objThs = List.foldl addSyms Symbol.empty objThs;

  fun fillTypeOp (ot,otO) =
      let
        val n = TypeOp.name ot
      in
        if NameMap.inDomain n otO then otO
        else NameMap.insert otO (n, ObjectProv.mkTypeOp ot)
      end;

  fun fillConst (con,conO) =
      let
        val n = Const.name con
      in
        if NameMap.inDomain n conO then conO
        else NameMap.insert conO (n, ObjectProv.mkConst con)
      end;
in
  fun fromList objThs =
      let
        val sym = getSyms objThs

        val ots = Symbol.typeOps sym
        and cons = Symbol.consts sym

        fun adds seen otO_conO objs =
            case objs of
              [] => otO_conO
            | obj :: objs =>
              let
                val id = ObjectProv.id obj
              in
                if IntSet.member id seen then adds seen otO_conO objs
                else
                  let
                    val seen = IntSet.add seen id

                    val otO_conO =
                        case ObjectProv.object obj of
                          Object.TypeOp ot =>
                          if not (TypeOpSet.member ot ots) then otO_conO
                          else
                            let
                              val (otO,conO) = otO_conO

                              val n = TypeOp.name ot

                              val otO = NameMap.insert otO (n,obj)
                            in
                              (otO,conO)
                            end
                        | Object.Const con =>
                          if not (ConstSet.member con cons) then otO_conO
                          else
                            let
                              val (otO,conO) = otO_conO

                              val n = Const.name con

                              val conO = NameMap.insert conO (n,obj)
                            in
                              (otO,conO)
                            end
                        | _ => otO_conO

                    val objs = ObjectProv.parents obj @ objs
                  in
                    adds seen otO_conO objs
                  end
              end

        fun split (obj,th) (ths,seqs) =
            let
              val ths = ThmSet.add ths th
              and seqs = SequentMap.insert seqs (Thm.sequent th, obj)
            in
              (obj,(ths,seqs))
            end

        val ths = ThmSet.empty
        and seqs = SequentMap.new ()

        val (objs,(ths,seqs)) = maps split objThs (ths,seqs)

        val otO = NameMap.new ()
        and conO = NameMap.new ()

        val (otO,conO) = adds IntSet.empty (otO,conO) objs

        val otO = TypeOpSet.foldl fillTypeOp otO ots
        and conO = ConstSet.foldl fillConst conO cons
      in
        Thms
          {thms = ths,
           symbol = sym,
           typeOps = otO,
           consts = conO,
           seqs = seqs}
      end
end;

(* ------------------------------------------------------------------------- *)
(* Looking up symbols and theorems.                                          *)
(* ------------------------------------------------------------------------- *)

fun peekThm (Thms {seqs,...}) seq = SequentMap.peek seqs seq;

fun peekTypeOp (Thms {typeOps,...}) n = NameMap.peek typeOps n;

fun peekConst (Thms {consts,...}) n = NameMap.peek consts n;

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun noDups _ = raise Bug "ObjectThms.union.noDups";

  fun pickSnd (_,obj) = SOME obj;
in
  fun union thms1 thms2 =
      let
        val Thms
              {thms = ths1,
               symbol = sym1,
               typeOps = ots1,
               consts = cons1,
               seqs = seqs1} = thms1
        and Thms
              {thms = ths2,
               symbol = sym2,
               typeOps = ots2,
               consts = cons2,
               seqs = seqs2} = thms2

        val ths = ThmSet.union ths1 ths2
        and sym = Symbol.union sym1 sym2
        and ots = NameMap.union noDups ots1 ots2
        and cons = NameMap.union noDups cons1 cons2
        and seqs = SequentMap.union pickSnd seqs1 seqs2
      in
        Thms
          {thms = ths,
           symbol = sym,
           typeOps = ots,
           consts = cons,
           seqs = seqs}
      end;
end;

local
  fun uncurriedUnion (thms1,thms2) = union thms1 thms2;
in
  fun unionList thmsl =
      case thmsl of
        [] => empty
      | thms :: thmsl => List.foldl uncurriedUnion thms thmsl;
end;

end
