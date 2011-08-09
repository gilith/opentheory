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
      {thms : Thms.thms,
       typeOps : ObjectProv.object NameMap.map,
       consts : ObjectProv.object NameMap.map,
       seqs : ObjectProv.object SequentMap.map};

val empty =
    let
      val thms = Thms.empty
      and typeOps = NameMap.new ()
      and consts = NameMap.new ()
      and seqs = SequentMap.new ()
    in
      Thms
        {thms = thms,
         typeOps = typeOps,
         consts = consts,
         seqs = seqs}
    end;

fun thms (Thms {thms = x, ...}) = x;

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
  fun pickSnd (_,(_,x)) = SOME x;
in
  fun union thms1 thms2 =
      let
        val Thms
              {thms = ths1,
               typeOps = ots1,
               consts = cons1,
               seqs = seqs1} = thms1
        and Thms
              {thms = ths2,
               typeOps = ots2,
               consts = cons2,
               seqs = seqs2} = thms2

        val ths = Thms.union ths1 ths2
        and ots = NameMap.union pickSnd ots1 ots2
        and cons = NameMap.union pickSnd cons1 cons2
        and seqs = SequentMap.union pickSnd seqs1 seqs2
      in
        Thms
          {thms = ths,
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

(* ------------------------------------------------------------------------- *)
(* I/O.                                                                      *)
(* ------------------------------------------------------------------------- *)

fun toExport ths =
    let
      fun add (th,exp) =
          case peekThm ths (Thm.sequent th) of
            SOME obj => ObjectExport.insert exp (obj,th)
          | NONE => raise Bug "ObjectThms.toExport.add: vanishing sequent"

      val exp = ObjectExport.empty

      val exp = ThmSet.foldl add exp (Thms.thms (thms ths))
    in
      ObjectExport.compress exp
    end;

local
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
  fun fromExport exp =
      let
        fun split (obj,th,(objs,ths,seqs)) =
            let
              val objs = obj :: objs
              and ths = Thms.add ths th
              and seqs = SequentMap.insert seqs (Thm.sequent th, obj)
            in
              (objs,ths,seqs)
            end

        val objs = []
        and ths = Thms.empty
        and seqs = SequentMap.new ()

        val (objs,ths,seqs) =
            ObjectProvMap.foldr split (objs,ths,seqs) (ObjectExport.toMap exp)

        val sym = Thms.symbol ths

        val ots = SymbolTable.typeOps sym
        and cons = SymbolTable.consts sym

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

        val otO = NameMap.new ()
        and conO = NameMap.new ()

        val (otO,conO) = adds IntSet.empty (otO,conO) objs

        val otO = TypeOpSet.foldl fillTypeOp otO ots
        and conO = ConstSet.foldl fillConst conO cons
      in
        Thms
          {thms = ths,
           typeOps = otO,
           consts = conO,
           seqs = seqs}
      end
end;

end
