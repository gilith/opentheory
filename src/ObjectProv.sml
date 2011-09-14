(* ========================================================================= *)
(* OPENTHEORY OBJECTS THAT TRACK THEIR PROVENANCE                            *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectProv :> ObjectProv =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of objects that track their provenance.                            *)
(* ------------------------------------------------------------------------- *)

type id = int;

datatype object =
    Object of
      {id : id,
       object : object'}

and object' =
    Object' of
      {object : Object.object,
       provenance : provenance}

and provenance =
    Default
  | Special of
      {command : Command.command,
       arguments : object list,
       generated : Object.object list,
       result : int};

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

val newId : unit -> id =
    let
      val counter = ref 0
    in
      fn () =>
         let
           val ref count = counter

           val () = counter := count + 1

(*OpenTheoryTrace2
           val () =
               if count mod 1000 <> 0 then ()
               else Print.trace Print.ppInt "ObjectProv.newId.counter" count
*)
         in
           count
         end
    end;

fun id (Object {id = x, ...}) = x;

fun equalId i obj = i = id obj;

fun equal (Object {id = i1, ...}) (Object {id = i2, ...}) = i1 = i2;

fun compare (Object {id = i1, ...}, Object {id = i2, ...}) =
    Int.compare (i1,i2);

(* ------------------------------------------------------------------------- *)
(* A type of provenances.                                                    *)
(* ------------------------------------------------------------------------- *)

fun mkSpecialProvenance cmd args gen res =
    Special
      {command = cmd,
       arguments = args,
       generated = gen,
       result = res};

fun isDefaultProvenance prov =
    case prov of
      Default => true
    | Special _ => false;

fun argumentsProvenance prov =
    case prov of
      Default => []
    | Special {arguments,...} => arguments;

fun updateArgumentsProvenance args prov =
    case prov of
      Default => raise Error "ObjectProv.updateArgumentProvenance"
    | Special
        {command = cmd,
         arguments = _,
         generated = gen,
         result = res} =>
      Special
        {command = cmd,
         arguments = args,
         generated = gen,
         result = res};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun object' (Object' {object = x, ...}) = x;

fun provenance' (Object' {provenance = x, ...}) = x;

fun mk obj' =
    let
      val id = newId ()
    in
      Object
        {id = id,
         object = obj'}
    end;

fun dest (Object {id = _, object = x}) = x;

fun object obj = object' (dest obj);

fun provenance obj = provenance' (dest obj);

fun isDefault obj = isDefaultProvenance (provenance obj);

fun allDefault objs = List.all isDefault objs;

fun parents obj = argumentsProvenance (provenance obj);

fun updateProvenance f obj =
    let
      val Object' {object = ob, provenance = prov} = dest obj

      val prov = f prov
    in
      mk (Object' {object = ob, provenance = prov})
    end;

fun updateArguments args obj =
    updateProvenance (updateArgumentsProvenance args) obj;

(* ------------------------------------------------------------------------- *)
(* Constructing objects from commands.                                       *)
(* ------------------------------------------------------------------------- *)

fun mkSpecial cmd args ob = mkSpecialProvenance cmd args [ob] 0;

fun mkSpecial2 cmd args (ob0,ob1) =
    let
      val gen = [ob0,ob1]
    in
      (mkSpecialProvenance cmd args gen 0,
       mkSpecialProvenance cmd args gen 1)
    end;

fun mkSpecial5 cmd args (ob0,ob1,ob2,ob3,ob4) =
    let
      val gen = [ob0,ob1,ob2,ob3,ob4]
    in
      (mkSpecialProvenance cmd args gen 0,
       mkSpecialProvenance cmd args gen 1,
       mkSpecialProvenance cmd args gen 2,
       mkSpecialProvenance cmd args gen 3,
       mkSpecialProvenance cmd args gen 4)
    end;

fun mkProv ob prov = mk (Object' {object = ob, provenance = prov});

fun mkDefault ob = mkProv ob Default;

(* Special commands *)

fun mkNum i = mkDefault (Object.Num i);

fun mkName n = mkDefault (Object.Name n);

(* Regular commands *)

fun mkAbsTerm {savable} objV objB =
    let
      val obV = object objV
      and obB = object objB

      val ob =
          let
            val v = Object.destVar obV
            and b = Object.destTerm obB
          in
            Object.Term (Term.mkAbs (v,b))
          end

      val cmd = Command.AbsTerm
      and args = [objV,objB]
      and gen = ob

      val prov =
          if not savable orelse allDefault args then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAbsTerm: " ^ err);
*)

fun mkAbsThm {savable} objV objT =
    let
      val obV = object objV
      and obT = object objT

      val ob =
          let
            val v = Object.destVar obV
            and th = Object.destThm obT
          in
            Object.Thm (Thm.abs v th)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkAbsThm" ob
*)

      val cmd = Command.AbsThm
      and args = [objV,objT]
      and gen = ob

      val prov =
          if not savable then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAbsThm: " ^ err);
*)

fun mkAppTerm {savable} objF objA =
    let
      val obF = object objF
      and obA = object objA

      val ob =
          let
            val f = Object.destTerm obF
            and a = Object.destTerm obA
          in
            Object.Term (Term.mkApp (f,a))
          end

      val cmd = Command.AppTerm
      and args = [objF,objA]
      and gen = ob

      val prov =
          if not savable orelse allDefault args then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAppTerm: " ^ err);
*)

fun mkAppThm {savable} objF objA =
    let
      val obF = object objF
      and obA = object objA

      val ob =
          let
            val f = Object.destThm obF
            and a = Object.destThm obA
          in
            Object.Thm (Thm.app f a)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkAppThm" ob
*)

      val cmd = Command.AppThm
      and args = [objF,objA]
      and gen = ob

      val prov =
          if not savable then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAppThm: " ^ err);
*)

fun mkAssume {savable} objT =
    let
      val obT = object objT

      val ob =
          let
            val t = Object.destTerm obT
          in
            Object.Thm (Thm.assume t)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkAssume" ob
*)

      val cmd = Command.Assume
      and args = [objT]
      and gen = ob

      val prov =
          if not savable then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAssume: " ^ err);
*)

fun mkAxiom {savable} objH objC seq =
    let
      val ob = Object.Thm (Thm.axiom seq)

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkAxiom" ob
*)

      val cmd = Command.Axiom
      and args = [objH,objC]
      and gen = ob

      val prov =
          if not savable orelse allDefault args then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkAppTerm: " ^ err);
*)

fun mkBetaConv {savable} objT =
    let
      val obT = object objT

      val ob =
          let
            val t = Object.destTerm obT
          in
            Object.Thm (Thm.betaConv t)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkBetaConv" ob
*)

      val cmd = Command.BetaConv
      and args = [objT]
      and gen = ob

      val prov =
          if not savable then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkBetaConv: " ^ err);
*)

fun mkCons {savable} objH objT =
    let
      val obH = object objH
      and obT = object objT

      val ob =
          let
            val l = Object.destList obT
          in
            Object.List (obH :: l)
          end

      val cmd = Command.Cons
      and args = [objH,objT]
      and gen = ob

      val prov =
          if not savable orelse allDefault args then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkCons: " ^ err);
*)

fun mkConst c = mkDefault (Object.Const c);

fun mkConstTerm {savable} objC objT =
    let
      val obC = object objC
      and obT = object objT

      val ob =
          let
            val c = Object.destConst obC
            and ty = Object.destType obT
          in
            Object.Term (Term.mkConst (c,ty))
          end

      val cmd = Command.ConstTerm
      and args = [objC,objT]
      and gen = ob

      val prov =
          if not savable orelse allDefault args then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkConstTerm: " ^ err);
*)

fun mkDeductAntisym {savable} objA objB =
    let
      val obA = object objA
      and obB = object objB

      val ob =
          let
            val a = Object.destThm obA
            and b = Object.destThm obB
          in
            Object.Thm (Thm.deductAntisym a b)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkDeductAntisym" ob
*)

      val cmd = Command.DeductAntisym
      and args = [objA,objB]
      and gen = ob

      val prov =
          if not savable then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkDeductAntisym: " ^ err);
*)

fun mkDefineConst {savable} n objT =
    let
      val obT = object objT

      val obs as (ob0,ob1) =
          let
            val t = Object.destTerm obT

            val (c,th) = Thm.defineConst n t
          in
            (Object.Const c, Object.Thm th)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkDefineConst" ob1
*)

      val cmd = Command.DefineConst
      and args = [mkName n, objT]
      and gen = obs

      val (prov0,prov1) =
          if not savable then (Default,Default)
          else mkSpecial2 cmd args gen
    in
      (mkProv ob0 prov0, mkProv ob1 prov1)
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkDefineConst: " ^ err);
*)

fun mkDefineTypeOp {savable} n a r objV objT =
    let
      val obV = object objV
      and obT = object objT

      val obs as (ob0,ob1,ob2,ob3,ob4) =
          let
            val v = Object.destNames obV
            and t = Object.destThm obT

            val (ot,{abs},{rep},ar,ra) =
                Thm.defineTypeOp n {abs = a} {rep = r} v t
          in
            (Object.TypeOp ot,
             Object.Const abs,
             Object.Const rep,
             Object.Thm ar,
             Object.Thm ra)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkDefineTypeOp.absRep" ob3

      val () = Print.trace Object.pp "ObjectProv.mkDefineTypeOp.repAbs" ob4
*)

      val cmd = Command.DefineTypeOp
      and args = [mkName n, mkName a, mkName r, objV, objT]
      and gen = obs

      val (prov0,prov1,prov2,prov3,prov4) =
          if not savable then (Default,Default,Default,Default,Default)
          else mkSpecial5 cmd args gen
    in
      (mkProv ob0 prov0,
       mkProv ob1 prov1,
       mkProv ob2 prov2,
       mkProv ob3 prov3,
       mkProv ob4 prov4)
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkDefineTypeOp: " ^ err);
*)

fun mkEqMp {savable} objA objB =
    let
      val obA = object objA
      and obB = object objB

      val ob =
          let
            val a = Object.destThm obA
            and b = Object.destThm obB
          in
            Object.Thm (Thm.eqMp a b)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkEqMp" ob
*)

      val cmd = Command.EqMp
      and args = [objA,objB]
      and gen = ob

      val prov =
          if not savable then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkEqMp: " ^ err);
*)

val mkNil = mkDefault (Object.List []);

fun mkOpType {savable} objO objL =
    let
      val obO = object objO
      and obL = object objL

      val ob =
          let
            val ot = Object.destTypeOp obO
            and tys = Object.destTypes obL
          in
            Object.mkOpType (ot,tys)
          end

      val cmd = Command.OpType
      and args = [objO,objL]
      and gen = ob

      val prov =
          if not savable orelse allDefault args then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkOpType: " ^ err);
*)

fun mkRefl {savable} objT =
    let
      val obT = object objT

      val ob =
          let
            val t = Object.destTerm obT
          in
            Object.Thm (Thm.refl t)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkRefl" ob
*)

      val cmd = Command.Refl
      and args = [objT]
      and gen = ob

      val prov =
          if not savable then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkRefl: " ^ err);
*)

fun mkSubst {savable} objS objT =
    let
      val obS = object objS
      and obT = object objT

      val ob =
          let
            val s = Object.destSubst obS
            and th = Object.destThm obT
          in
            Object.Thm (Thm.subst (TermSubst.mk s) th)
          end

(*OpenTheoryTrace2
      val () = Print.trace Object.pp "ObjectProv.mkSubst" ob
*)

      val cmd = Command.Subst
      and args = [objS,objT]
      and gen = ob

      val prov =
          if not savable then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkSubst: " ^ err);
*)

fun mkTypeOp ot = mkDefault (Object.TypeOp ot);

fun mkVar {savable} objN objT =
    let
      val obN = object objN
      and obT = object objT

      val ob =
          let
            val n = Object.destName obN
            and ty = Object.destType obT

            val () =
                if Name.isGlobal n then ()
                else raise Error ("non-global name: " ^ Name.toString n)
          in
            Object.Var (Var.mk (n,ty))
          end

      val cmd = Command.Var
      and args = [objN,objT]
      and gen = ob

      val prov =
          if not savable orelse allDefault args then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkVar: " ^ err);
*)

fun mkVarTerm {savable} objV =
    let
      val obV = object objV

      val ob =
          let
            val v = Object.destVar obV
          in
            Object.Term (Term.mkVar v)
          end

      val cmd = Command.VarTerm
      and args = [objV]
      and gen = ob

      val prov =
          if not savable orelse allDefault args then Default
          else mkSpecial cmd args gen
    in
      mkProv ob prov
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkVarTerm: " ^ err);
*)

fun mkVarType objN =
    let
      val obN = object objN

      val ob =
          let
            val n = Object.destName obN

            val () =
                if Name.isGlobal n then ()
                else raise Error ("non-global name: " ^ Name.toString n)
          in
            Object.mkVarType n
          end
    in
      mkDefault ob
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("ObjectProv.mkVarType: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Folding state over objects.                                               *)
(* ------------------------------------------------------------------------- *)

fun foldl {preDescent,postDescent} =
    let
      fun foldlObj (obj,acc) =
          let
            val {descend, result = acc} = preDescent obj acc
          in
            if not descend then acc
            else
              let
                val acc =
                    case provenance obj of
                      Default => acc
                    | Special {arguments = args, ...} =>
                      List.foldl foldlObj acc args
              in
                postDescent obj acc
              end
          end
    in
      fn acc => fn obj => foldlObj (obj,acc)
    end;

(* ------------------------------------------------------------------------- *)
(* Mapping with state over objects.                                          *)
(* ------------------------------------------------------------------------- *)

fun maps {preDescent,postDescent} =
    let
      fun mapsObj obj acc =
          let
            val {descend,result} = preDescent obj acc
          in
            if not descend then result
            else
              let
                val (obj',acc) = result

                val (obj',acc) =
                    case provenance obj' of
                      Default => result
                    | Special {arguments = args, ...} =>
                      let
                        val (args',acc) = Useful.maps mapsObj args acc

                        val obj' =
                            if listEqual equal args' args then obj'
                            else updateArguments args' obj'
                      in
                        (obj',acc)
                      end
              in
                postDescent obj obj' acc
              end
          end
    in
      mapsObj
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap object Object.pp;

fun ppProvenance prov =
    case prov of
      Default => Print.ppString "Default"
    | Special {command,arguments,generated,result} =>
      Print.ppString "Special";

end

structure ObjectProvOrdered =
struct type t = ObjectProv.object val compare = ObjectProv.compare end

structure ObjectProvMap = KeyMap (ObjectProvOrdered)

structure ObjectProvSet =
struct

local
  structure S = ElementSet (ObjectProvMap);
in
  open S;
end;

local
  fun ancs set [] = set
    | ancs set (obj :: objs) =
      if member obj set then ancs set objs
      else ancs (add set obj) (ObjectProv.parents obj @ objs);

  fun addAncestors (obj,set) = ancs set [obj];
in
  fun ancestorsObject obj = addAncestors (obj,empty);

  val ancestors = foldl addAncestors empty;
end;

local
  fun toStrm NONE = Stream.Nil
    | toStrm (SOME iter) = Stream.Cons (readIterator iter, toDelay iter)

  and toDelay iter () = toStrm (advanceIterator iter)
in
  fun toStream s = toStrm (mkIterator s);
end;

val pp = Print.ppBracket "{" "}" (Print.ppMap size Print.ppInt);

end
