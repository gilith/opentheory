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
       definitions : object list,
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

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun object' (Object' {object = x, ...}) = x;

fun definitions' (Object' {definitions = x, ...}) = x;

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

fun definitions obj = definitions' (dest obj);

fun provenance obj = provenance' (dest obj);

fun isDefault obj = isDefaultProvenance (provenance obj);

fun allDefault objs = List.all isDefault objs;

fun parents obj = definitions obj @ argumentsProvenance (provenance obj);

(* Num objects *)

fun destNum obj = Object.destNum (object obj);

(* Name objects *)

fun destName obj = Object.destName (object obj);

(* Type operator objects *)

fun destTypeOp obj = Object.destTypeOp (object obj);

fun isTypeOp obj = Object.isTypeOp (object obj);

fun equalTypeOp ot obj = Object.equalTypeOp ot (object obj);

(* Constant objects *)

fun destConst obj = Object.destConst (object obj);

fun isConst obj = Object.isConst (object obj);

fun equalConst c obj = Object.equalConst c (object obj);

(* Sequent objects *)

fun destSequent (objH,objC) = Object.destSequent (object objH, object objC);

(* Theorem objects *)

fun destThm obj = Object.destThm (object obj);

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

fun mkDefProv' ob defs prov =
    Object'
      {object = ob,
       definitions = defs,
       provenance = prov};

fun mkDefProv ob defs prov = mk (mkDefProv' ob defs prov);

fun mkProv ob prov = mkDefProv ob [] prov;

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
    handle Error err => raise Error ("in ObjectProv.mkAbsTerm:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkAbsThm:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkAppTerm:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkAppThm:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkAssume:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkAppTerm:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkBetaConv:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkCons:\n" ^ err);
*)

fun mkConst n = mkDefault (Object.Const (Const.mkUndef n));

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
    handle Error err => raise Error ("in ObjectProv.mkConstTerm:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkDeductAntisym:\n" ^ err);
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

      val obj0 = mkProv ob0 prov0

      val defs = [obj0]

      val obj1 = mkDefProv ob1 defs prov1
    in
      (obj0,obj1)
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in ObjectProv.mkDefineConst:\n" ^ err);
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

      val obj0 = mkProv ob0 prov0

      val defs = [obj0]

      val obj1 = mkDefProv ob1 defs prov1
      and obj2 = mkDefProv ob2 defs prov2

      val defs = obj2 :: obj1 :: defs

      val obj3 = mkDefProv ob3 defs prov3
      and obj4 = mkDefProv ob4 defs prov4
    in
      (obj0,obj1,obj2,obj3,obj4)
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in ObjectProv.mkDefineTypeOp:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkEqMp:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkOpType:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkRefl:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkSubst:\n" ^ err);
*)

fun mkTypeOp n = mkDefault (Object.TypeOp (TypeOp.mkUndef n));

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
    handle Error err => raise Error ("in ObjectProv.mkVar:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkVarTerm:\n" ^ err);
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
    handle Error err => raise Error ("in ObjectProv.mkVarType:\n" ^ err);
*)

(* General commands *)

fun mkCommand sav cmd args =
    (case (cmd,args) of
       (Command.Num i, []) => [mkNum i]
     | (Command.Name n, []) => [mkName n]
     | (Command.AbsTerm,[objV,objB]) => [mkAbsTerm sav objV objB]
     | (Command.AbsThm,[objV,objT]) => [mkAbsThm sav objV objT]
     | (Command.AppTerm,[objF,objA]) => [mkAppTerm sav objF objA]
     | (Command.AppThm,[objF,objA]) => [mkAppThm sav objF objA]
     | (Command.Assume,[objT]) => [mkAssume sav objT]
     | (Command.Axiom,[objH,objC]) =>
       let
         val seq = destSequent (objH,objC)
       in
         [mkAxiom sav objH objC seq]
       end
     | (Command.BetaConv,[objT]) => [mkBetaConv sav objT]
     | (Command.Cons,[objH,objT]) => [mkCons sav objH objT]
     | (Command.Const,[objN]) => [mkConst (destName objN)]
     | (Command.ConstTerm,[objC,objT]) => [mkConstTerm sav objC objT]
     | (Command.DeductAntisym,[objA,objB]) => [mkDeductAntisym sav objA objB]
     | (Command.DefineConst,[objN,objT]) =>
       let
         val n = destName objN

         val (obj0,obj1) = mkDefineConst sav n objT
       in
         [obj0,obj1]
       end
     | (Command.DefineTypeOp,[objN,objA,objR,objV,objT]) =>
       let
         val n = destName objN
         and a = destName objA
         and r = destName objR

         val (obj0,obj1,obj2,obj3,obj4) = mkDefineTypeOp sav n a r objV objT
       in
         [obj0,obj1,obj2,obj3,obj4]
       end
     | (Command.EqMp,[objA,objB]) => [mkEqMp sav objA objB]
     | (Command.Nil,[]) => [mkNil]
     | (Command.OpType,[objO,objL]) => [mkOpType sav objO objL]
     | (Command.Refl,[objT]) => [mkRefl sav objT]
     | (Command.Subst,[objS,objT]) => [mkSubst sav objS objT]
     | (Command.TypeOp,[objN]) => [mkTypeOp (destName objN)]
     | (Command.Var,[objN,objT]) => [mkVar sav objN objT]
     | (Command.VarTerm,[objV]) => [mkVarTerm sav objV]
     | (Command.VarType,[objN]) => [mkVarType objN]
     | _ => raise Bug "ObjectProv.mkCommand")
(*OpenTheoryDebug
    handle Error err => raise Error ("in ObjectProv.mkCommand:\n" ^ err);
*)

(* Commands for making specific type operators and constants *)

fun mkSpecificTypeOp {savable} ot =
    if TypeOp.isUndef ot then mkTypeOp (TypeOp.name ot)
    else
      let
        val () =
            if not savable then ()
            else raise Bug "ObjectProv.mkSpecificTypeOp"

        val ob = Object.TypeOp ot
      in
        mkDefault ob
      end;

fun mkSpecificConst {savable} c =
    if Const.isUndef c then mkConst (Const.name c)
    else
      let
        val () =
            if not savable then ()
            else raise Bug "ObjectProv.mkSpecificConst"

        val ob = Object.Const c
      in
        mkDefault ob
      end;

(* ------------------------------------------------------------------------- *)
(* Folding over objects.                                                     *)
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

fun maps {preDescent,postDescent,savable} =
    let
      fun mapsObj obj0 acc =
          let
            val {descend,result} = preDescent obj0 acc
          in
            if not descend then result
            else
              let
                val (obj1',acc) = result

                val (unchanged,obj1) =
                    case obj1' of
                      NONE => (true,obj0)
                    | SOME obj => (false,obj)

                val (unchanged,obj2,acc) =
                    case provenance obj1 of
                      Default => (unchanged,obj1,acc)
                    | Special
                        {command = cmd,
                         arguments = args,
                         generated = _,
                         result = idx} =>
                      let
                        val (args',acc) = mapsObjList args acc
                      in
                        case args' of
                          NONE => (unchanged,obj1,acc)
                        | SOME args =>
                          let
                            val objs = mkCommand {savable = savable} cmd args

                            val obj = List.nth (objs,idx)
                          in
                            (false,obj,acc)
                          end
                      end

                val obj2' = if unchanged then NONE else SOME obj2
              in
                postDescent obj0 obj2' acc
              end
          end

      and mapsObjUnchanged obj (unchanged,acc) =
          let
            val (obj',acc) = mapsObj obj acc
          in
            case obj' of
              NONE => (obj,(unchanged,acc))
            | SOME obj => (obj,(false,acc))
          end

      and mapsObjList objs acc =
          let
            val (objs,(unchanged,acc)) =
                Useful.maps mapsObjUnchanged objs (true,acc)

            val objs' = if unchanged then NONE else SOME objs
          in
            (objs',acc)
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
  fun ancs set objs =
      case objs of
        [] => set
      | obj :: objs =>
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
