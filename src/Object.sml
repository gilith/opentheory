(* ========================================================================= *)
(* OPENTHEORY OBJECTS THAT TRACK THEIR PROVENANCE                            *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Object :> Object =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of OpenTheory objects that track their provenance.                 *)
(* ------------------------------------------------------------------------- *)

type id = int;

datatype object =
    Object of
      {id : id,
       object : object'}

and object' =
    Object' of
      {data : ObjectData.data,
       provenance : provenance}

and provenance =
    Default
  | Special of
      {command : Command.command,
       arguments : object list,
       definitions : object list,
       generated : ObjectData.data list,
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
               else Print.trace Print.ppInt "Object.newId.counter" count
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

fun isDefaultProvenance prov =
    case prov of
      Default => true
    | Special _ => false;

fun argumentsProvenance prov =
    case prov of
      Default => []
    | Special {arguments = x, ...} => x;

fun definitionsProvenance prov =
    case prov of
      Default => []
    | Special {definitions = x, ...} => x;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun data' (Object' {data = x, ...}) = x;

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

fun data obj = data' (dest obj);

fun equalData d obj = ObjectData.equal d (data obj);

fun provenance obj = provenance' (dest obj);

fun isDefault obj = isDefaultProvenance (provenance obj);

fun allDefault objs = List.all isDefault objs;

fun arguments obj = argumentsProvenance (provenance obj);

fun definitions obj = definitionsProvenance (provenance obj);

(* List objects *)

fun destList obj = ObjectData.destList (data obj);

fun isList obj = ObjectData.isList (data obj);

(* Num objects *)

fun destNum obj = ObjectData.destNum (data obj);

fun isNum obj = ObjectData.isNum (data obj);

(* Name objects *)

fun destName obj = ObjectData.destName (data obj);

fun isName obj = ObjectData.isName (data obj);

(* Name list objects *)

fun destNames obj = ObjectData.destNames (data obj);

fun isNames obj = ObjectData.isNames (data obj);

(* Type operator objects *)

fun destTypeOp obj = ObjectData.destTypeOp (data obj);

fun isTypeOp obj = ObjectData.isTypeOp (data obj);

fun equalTypeOp ot obj = ObjectData.equalTypeOp ot (data obj);

(* Type objects *)

fun destType obj = ObjectData.destType (data obj);

fun isType obj = ObjectData.isType (data obj);

(* Type list objects *)

fun destTypes obj = ObjectData.destTypes (data obj);

fun isTypes obj = ObjectData.isTypes (data obj);

(* Constant objects *)

fun destConst obj = ObjectData.destConst (data obj);

fun isConst obj = ObjectData.isConst (data obj);

fun equalConst c obj = ObjectData.equalConst c (data obj);

(* Term variable objects *)

fun destVar obj = ObjectData.destVar (data obj);

fun isVar obj = ObjectData.isVar (data obj);

(* Term objects *)

fun destTerm obj = ObjectData.destTerm (data obj);

fun isTerm obj = ObjectData.isTerm (data obj);

(* Sequent objects *)

fun destSequent (objH,objC) = ObjectData.destSequent (data objH, data objC);

fun isSequent (objH,objC) = ObjectData.isSequent (data objH, data objC);

(* Theorem objects *)

fun destThm obj = ObjectData.destThm (data obj);

fun isThm obj = ObjectData.isThm (data obj);

(* Substitution objects *)

fun destSubst obj = ObjectData.destSubst (data obj);

fun isSubst obj = ObjectData.isSubst (data obj);

(* ------------------------------------------------------------------------- *)
(* Constructing objects from commands.                                       *)
(* ------------------------------------------------------------------------- *)

fun mkProv' d prov =
    Object'
      {data = d,
       provenance = prov};

fun mkProv d prov = mk (mkProv' d prov);

fun mkDefault d = mkProv d Default;

fun mkSpecial d cmd args defs gen res =
    let
      val prov =
          Special
            {command = cmd,
             arguments = args,
             definitions = defs,
             generated = gen,
             result = res}
    in
      mkProv d prov
    end;

fun mkSimple d cmd args =
    let
      val defs = []
      and gen = [d]
      and res = 0
    in
      mkSpecial d cmd args defs gen res
    end;

(* Special commands *)

fun mkNum i = mkDefault (ObjectData.Num i);

fun mkName n = mkDefault (ObjectData.Name n);

(* Regular commands *)

fun mkAbsTerm {savable} objV objB =
    let
      val d =
          let
            val v = destVar objV
            and b = destTerm objB
          in
            ObjectData.Term (Term.mkAbs (v,b))
          end

      val args = [objV,objB]
    in
      if not savable orelse allDefault args then mkDefault d
      else mkSimple d Command.AbsTerm args
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkAbsTerm:\n" ^ err);
*)

fun mkAbsThm {savable} objV objT =
    let
      val d =
          let
            val v = destVar objV
            and th = destThm objT
          in
            ObjectData.Thm (Thm.abs v th)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkAbsThm" d
*)
    in
      if not savable then mkDefault d
      else mkSimple d Command.AbsThm [objV,objT]
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkAbsThm:\n" ^ err);
*)

fun mkAppTerm {savable} objF objA =
    let
      val d =
          let
            val f = destTerm objF
            and a = destTerm objA
          in
            ObjectData.Term (Term.mkApp (f,a))
          end

      val args = [objF,objA]
    in
      if not savable orelse allDefault args then mkDefault d
      else mkSimple d Command.AppTerm args
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkAppTerm:\n" ^ err);
*)

fun mkAppThm {savable} objF objA =
    let
      val d =
          let
            val f = destThm objF
            and a = destThm objA
          in
            ObjectData.Thm (Thm.app f a)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkAppThm" d
*)
    in
      if not savable then mkDefault d
      else mkSimple d Command.AppThm [objF,objA]
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkAppThm:\n" ^ err);
*)

fun mkAssume {savable} objT =
    let
      val d =
          let
            val t = destTerm objT
          in
            ObjectData.Thm (Thm.assume t)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkAssume" d
*)
    in
      if not savable then mkDefault d
      else mkSimple d Command.Assume [objT]
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkAssume:\n" ^ err);
*)

fun mkAxiom {savable} objH objC seq =
    let
      val d = ObjectData.Thm (Thm.axiom seq)

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkAxiom" d
*)

      val args = [objH,objC]
    in
      if not savable orelse allDefault args then mkDefault d
      else mkSimple d Command.Axiom args
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkAxiom:\n" ^ err);
*)

fun mkBetaConv {savable} objT =
    let
      val d =
          let
            val t = destTerm objT
          in
            ObjectData.Thm (Thm.betaConv t)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkBetaConv" d
*)
    in
      if not savable then mkDefault d
      else mkSimple d Command.BetaConv [objT]
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkBetaConv:\n" ^ err);
*)

fun mkCons {savable} objH objT =
    let
      val d =
          let
            val h = data objH
            and t = destList objT
          in
            ObjectData.List (h :: t)
          end

      val args = [objH,objT]
    in
      if not savable orelse allDefault args then mkDefault d
      else mkSimple d Command.Cons args
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkCons:\n" ^ err);
*)

fun mkConst n = mkDefault (ObjectData.Const (Const.mkUndef n));

fun mkConstTerm {savable} objC objT =
    let
      val d =
          let
            val c = destConst objC
            and ty = destType objT
          in
            ObjectData.Term (Term.mkConst (c,ty))
          end

      val args = [objC,objT]
    in
      if not savable orelse allDefault args then mkDefault d
      else mkSimple d Command.ConstTerm args
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkConstTerm:\n" ^ err);
*)

fun mkDeductAntisym {savable} objA objB =
    let
      val d =
          let
            val a = destThm objA
            and b = destThm objB
          in
            ObjectData.Thm (Thm.deductAntisym a b)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkDeductAntisym" d
*)
    in
      if not savable then mkDefault d
      else mkSimple d Command.DeductAntisym [objA,objB]
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkDeductAntisym:\n" ^ err);
*)

fun mkDefineConst {savable} n objT =
    let
      val (d0,d1) =
          let
            val t = destTerm objT

            val (c,th) = Thm.defineConst n t
          in
            (ObjectData.Const c, ObjectData.Thm th)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkDefineConst.def" d1
*)
    in
      if not savable then (mkDefault d0, mkDefault d1)
      else
        let
          val cmd = Command.DefineConst
          and args = [mkName n, objT]
          and gen = [d0,d1]

          val defs = []

          val obj0 = mkSpecial d0 cmd args defs gen 0

          val defs = obj0 :: defs

          val obj1 = mkSpecial d1 cmd args defs gen 1
        in
          (obj0,obj1)
        end
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkDefineConst:\n" ^ err);
*)

fun mkDefineTypeOp {savable} n a r objV objT =
    let
      val (d0,d1,d2,d3,d4) =
          let
            val v = destNames objV
            and t = destThm objT

            val (ot,{abs},{rep},ar,ra) =
                Thm.defineTypeOp n {abs = a} {rep = r} v t
          in
            (ObjectData.TypeOp ot,
             ObjectData.Const abs,
             ObjectData.Const rep,
             ObjectData.Thm ar,
             ObjectData.Thm ra)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkDefineTypeOp.absRep" d3

      val () = Print.trace ObjectData.pp "Object.mkDefineTypeOp.repAbs" d4
*)
    in
      if not savable then
        let
          val obj0 = mkDefault d0
          and obj1 = mkDefault d1
          and obj2 = mkDefault d2
          and obj3 = mkDefault d3
          and obj4 = mkDefault d4
        in
          (obj0,obj1,obj2,obj3,obj4)
        end
      else
        let
          val cmd = Command.DefineTypeOp
          and args = [mkName n, mkName a, mkName r, objV, objT]
          and gen = [d0,d1,d2,d3,d4]

          val defs = []

          val obj0 = mkSpecial d0 cmd args defs gen 0

          val defs = obj0 :: defs

          val obj1 = mkSpecial d1 cmd args defs gen 1
          and obj2 = mkSpecial d2 cmd args defs gen 2

          val defs = obj2 :: obj1 :: defs

          val obj3 = mkSpecial d3 cmd args defs gen 3
          and obj4 = mkSpecial d4 cmd args defs gen 4
        in
          (obj0,obj1,obj2,obj3,obj4)
        end
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkDefineTypeOp:\n" ^ err);
*)

fun mkDefineTypeOpLegacy {savable} n a r objV objT =
    let
      val (d0,d1,d2,d3,d4) =
          let
            val v = destNames objV
            and t = destThm objT

            val (ot,{abs},{rep},ar,ra) =
                Rule.defineTypeOpLegacy n {abs = a} {rep = r} v t
          in
            (ObjectData.TypeOp ot,
             ObjectData.Const abs,
             ObjectData.Const rep,
             ObjectData.Thm ar,
             ObjectData.Thm ra)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkDefineTypeOpLegacy.absRep" d3

      val () = Print.trace ObjectData.pp "Object.mkDefineTypeOpLegacy.repAbs" d4
*)
    in
      if not savable then
        let
          val obj0 = mkDefault d0
          and obj1 = mkDefault d1
          and obj2 = mkDefault d2
          and obj3 = mkDefault d3
          and obj4 = mkDefault d4
        in
          (obj0,obj1,obj2,obj3,obj4)
        end
      else
        let
          val cmd = Command.DefineTypeOpLegacy
          and args = [mkName n, mkName a, mkName r, objV, objT]
          and gen = [d0,d1,d2,d3,d4]

          val defs = []

          val obj0 = mkSpecial d0 cmd args defs gen 0

          val defs = obj0 :: defs

          val obj1 = mkSpecial d1 cmd args defs gen 1
          and obj2 = mkSpecial d2 cmd args defs gen 2

          val defs = obj2 :: obj1 :: defs

          val obj3 = mkSpecial d3 cmd args defs gen 3
          and obj4 = mkSpecial d4 cmd args defs gen 4
        in
          (obj0,obj1,obj2,obj3,obj4)
        end
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkDefineTypeOpLegacy:\n" ^ err);
*)

fun mkEqMp {savable} objA objB =
    let
      val d =
          let
            val a = destThm objA
            and b = destThm objB
          in
            ObjectData.Thm (Thm.eqMp a b)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkEqMp" d
*)
    in
      if not savable then mkDefault d
      else mkSimple d Command.EqMp [objA,objB]
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkEqMp:\n" ^ err);
*)

val mkNil = mkDefault (ObjectData.mkNil);

fun mkOpType {savable} objO objL =
    let
      val d =
          let
            val ot = destTypeOp objO
            and tys = destTypes objL
          in
            ObjectData.mkOpType (ot,tys)
          end

      val args = [objO,objL]
    in
      if not savable orelse allDefault args then mkDefault d
      else mkSimple d Command.OpType args
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkOpType:\n" ^ err);
*)

fun mkRefl {savable} objT =
    let
      val d =
          let
            val t = destTerm objT
          in
            ObjectData.Thm (Thm.refl t)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkRefl" d
*)
    in
      if not savable then mkDefault d
      else mkSimple d Command.Refl [objT]
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkRefl:\n" ^ err);
*)

fun mkSubst {savable} objS objT =
    let
      val d =
          let
            val (sty,stm) = destSubst objS
            and th = destThm objT

            val sub = TermSubst.mk (TypeSubst.mk sty) stm
          in
            ObjectData.Thm (Thm.subst sub th)
          end

(*OpenTheoryTrace2
      val () = Print.trace ObjectData.pp "Object.mkSubst" d
*)
    in
      if not savable then mkDefault d
      else mkSimple d Command.Subst [objS,objT]
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkSubst:\n" ^ err);
*)

fun mkTypeOp n = mkDefault (ObjectData.TypeOp (TypeOp.mkUndef n));

fun mkVar {savable} objN objT =
    let
      val d =
          let
            val n = destName objN
            and ty = destType objT

            val () =
                if Name.isGlobal n then ()
                else raise Error ("non-global name: " ^ Name.toString n)
          in
            ObjectData.Var (Var.mk (n,ty))
          end

      val args = [objN,objT]
    in
      if not savable orelse allDefault args then mkDefault d
      else mkSimple d Command.Var args
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkVar:\n" ^ err);
*)

fun mkVarTerm {savable} objV =
    let
      val d =
          let
            val v = destVar objV
          in
            ObjectData.Term (Term.mkVar v)
          end

      val args = [objV]
    in
      if not savable orelse allDefault args then mkDefault d
      else mkSimple d Command.VarTerm args
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkVarTerm:\n" ^ err);
*)

fun mkVarType objN =
    let
      val d =
          let
            val n = destName objN

            val () =
                if Name.isGlobal n then ()
                else raise Error ("non-global name: " ^ Name.toString n)
          in
            ObjectData.mkVarType n
          end
    in
      mkDefault d
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkVarType:\n" ^ err);
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
     | _ => raise Bug "Object.mkCommand")
(*OpenTheoryDebug
    handle Error err => raise Error ("in Object.mkCommand:\n" ^ err);
*)

(* Derived commands *)

fun mkList sav objs =
    case objs of
      [] => mkNil
    | obj :: objs => mkCons sav obj (mkList sav objs);

(* ------------------------------------------------------------------------- *)
(* Reconstructing the command and arguments used to make an object.          *)
(* ------------------------------------------------------------------------- *)

fun unMkCommand obj =
    case provenance obj of
      Default =>
      let
        val (cmd,args) = ObjectData.command (data obj)

        val args = List.map mkDefault args
      in
        (cmd,args)
      end
    | Special {command,arguments,...} =>
      (command,arguments);

fun unMkAbsTerm obj =
    case unMkCommand obj of
      (Command.AbsTerm,[objV,objB]) => (objV,objB)
    | _ => raise Error "Object.unMkAbsTerm";

fun unMkAppTerm obj =
    case unMkCommand obj of
      (Command.AppTerm,[objF,objA]) => (objF,objA)
    | _ => raise Error "Object.unMkAppTerm";

fun unMkAxiom obj =
    case unMkCommand obj of
      (Command.Axiom,[objH,objC]) => (objH,objC)
    | _ => raise Error "Object.unMkAxiom";

fun unMkVar obj =
    case unMkCommand obj of
      (Command.Var,[objN,objT]) => (objN,objT)
    | _ => raise Error "Object.unMkVar";

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap data ObjectData.pp;

fun ppProvenance prov =
    case prov of
      Default => Print.ppString "default"
    | Special {command,...} => Command.pp command;

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
                val unchanged = true

                val (obj1',acc) = result

                val (unchanged,obj1) =
                    case obj1' of
                      NONE => (unchanged,obj0)
                    | SOME obj => (false,obj)

                val (unchanged,obj2,acc) =
                    case provenance obj1 of
                      Default => (unchanged,obj1,acc)
                    | Special
                        {command = cmd,
                         arguments = args,
                         definitions = _,
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

(*OpenTheoryTrace6
                            val () = Print.trace Command.pp
                                      "Object.maps: re-running command" cmd

                            val () = Print.trace pp
                                      "Object.maps: command result" obj
*)
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
(* Constructing unsavable objects.                                           *)
(* ------------------------------------------------------------------------- *)

val mkUnsavable = mkDefault;

end

structure ObjectOrdered =
struct type t = Object.object val compare = Object.compare end

structure ObjectMap = KeyMap (ObjectOrdered)

structure ObjectSet =
struct

local
  structure S = ElementSet (ObjectMap);
in
  open S;
end;

local
  fun ancs set objs =
      case objs of
        [] => set
      | obj :: objs =>
        if member obj set then ancs set objs
        else ancs (add set obj) (Object.arguments obj @ objs);

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
