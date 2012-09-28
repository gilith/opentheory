(* ========================================================================= *)
(* READING OBJECTS FROM COMMANDS                                             *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectRead :> ObjectRead =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of parameters for reading objects from commands.                   *)
(* ------------------------------------------------------------------------- *)

type parameters =
     {import : ObjectThms.thms,
      interpretation : Interpretation.interpretation,
      savable : bool};

(* ------------------------------------------------------------------------- *)
(* A type of states for reading objects from commands.                       *)
(* ------------------------------------------------------------------------- *)

datatype state =
    State of
      {parameters : parameters,
       stack : ObjectStack.stack,
       dict : ObjectDict.dict,
       export : ObjectExport.export,
       inference : Inference.inference};

fun initial parameters =
    let
      val {savable,...} = parameters

      val stack = ObjectStack.empty
      and dict = ObjectDict.empty
      and export = ObjectExport.new {savable = savable}
      and inference = Inference.empty
    in
      State
        {parameters = parameters,
         stack = stack,
         dict = dict,
         export = export,
         inference = inference}
    end;

fun parameters (State {parameters = x, ...}) = x;

fun stack (State {stack = x, ...}) = x;

fun dict (State {dict = x, ...}) = x;

fun export (State {export = x, ...}) = x;

fun inference (State {inference = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Executing commands.                                                       *)
(* ------------------------------------------------------------------------- *)

fun execute cmd state =
    let
      val State {parameters,stack,dict,export,inference} = state

      val {import,interpretation,savable} = parameters

(*OpenTheoryTrace2
      val () = Print.trace Command.pp "ObjectRead.execute: cmd" cmd

      val () = Print.trace ObjectStack.pp "ObjectRead.execute: stack" stack
*)
      val inference = Inference.add inference cmd
    in
      case cmd of
      (* SPECIAL COMMANDS *)

      (* Numbers *)

        Command.Num i =>
        let
          val obj = Object.mkNum i

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Names *)

      | Command.Name n =>
        let
          val obj = Object.mkName n

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* REGULAR COMMANDS *)

      (* Lambda abstraction terms *)

      | Command.AbsTerm =>
        let
          val (stack,objV,objB) = ObjectStack.pop2 stack

          val obj = Object.mkAbsTerm {savable = savable} objV objB

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The abs primitive inference *)

      | Command.AbsThm =>
        let
          val (stack,objV,objT) = ObjectStack.pop2 stack

          val obj = Object.mkAbsThm {savable = savable} objV objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Function application terms *)

      | Command.AppTerm =>
        let
          val (stack,objF,objA) = ObjectStack.pop2 stack

          val obj = Object.mkAppTerm {savable = savable} objF objA

(*OpenTheoryTrace2
          val () = Print.trace Object.pp "ObjectRead.execute appTerm" obj
*)

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The app primitive inference *)

      | Command.AppThm =>
        let
          val (stack,objF,objA) = ObjectStack.pop2 stack

          val obj = Object.mkAppThm {savable = savable} objF objA

(*OpenTheoryTrace2
          val () = Print.trace Object.pp "ObjectRead.execute appThm" obj
*)

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The assume primitive inference *)

      | Command.Assume =>
        let
          val (stack,objT) = ObjectStack.pop stack

          val obj = Object.mkAssume {savable = savable} objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Axioms *)

      | Command.Axiom =>
        let
          val (stack,objH,objC) = ObjectStack.pop2 stack

          val seq = Object.destSequent (objH,objC)

          val obj =
              case ObjectThms.peekThm import seq of
                SOME x => x
              | NONE => Object.mkAxiom {savable = savable} objH objC seq

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The betaConv primitive inference *)

      | Command.BetaConv =>
        let
          val (stack,objT) = ObjectStack.pop stack

          val obj = Object.mkBetaConv {savable = savable} objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Cons lists *)

      | Command.Cons =>
        let
          val (stack,objH,objT) = ObjectStack.pop2 stack

          val obj = Object.mkCons {savable = savable} objH objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Constants *)

      | Command.Const =>
        let
          val (stack,objN) = ObjectStack.pop stack

          val n = Object.destName objN

          val n = Interpretation.interpretConst interpretation n

          val obj =
              case ObjectThms.peekConst import n of
                SOME x => x
              | NONE => Object.mkConst n

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Constant terms *)

      | Command.ConstTerm =>
        let
          val (stack,objC,objT) = ObjectStack.pop2 stack

          val obj = Object.mkConstTerm {savable = savable} objC objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The deductAntisym primitive inference *)

      | Command.DeductAntisym =>
        let
          val (stack,objA,objB) = ObjectStack.pop2 stack

          val obj = Object.mkDeductAntisym {savable = savable} objA objB

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Dictionary definitions *)

      | Command.Def =>
        let
          val (stack,objI) = ObjectStack.pop stack

          val objD = ObjectStack.peek stack

          val i = Object.destNum objI

          val dict = ObjectDict.define dict (i,objD)
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The defineConst principle of definition *)

      | Command.DefineConst =>
        let
          val (stack,objN,objT) = ObjectStack.pop2 stack

          val n = Object.destName objN

          val n = Interpretation.interpretConst interpretation n

          val (obj0,obj1) =
              Object.mkDefineConst {savable = savable} n objT

          val stack = ObjectStack.push2 stack obj0 obj1
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The defineTypeOp principle of definition *)

      | Command.DefineTypeOp =>
        let
          val (stack,objN,objA,objR,objV,objT) = ObjectStack.pop5 stack

          val n = Object.destName objN

          val n = Interpretation.interpretTypeOp interpretation n

          val a = Object.destName objA

          val a = Interpretation.interpretConst interpretation a

          val r = Object.destName objR

          val r = Interpretation.interpretConst interpretation r

          val (obj0,obj1,obj2,obj3,obj4) =
              Object.mkDefineTypeOp {savable = savable} n a r objV objT

          val stack = ObjectStack.push5 stack obj0 obj1 obj2 obj3 obj4
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The eqMp primitive inference *)

      | Command.EqMp =>
        let
          val (stack,objA,objB) = ObjectStack.pop2 stack

          val obj = Object.mkEqMp {savable = savable} objA objB

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Empty lists *)

      | Command.Nil =>
        let
          val obj = Object.mkNil

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Type operator types *)

      | Command.OpType =>
        let
          val (stack,objT,objL) = ObjectStack.pop2 stack

          val obj = Object.mkOpType {savable = savable} objT objL

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Popping the stack *)

      | Command.Pop =>
        let
          val (stack,_) = ObjectStack.pop stack
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Dictionary lookups *)

      | Command.Ref =>
        let
          val (stack,objI) = ObjectStack.pop stack

          val i = Object.destNum objI

          val obj = ObjectDict.refer dict i

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Dictionary removals *)

      | Command.Remove =>
        let
          val (stack,objI) = ObjectStack.pop stack

          val i = Object.destNum objI

          val (dict,obj) = ObjectDict.remove dict i

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The refl primitive inference *)

      | Command.Refl =>
        let
          val (stack,objT) = ObjectStack.pop stack

          val obj = Object.mkRefl {savable = savable} objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* The subst primitive inference *)

      | Command.Subst =>
        let
          val (stack,objS,objT) = ObjectStack.pop2 stack

          val obj = Object.mkSubst {savable = savable} objS objT

(*OpenTheoryTrace4
          val () = Print.trace Object.pp "subst objS" objS
*)

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Saving theorems *)

      | Command.Thm =>
        let
          val (stack,objT,objH,objC) = ObjectStack.pop3 stack

          val th = ObjectThm.Thm {proof = objT, hyp = objH, concl = objC}

          val export = ObjectExport.add export th
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Type operators *)

      | Command.TypeOp =>
        let
          val (stack,objN) = ObjectStack.pop stack

          val n = Object.destName objN

          val n = Interpretation.interpretTypeOp interpretation n

          val obj =
              case ObjectThms.peekTypeOp import n of
                SOME x => x
              | NONE => Object.mkTypeOp n

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Term variables *)

      | Command.Var =>
        let
          val (stack,objN,objT) = ObjectStack.pop2 stack

          val obj = Object.mkVar {savable = savable} objN objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Term variable terms *)

      | Command.VarTerm =>
        let
          val (stack,objV) = ObjectStack.pop stack

          val obj = Object.mkVarTerm {savable = savable} objV

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end

      (* Type variable types *)

      | Command.VarType =>
        let
          val (stack,objN) = ObjectStack.pop stack

          val obj = Object.mkVarType objN

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             export = export,
             inference = inference}
        end
    end
    handle Error err =>
      let
(*OpenTheoryDebug
        fun ppState (State {stack,...}) =
            Print.consistentBlock 4
              [Print.ppString "  stack =",
               Print.break,
               ObjectStack.pp stack]

        val err = Print.toString ppState state ^ "\n" ^ err
*)
        val err =
            "while executing " ^ Command.toString cmd ^ " command:\n" ^ err
      in
        raise Error err
      end;

local
  fun process (cmd,state) = execute cmd state;
in
  fun executeStream strm state = Stream.foldl process state strm;
end;

(* ------------------------------------------------------------------------- *)
(* Executing text files.                                                     *)
(* ------------------------------------------------------------------------- *)

local
  (* Comment lines *)

  fun isComment l =
      case List.find (not o Char.isSpace) l of
        NONE => true
      | SOME #"#" => true
      | _ => false;
in
  fun executeTextFile {filename} state =
      let
        (* Estimating parse error line numbers *)

        val lines = Stream.fromTextFile {filename = filename}

        val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
      in
        (let
           (* The character stream *)

           val chars = Stream.filter (not o isComment) chars

           val chars = Parse.everything Parse.any chars

           (* The command stream *)

           val commands = Parse.everything Command.spacedParser chars
         in
           executeStream commands state
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

(* ------------------------------------------------------------------------- *)
(* The exported theorems.                                                    *)
(* ------------------------------------------------------------------------- *)

fun thms state = ObjectThms.fromExport (export state);

end
