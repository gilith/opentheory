(* ========================================================================= *)
(* READING OBJECTS FROM COMMANDS                                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectRead :> ObjectRead =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Profiling inference functions.                                            *)
(* ------------------------------------------------------------------------- *)

(*OpenTheoryDebug
datatype inferenceCount = InferenceCount of int CommandMap.map;

val newInferenceCount = InferenceCount (CommandMap.new ());

fun incrementInferenceCount (InferenceCount m) n =
    let
      val i = Option.getOpt (NameMap.peek m n, 0)

      val m = NameMap.insert m (n, i + 1)
    in
      InferenceCount m
    end;

local
  val alignment : columnAlignment list =
      [{leftAlign = true, padChar = #"."},
       {leftAlign = false, padChar = #"."}];

  val countToString = Print.toString Print.ppPrettyInt;

  fun mkRow (s,i) = [s ^ " ...", " " ^ countToString i];

  fun mkInfRow (n,i) = mkRow (Name.toString n, i);

  fun mkTotalRow i = mkRow ("Total",i);
in
  fun ppInferenceCount (InferenceCount m) =
      let
        val infs = sortMap snd (revCompare Int.compare) (NameMap.toList m)

        val tot = foldl (fn ((_,i),k) => i + k) 0 infs

        val table = map mkInfRow infs @ [mkTotalRow tot]

        val rows = alignTable alignment table
      in
        case rows of
          [] => Print.skip
        | row :: rows =>
          Print.blockProgram Print.Consistent 0
            (Print.ppString row ::
             map (Print.sequence Print.addNewline o Print.ppString) rows)
      end;
end;

local
  val inferenceCount = ref (InferenceCount (NameMap.new ()));
in
  fun theInferenceCount () = !inferenceCount;

  fun incrementTheInferenceCount n =
      let
        val ref i = inferenceCount

        val () = inferenceCount := incrementInferenceCount i n
      in
        ()
      end
end;
*)

(* ------------------------------------------------------------------------- *)
(* A type of parameters for reading objects from commands.                   *)
(* ------------------------------------------------------------------------- *)

type parameters =
     {known : ObjectThms.thms,
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
       thms : Thm.thm ObjectProvMap.map};

fun initial parameters =
    let
      val stack = ObjectStack.empty

      val dict = ObjectDict.empty

      val thms = ObjectProvMap.new ()
    in
      State
        {parameters = parameters,
         stack = stack,
         dict = dict,
         thms = thms}
    end;

fun parameters (State {parameters = x, ...}) = x;

fun stack (State {stack = x, ...}) = x;

fun dict (State {dict = x, ...}) = x;

fun thms (State {thms = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Executing commands.                                                       *)
(* ------------------------------------------------------------------------- *)

fun execute cmd state =
    let
      val State {parameters,stack,dict,thms} = state

      val {known,interpretation,savable} = parameters

(*OpenTheoryDebug
      val () = incrementTheInferenceCount cmd
*)
    in
      case cmd of
      (* SPECIAL COMMANDS *)

      (* Numbers *)

        Command.Num i =>
        let
          val obj = ObjectProv.mkNum i

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Names *)

      | Command.Name n =>
        let
          val obj = ObjectProv.mkName n

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* REGULAR COMMANDS *)

      (* Lambda abstraction terms *)

      | Command.AbsTerm =>
        let
          val (stack,objV,objB) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkAbsTerm {savable = savable} objV objB

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Function application terms *)

      | Command.AppTerm =>
        let
          val (stack,objF,objA) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkAppTerm {savable = savable} objF objA

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Axioms *)

      | Command.Axiom =>
        let
          val (stack,objH,objC) = ObjectStack.pop2 stack

          val seq =
              let
                val obH = ObjectProv.object objH
                and obC = ObjectProv.object objC
              in
                Object.destSeq (obH,obC)
              end

          val obj =
              case ObjectThms.peekThm known seq of
                SOME x => x
              | NONE => ObjectProv.mkAxiom {savable = savable} objH objC seq

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

(***
      (* Function calls *)

      | Command.Call =>
        let
          val (stack,objA,objN) = ObjectStack.pop2 stack

          val obA = ObjectProv.object objA
          and obN = ObjectProv.object objN

          val _ = not (Object.isCall obA) orelse
                  raise Error "cannot use a Call object as a call argument"

          val n = Object.destName obN

(*OpenTheoryDebug
          val () = incrementTheInferenceCount n
*)

(*OpenTheoryTrace2
          val traceCall = null (ObjectStack.callStack stack)
(*OpenTheoryTrace3
          val traceCall = true
*)
          val () =
              if not traceCall then ()
              else
                trace
                  ("call: " ^ Name.toString n ^ "\n" ^ "  stack = [" ^
                   Int.toString (ObjectStack.size stack) ^ "], call stack = [" ^
                   Int.toString (length (ObjectStack.callStack stack)) ^ "]\n")

          val () = if not traceCall then ()
                   else Print.trace Object.pp "  input" obA
*)

          val (obA,objA,sims) =
              case Simulation.peek simulations n of
                NONE => (obA,objA,ThmSet.empty)
              | SOME (Simulation.Simulation sim) =>
                let
                  val ctxt =
                      Simulation.Context
                        {interpretation = interpretation,
                         input = obA}

                  val result =
                      sim ctxt
                      handle Error err =>
                        let
                          val ppOb = Print.ppOp2 " =" Print.ppString Object.pp
                        in
                          raise Error
                            ("simulation failed: " ^ Name.toString n ^
                             "\n" ^ Print.toString ppOb ("input",obA) ^
                             "\n" ^ err)
                        end

                  val Simulation.Result {input,thms} = result

                  val (obA,objA) =
                      case input of
                        NONE => (obA,objA)
                      | SOME obA =>
                        let
                          val objA = buildObject {savable = savable} objA obA
                        in
                          (obA,objA)
                        end
                in
                  (obA,objA,thms)
                end

          val obj = ObjectProv.mkCall n objA

          val stack = ObjectStack.push stack obj
          val stack = ObjectStack.push stack objA
          val stack = ObjectStack.addSimulation stack (sims,obj)
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end
***)

      (* Cons lists *)

      | Command.Cons =>
        let
          val (stack,objH,objT) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkCons {savable = savable} objH objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Constants *)

      | Command.Const =>
        let
          val (stack,objN) = ObjectStack.pop stack

          val n = Object.destName (ObjectProv.object objN)
          val n = Interpretation.interpretConst interpretation n

          val obj =
              case ObjectThms.peekConst known n of
                SOME x => x
              | NONE => ObjectProv.mkConst (Const.mkUndef n)

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Constant terms *)

      | Command.ConstTerm =>
        let
          val (stack,objC,objT) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkConstTerm {savable = savable} objC objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Dictionary definitions *)

      | Command.Def =>
        let
          val (stack,objI) = ObjectStack.pop stack

          val objD = ObjectStack.peek stack

          val i = Object.destNum (ObjectProv.object objI)

          val dict = ObjectDict.define dict (i,objD)
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Empty lists *)

      | Command.Nil =>
        let
          val obj = ObjectProv.mkNil

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Type operator types *)

      | Command.OpType =>
        let
          val (stack,objT,objL) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkOpType {savable = savable} objT objL

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
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
             thms = thms}
        end

      (* Dictionary lookups *)

      | Command.Ref =>
        let
          val (stack,objI) = ObjectStack.pop stack

          val i = Object.destNum (ObjectProv.object objI)

          val obj = ObjectDict.refer dict i

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Dictionary removals *)

      | Command.Remove =>
        let
          val (stack,objI) = ObjectStack.pop stack

          val i = Object.destNum (ObjectProv.object objI)

          val (dict,obj) = ObjectDict.remove dict i

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

(***
      (* Function call returns *)

      | Command.Return =>
        let
          val (stack,objR,objN) = ObjectStack.pop2 stack

          val obR = ObjectProv.object objR
          and obN = ObjectProv.object objN

          val _ = not (Object.isCall obR) orelse
                  raise Error "cannot return a Call object from a function"

          val n = Object.destName obN

          val (stack,n') = ObjectStack.popCall stack

          val _ = Name.equal n' n orelse
                  raise Error ("call " ^ Name.toString n' ^
                               " matched by return " ^ Name.toString n)
(*OpenTheoryTrace2
          val traceReturn = null (ObjectStack.callStack stack)
(*OpenTheoryTrace3
          val traceReturn = true
*)
          val () =
              if not traceReturn then ()
              else
                trace
                  ("return: " ^ Name.toString n ^ "\n" ^
                   "  stack = [" ^ Int.toString (ObjectStack.size stack) ^
                   "], call stack = [" ^
                   Int.toString (length (ObjectStack.callStack stack)) ^ "]\n")

          val () = if not traceReturn then ()
                   else Print.trace Object.pp "  return" obR
*)

          val obj = ObjectProv.mkReturn {savable = savable} objR

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end
***)

      (* Saving theorems *)

      | Command.Thm =>
        let
          val (stack,objT,objH,objC) = ObjectStack.pop3 stack

          val th =
              let
                val obT = ObjectProv.object objT
                and obH = ObjectProv.object objH
                and obC = ObjectProv.object objC

                val t = Object.destThm obT
                and seq = Object.destSeq (obH,obC)
              in
                Rule.alpha seq t
              end

          val thms = ObjectProvMap.insert thms (objT,th)
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Type operators *)

      | Command.TypeOp =>
        let
          val (stack,objN) = ObjectStack.pop stack

          val n = Object.destName (ObjectProv.object objN)
          val n = Interpretation.interpretTypeOp interpretation n

          val obj =
              case ObjectThms.peekTypeOp known n of
                SOME x => x
              | NONE => ObjectProv.mkTypeOp (TypeOp.mkUndef n)

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Term variables *)

      | Command.Var =>
        let
          val (stack,objN,objT) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkVar {savable = savable} objN objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Term variable terms *)

      | Command.VarTerm =>
        let
          val (stack,objV) = ObjectStack.pop stack

          val obj = ObjectProv.mkVarTerm {savable = savable} objV

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end

      (* Type variable types *)

      | Command.VarType =>
        let
          val (stack,objN) = ObjectStack.pop stack

          val obj = ObjectProv.mkVarType objN

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             thms = thms}
        end
    end
    handle Error err =>
      let
(*OpenTheoryDebug
        val State {stack,...} = state

        val ppStack =
            Print.ppMap
              (map ObjectProv.object o ObjectStack.objects)
              (Print.ppList Object.pp)

        val () = Print.trace ppStack "ObjectRead.execute: stack" stack
*)
        val err = "ObjectRead.execute " ^ Command.toString cmd ^ ": " ^ err
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

end
